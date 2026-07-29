{ lib, ... }:
# Automatic per-aspect scoping for the `secrets` / `templates` den classes.
#
# `den.schema.aspect` is imported into *every* aspect submodule (den
# nix/lib/aspects/types.nix), where `config.name` is the aspect name. This moves
# each aspect's `secrets`/`templates` class content under `scoped.<aspect>`,
# which the `scoped` class routes to `age.scoped` -- so scoping is automatic
# rather than something each aspect opts into.
#
# The `age.scoped` option itself lives in lib/age-scoped.nix and is imported into
# the nixos / darwin / homeManager sides of `den.aspects.agenix-rekey`
# (../security/agenix-rekey.nix), which is in `den.default.includes`.
let
  # Aspect keys nested under the aspect's own scope. They stay registered as den
  # classes (key classification needs them) but no longer route anywhere by
  # themselves -- this module is their only consumer.
  scopedKeys = [
    "secrets"
    "templates"
  ];

  # ---------------------------------------------------------------------------
  # Why terminal class bodies (nixos / darwin / homeManager) must name the scope
  # -- `scoped.<aspect>.secrets.<key>` -- while `secrets`/`templates` bodies do
  # not. This WAS implemented and then removed; do not re-attempt it as written.
  #
  # The hook is real: wrap-classes.nix `applyPipeTargeting` does
  #     ctx // (ctx.__pipeTargeted.<declaringAspectIdentity> or { })
  # at class-wrap time, keyed by the DECLARING aspect -- the one place den knows
  # it. `pipe.to` fills that map with lists, but the override is shape-agnostic,
  # so a `policy.resolve { __pipeTargeted = ...; }` in `den.default.includes`
  # delivers a per-aspect SCALAR. That evaluated correctly for nixos.
  #
  # It is nevertheless wrong, because a context binding is per SCOPE and a scope
  # emits modules for more than one config. `captureFleet` shows the scope
  # `aspect=<h>,host=<h>,system=<s>,user=<u>` carries both `homeManager` content
  # and `os` content (aspects/security/user-pwd.nix writes
  # `users.users.<u>.hashedPasswordFile` from a user scope). One binding cannot be
  # right for both, and `class` is NOT in the policy context -- the keys are
  # __entityKind, anyUser, aspect, home, host, self, system, user -- so there is
  # nothing to discriminate on. Binding the host config made home-manager bodies
  # silently resolve `scoped.secrets.<k>` against the HOST's scope, and standalone
  # homes (`ivy@contabo`) got no binding at all and failed outright.
  #
  # A wrong secret that still evaluates is far worse than a longer spelling, so
  # the scope name stays explicit. `__entityKind` does not rescue it: the
  # os-from-user-scope case is ambiguous by construction.
  # ---------------------------------------------------------------------------

in
{
  den.classes.scoped = { };

  den.schema.aspect =
    {
      config,
      options,
      ...
    }:
    {
      options = {
        secretScope = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = ''
            Which `age.scoped.<key>` entry this aspect's secrets and templates
            join. Defaults to the aspect name; set it to make several aspects
            share one scope, which also makes them share `access` and the
            implicit template dependencies.

            This is the attribute key, so it is what `scoped.<key>.access.<name>`
            reads. To change only the string prefixed onto the flat
            `age.secrets` names, set `secretSettings.name` instead.
          '';
        };
        secretSettings = lib.mkOption {
          type = lib.types.raw;
          default = { };
          description = ''
            Config for this aspect's scope, merged into `age.scoped.<scope>`.
            Anything the scope submodule declares goes here:

              secretSettings = {
                service   = "nginx";              # infer owner/group/restartUnits
                separator = "-";                  # flat names instead of a directory
                settings  = { owner = "acme"; };  # defaults for every entry
              };

            Note the nesting: the scope's own options sit at the top, and
            `settings` is the per-secret/per-template default block inside it.
          '';
        };
      };

      # This module's config KEYS must not depend on the aspect's own fixpoint.
      # `secrets`/`templates`/`scoped` are freeform keys, and the set of freeform
      # keys is the union of every module's config attribute names -- so a guard
      # like `lib.optionalAttrs (config ? secrets)` would need the key set in
      # order to produce the key set. Hence `scoped` is always emitted here and
      # the emptiness test lives inside the VALUE, where reading `config.secrets`
      # is just an ordinary lazy lookup.
      #
      # The key is therefore present on every aspect, so it must always carry a
      # value: `scoped` is a registered class, and den's class collector reads
      # `merged.scoped` for every aspect that has the key. Discharging to zero
      # definitions (`mkMerge []`) makes that read fail with "was accessed but
      # has no value defined", so aspects without secrets emit an empty module
      # instead -- it routes an empty `age.scoped` contribution and is inert.
      #
      # (`options ? includes` is safe: `options` is the option tree, not the
      # fixpoint. den also introspects this schema standalone, where `includes`
      # is not declared -- `lib.optionalAttrs` omits the definition entirely
      # there, where `lib.mkIf` would still register one and fault the
      # introspection. Same reasoning as ../framework/roles.nix.)
      config = lib.optionalAttrs (options ? includes) (
        let
          scope = if config.secretScope != null then config.secretScope else config.name;

          # Freeform aspect keys are wrapped by den's aspectContentType, which
          # keeps the raw per-file definitions in `__contentValues`. Re-emit one
          # `scoped` definition per original definition rather than merging them:
          # the module system expands `mkMerge` into separate defs before
          # aspectContentType.merge runs, so each survives as its own class
          # module. That preserves den's "unsatisfied entity arg => skip this
          # module" behaviour, which a single merged function would destroy for
          # an aspect that has both a `{ host, ... }` and a `{ user, ... }`
          # definition.
          # Inside this aspect's own `secrets`/`templates` bodies, `scoped` IS
          # this aspect's scope -- so `scoped.secrets.celler_token`, with
          # `secrets` and `templates` as aliases for `scoped.secrets` and
          # `scoped.templates`. Both are keyed by the SHORT name and hold the
          # deployed entries, so nothing repeats the scope prefix.
          #
          # Narrow the tree to THIS aspect's scope. The `scoped` arg is already
          # the viewed tree (lib/age-scoped.nix), so `s.secrets` / `s.templates`
          # are the evaluated `age.secrets` / `age.templates` entries under their
          # short names -- nothing to re-derive here, just pick the scope out and
          # alias its two halves.
          #
          # Read it from the `scoped` arg, never `config`: a nested-route module's
          # `config` is the subtree it was nested under, not the top-level config,
          # so `config.age` is not there.
          #
          # Stays lazy on purpose. Forcing these needs the key set of
          # `age.scoped.<scope>.secrets`, which needs this module's own config
          # keys -- so they must not be forced while the wrapper is building the
          # attrset it returns.
          scopeLocal =
            args:
            let
              raw = args.scoped.${scope};
              # Re-view unconditionally. `scoped` can arrive here already viewed
              # (lib/age-scoped.nix `_module.args`, or the per-aspect
              # `__pipeTargeted` override) or raw, depending on which supplier
              # wins for this routed module -- and a raw scope's `secrets` holds
              # DECLARATIONS, so a template dependency taken from it is not a real
              # agenix secret. agenix-rekey then reads `secret.rekeyFile` off it
              # (apps/rekey.nix pulls every template dependency into the rekey
              # set) and fails with "attribute 'rekeyFile' missing" -- but only
              # for entries that do not literally declare `rekeyFile`, i.e.
              # generated ones, so it hides until rekey runs.
              #
              # Idempotent: `intersectAttrs` takes names from the first argument
              # and VALUES from `access`, so viewing an already-viewed scope is a
              # no-op.
              s = raw // {
                secrets = builtins.intersectAttrs raw.secrets raw.access;
                templates = builtins.intersectAttrs raw.templates raw.access;
              };
            in
            {
              scoped = s;
              inherit (s) secrets templates;
            };

          # A class definition comes in two shapes, and both may be functions:
          #
          #   secrets       = { host, ... }: { foo = ...; };   whole-class
          #   templates.env = { secrets, ... }: { ... };       per-ENTRY
          #
          # Only the whole-class shape is handled here. Per-entry functions are
          # modules, and `age.scoped.<scope>.{secrets,templates}` entries are
          # freeform submodules (lib/age-scoped.nix) -- so those stay unapplied
          # and get their scope-local args from the entry submodule's own
          # `_module.args`. Applying them here would also wrongly reach into
          # `content` / `generator.script`, which are agenix's own callbacks.
          nest =
            key: value:
            if lib.isFunction value then
              # Keep this definition's own arg signature so den's class-module
              # wrapper (which reads builtins.functionArgs) still pre-applies
              # `host` / `user` exactly as it did before nesting. `scoped` is
              # added because the rebinding above needs it.
              lib.setFunctionArgs (args: {
                ${scope}.${key} = value (args // scopeLocal args);
              }) (builtins.functionArgs value // { scoped = true; })
            else
              { ${scope}.${key} = value; };

          defsFor = key: map (def: nest key def.value) ((config.${key} or null).__contentValues or [ ]);

          contentDefs = lib.concatMap defsFor scopedKeys;
        in
        {
          scoped = lib.mkMerge (
            [ { } ]
            ++ lib.optionals (contentDefs != [ ]) (contentDefs ++ [ { ${scope} = config.secretSettings; } ])
          );

          # NOTE (tried and rejected): binding `scoped` per aspect for TERMINAL
          # classes via `den.lib.policy.resolve` in this aspect's `includes`.
          #
          # Two things have to hold, and only the first does:
          #
          #  1. The binding must not force the flake fixpoint. den derives an
          #     injective scope identity from every context value by forcing it
          #     to WHNF and reading `.name` (den fx/pipeline.nix mkScopeId), and
          #     a context value is a constant computed before any target config
          #     exists -- so the view has to come back through
          #     `flake.<host>.config` and be an attrset LITERAL carrying a plain
          #     `name`, never `s // { ... }` (which forces `s`). Done that way it
          #     evaluates: no recursion.
          #
          #  2. The binding must reach the aspect's OWN class bodies. It does
          #     not. `resolve` widens the scope for deferred children (den
          #     fx/handlers/scope-widen.nix drains and re-resolves them under the
          #     new ctx), while the aspect's own class content was already
          #     emitted in the parent scope -- so an aspect's `nixos`/`homeManager`
          #     body sees an ANCESTOR's binding, silently reading the wrong
          #     scope's secrets. `resolve.withIncludes` puts content inside the
          #     new scope, but the aspect's own content cannot be moved there
          #     without emitting it twice.
          #
          #  3. `aspect-chain` is not a way around it either, and neither are
          #     quirks. `aspect-chain` is the one per-aspect handle den maintains
          #     (fx/handlers/chain.nix pushes and pops it as the walk descends,
          #     and aspects/base/home.nix reads `lib.head aspect-chain` to forward
          #     per aspect) -- but it reaches PARAMETRIC INCLUDES only. A class
          #     module asking for it falls through to `_module.args.aspect-chain`
          #     and errors: den's class-module wrapper pre-applies entity args
          #     only. Quirks fail for the same underlying reason -- values are
          #     collected per scope and delivered as a LIST, and though
          #     `pipe.withProvenance` tags each entry with its origin, the consumer
          #     has no handle on which aspect it itself is, so it cannot pick its
          #     own entry out.
          #
          # So terminal bodies keep the whole tree: `scoped.<aspect>.secrets.<key>`.
          # The way to shorten a given site is to move its content into a class
          # that IS re-emitted here -- a raw `age.templates.x` in a `nixos` body
          # becomes `templates.x` -- which works unless the entry needs the
          # enclosing config (inside a submodule, `config` is the entry's own).
          #
          # Scoped alias classes (`hmScoped`/`osScoped` routing into
          # homeManager/os, the way aspects/base/home.nix aliases `hm`) would
          # also work, since anything re-emitted here owns its arg set. Declined
          # deliberately -- not worth another class name to remember.
        }
      );
    };
}
