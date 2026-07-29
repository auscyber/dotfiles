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
          # These two are bound by this wrapper because it re-emits them.
          # Terminal classes get the same `scoped` from the `policy.resolve`
          # context binding below.
          #
          # Everything below is lazy on purpose. Forcing `access` needs the key
          # set of `age.scoped.<scope>.secrets`, which needs this module's own
          # config keys -- so the injected args must not be forced while the
          # wrapper is building the attrset it returns.
          # Read the scope out of the `scoped` arg, not `config`: a nested-route
          # module's `config` is the subtree it was nested under, not the
          # top-level config, so `config.age` is not there. The route's
          # `adaptArgs` (../security/agenix-rekey.nix) injects `scoped` from the
          # real config, and lib/age-scoped.nix sets the same value as a global
          # `_module.args`, so this arg is right either way.
          scopeLocal =
            args:
            let
              s = args.scoped.${scope};
              view = s // {
                secrets = builtins.intersectAttrs s.secrets s.access;
                templates = builtins.intersectAttrs s.templates s.access;
              };
            in
            {
              scoped = view;
              inherit (view) secrets templates;
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
