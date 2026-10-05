{
  inputs,
  lib,
  den,
  config,
  ...
}:
{
  systems = [
    "x86_64-linux"
    "aarch64-darwin"
  ];
  imports = [
    inputs.flake-file.flakeModules.default
    (lib.mkAliasOptionModule [ "ff" ] [ "flake-file" "inputs" ])
  ];
  den = {
    # A registered class, so aspects can write `den.aspects.foo.inputs.bla.url`
    # and den collects it per scope. Deliberately has NO policy routing it into
    # `flake-mod`: a class only reaches the route tree if something routes it,
    # and routing this one would land every declaration in the root lock.
    classes.inputs = { };

    schema.flake-mod = { };
    # Layer-aware. The policy function receives the entity context (the same
    # shape ./roles.nix reads `host`/`user` from), so the ROUTE PATH can be
    # chosen per host: a host in the `darwin` layer sends its inputs to
    # `flake-file-layers.darwin` and they never reach `flake-file.inputs` at
    # all. Not added, rather than added and subtracted.
    #
    # A guard cannot do this -- a route guard receives flake-parts module args
    # (_class, _prefix, config, inputs, lib, options, specialArgs) with no
    # entity in them. Verified by probe, not assumed.
    # Two registrations, two jobs. Both used to be this one policy with
    # `collectSubtree = true`, and that is why layering could not work: the
    # flake-mod registration has no `host` in scope, so it took the null-layer
    # branch AND swept the entire subtree into `flake-file` before any
    # host-scope route could place anything in a layer.
    #
    # `collectFromSubtree` roots at the route's own sourceScopeId (den
    # nix/lib/aspects/fx/edges/route.nix), so scoping the flake-mod route to
    # itself leaves host aspects to the host-scope route below, which knows
    # which layer they belong to.
    policies.inputs-to-flake-parts = _: [
      (den.lib.policy.route {
        fromClass = "flake-file";
        intoClass = "flake-mod";
        # Own scope only: flake-level aspects, which belong at the root.
        collectSubtree = false;
        path = [
          "flake-file"
        ];
        adaptArgs = lib.id;
      })
    ];

    # Host scope: `host` IS bound here, so the route path is chosen per layer
    # and a darwin host's inputs are never added to `flake-file.inputs` at all.
    policies.host-inputs-to-layers =
      {
        host ? { },
        ...
      }:
      let
        hostLayers = host.layers or [ ];
        layer =
          if hostLayers == [ ] then null else lib.concatStringsSep "-" (lib.sort (a: b: a < b) hostLayers);
      in
      [
        (den.lib.policy.route {
          fromClass = "flake-file";
          intoClass = "flake-mod";
          collectSubtree = true;
          path =
            if layer == null then
              [ "flake-file" ]
            else
              [
                "flake-file-layers"
                layer
              ];
          adaptArgs = lib.id;
        })
      ];

    # NOTE: `den.aspects.foo.inputs.bla.url = "<flakeref>"` is the declaration
    # surface, and it deliberately has NO route into `flake-mod`/`flake-file`.
    #
    # `flake-file.inputs` is what `write-flake` serializes into the ROOT
    # flake.nix, so anything routed there lands in the root lock by definition --
    # which is the opposite of splitting locks per layer. An `inputs`-class
    # declaration is read by the GENERATOR instead, which decides from the
    # declaring scopes which `partitions/<layer>/flake.nix` it belongs in.
    #
    # At evaluation time nothing reads `flake-file.inputs` either: inputs come
    # from the locks, reassembled by ../../lib/inputs.nix into one flat
    # attrset. `flake-file.inputs` is purely the generator's output target for
    # the root's own inputs.

    # NOTE: `den.aspects.foo.inputs.bla.url` is NOT routed anywhere.
    #
    # Every route has to name an `intoClass`, and routing into `flake-mod` is
    # wrong twice over: flake-mod is a singleton resolved with empty context
    # (../framework/flakeExtra.nix), so it loses which host an input came from;
    # and its `flake-file.inputs` is what `write-flake` writes into the ROOT
    # flake.nix. Inputs are read straight off the aspects instead -- see
    # ./layers.nix.

    policies.sources-to-flake-parts = _: [
      (den.lib.policy.route {
        fromClass = "nvfetcher";
        intoClass = "flake-mod";
        collectSubtree = true;
        path = [
        ];
        adaptArgs = lib.id;
      })
    ];
    policies.flake-parts-to-host =
      _:
      map (host: den.lib.policy.resolve.to "host" { inherit host; }) (
        builtins.concatMap builtins.attrValues (builtins.attrValues den.hosts)
      );
    schema.flake-mod.includes = [
      den.policies.inputs-to-flake-parts
      den.policies.sources-to-flake-parts
      den.policies.flake-parts-to-host
    ];

    schema.host.include = [ den.policies.host-inputs-to-layers ];
  };
  flake.den = den;

  #  disabledModules = [ (inputs.flake-file + "/modules/flake-parts.nix") ];

  #perSystem =
  #  { pkgs, ... }:
  #  {
  #    apps =
  #      config.flake-file.apps
  #      |> lib.mapAttrs (
  #        _: f:
  #        let
  #          pkg = f pkgs;
  #        in
  #        {
  #          type = "app";
  #          program = lib.getExe pkg;
  #        }
  #      );

  #    checks.check-flake-file = config.flake-file.check-flake-file pkgs;
  #  };

  flake-file = {
    #  prune-lock.enable = true;
    inputs.flake-file.url = "github:denful/flake-file";
    inputs.rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # The `gen` family, under github:sini rather than github:denful. Locked and
    # available; NOTHING IN THIS REPO CONSUMES THEM YET.
    #
    # The one-line descriptions on denful.dev/ecosystem/gen are MARKETING, not
    # API, and several are stale against the source as of 2026-10-05. Verified
    # corrections, so nobody re-reads the website and believes it:
    #
    #   * gen-graph's label-resolution surface (`query`, `regex`, `labeledFrom`,
    #     `boundedBy`) is RETIRED TOMBSTONES that throw and point at gen-scope,
    #     and its own source withdraws the Arntzenius 2016 attribution for the
    #     reverse queries ("this library's own; the attribution that stood here
    #     is withdrawn"). Only `fixpoint`'s Lemma 4 survives. So
    #     multi-hop-with-label-precedence is a gen-SCOPE adoption, not gen-graph.
    #   * gen-derive's own README is titled gen-DISPATCH, and states the
    #     convergence loop "belongs to gen-resolve". "with fixpoint convergence"
    #     is wrong.
    #   * gen-select has NO edge attributes. Its context is five NODE accessors
    #     (`data`/`parent`/`children`/`ancestors`/`siblings`) -- there is no edge
    #     in the vocabulary to hang an attribute on. "Attributed graph positions"
    #     means NODE positions carrying data.
    #   * gen-algebra exports no search monad and no validators; that tier
    #     "relocated to gen-schema". It exports `either`, `record`, `term` and an
    #     intensional-identity family.
    #   * gen-link is cross-flake aspect FEDERATION (Backpack-style imports and
    #     signatures), not links-as-edges. gen-class is byte-identical class
    #     SHARE dedup, not den's `den.classes.<n>`. gen-memo is reuse ACROSS
    #     evaluations needing hashable values -- a value carrying a function is
    #     "always-dirty", and service instances carry modules.
    #
    # And den itself uses almost none of this: its flake is
    # `{ outputs = _: import ./nix; }` with NO inputs, and its only gen
    # consumption is `nix/lib/schema.nix` taking `roster.schema` (gen-schema's
    # `mkInstanceType`). The gen names in den's templates/ci/flake.lock are a
    # lockfile closure, never evaluated. So adopting the rest here would put
    # this repo ahead of its own framework.
    #
    # Worth knowing for later: `nix/lib/schema.nix:33` is
    # `if inputs ? gen then inputs.gen.lib.mkGenLibs { } else import genSrc { }`
    # -- den ACCEPTS a `gen` input if one is passed, which is the supported
    # integration point.
    # ONE input: the gen HUB, not the members individually.
    #
    # den's `nix/lib/schema.nix` is explicit that this is not a preference:
    # "the hub binds every sibling's `gen-*` input with `follows`, so one input
    # yields one revision per library. Pinning gen-schema directly leaves den
    # holding its transitive closure -- measured at the bump that motivated this
    # file: three revisions of gen-prelude and two of gen-schema in one lock,
    # which reads as one value while being several builds."
    #
    # Measured here too. Eleven individual inputs gave 18 gen-prelude nodes, 7
    # gen-graph, 7 gen-identity and 3 gen-scope; adding top-level `follows` only
    # got them to 14/6/6/3, because it cannot reach the deeper transitive copies.
    # gen-scope's own flake says why that matters: "Two instances of gen-scope in
    # one evaluation are two identity formulas for the same node -- a failure
    # measured in a shipped consumer, not a hazard imagined here."
    #
    # It is also the supported integration with den, which does
    # `roster = if inputs ? gen then inputs.gen.lib.mkGenLibs { } else <CI-lock
    # fallback>` -- so declaring it here makes den mint identities with OUR
    # pinned revision instead of the one frozen in its templates/ci/flake.lock.
    #
    # Members are read as `(inputs.gen.lib.mkGenLibs { }).<member>` -- `scope`,
    # `graph`, `identity`, `schema`, and so on.
    # NOT `inputs.gen`, and this was measured rather than chosen.
    #
    # den does `roster = if inputs ? gen then inputs.gen.lib.mkGenLibs { } else
    # <its CI-lock fallback>`, so DECLARING a `gen` input hands den our hub and
    # it mints every entity identity through it. Its comment claims "two entry
    # shapes for one roster", but they are not interchangeable: through
    # `mkGenLibs` den's own wrapper
    # (`mkInstanceType = kindValue: withLib (base.mkInstanceType kindValue)`)
    # fails immediately with
    #
    #   error: gen-schema.mkInstanceType: '__functor' is not an option of this
    #   door; the options are closed (accepted: 'extraModules', 'strict',
    #   'specialArgs')
    #
    # and it fails identically at the hub revision den's OWN templates/ci/flake.lock
    # pins (5f58e7bf), so it is not revision skew -- the `inputs ? gen` arm is
    # broken against the pinned den. Fixing that is a den change, not ours.
    #
    # So den keeps its working fallback and we take only what our own code uses.
    # The duplication that costs is duplication inside ONE evaluator's identity
    # formulas; den's identities all come from its single fallback roster, and
    # these copies are read only by ../base/scope-probe.nix.
    inputs.gen-scope.url = "github:sini/gen-scope";
    # For `record.foldLayers` -- a DECLARED per-field merge over ordered
    # layers, with `"semilattice-set"` being the ACI join ("invariant under
    # layer reordering and under duplicate contributions"). That is what
    # merging capabilities across an inheritance chain needs, rather than
    # hand-rolled `listToAttrs`/`concatLists`.
    inputs.gen-algebra.url = "github:sini/gen-algebra";
    # For `implement`'s bindings. A den aspect receives `{ host, user, service }`
    # because den binds them; an `implement` body needs `{ http, cache, fiber }`
    # -- the resolved edges by their own want names, the ancestors by class, and
    # the fiber -- and nothing in den binds those.
    #
    # gen-bind is exactly that and not a near-miss: "it takes a module plus a set
    # of named bindings and partially applies the bindings the module actually
    # declares". So a body that asks for `{ http, ... }` gets the link and
    # nothing else, by introspection rather than by being handed a fixed arg set.
    #
    # Three parts of it are load-bearing here rather than conveniences:
    #   * `mkThunk` defers a binding that depends on the evaluated config
    #     fixpoint, which is what a link's address is.
    #   * `contract.hasFields` is a LAZY per-binding assertion, so a body asking
    #     for a binding that resolution could not supply fails by name instead of
    #     as a missing attribute three frames down.
    #   * `mergeStrategy` / `mkMergeValidator` resolve a binding colliding with a
    #     module-system arg WITH BLAME -- the `nix.settings.netrc-file` case,
    #     where two edges of one fiber both write one singular option and the
    #     silent outcome is last-writer-wins.
    #
    # KNOWINGLY ACCEPTING THE DUPLICATION. Eleven direct inputs measured 18
    # gen-prelude nodes; the `gen` hub that collapses them fails against den's
    # own pinned revision (`'__functor' is not an option of this door`), so the
    # choice is a direct input or nothing. Revisit when den's hub arm works.
    inputs.gen-bind.url = "github:sini/gen-bind";
    #    preProcess =
    #      serialized:
    #      let
    #        isFlake = name: ((serialized.${name}.flake or true) != false);
    #        autoFollow =
    #          name:
    #          let
    #            top = lib.attrNames serialized;
    #            sub = lib.attrNames (inputs.${name}.inputs or { });
    #            shared = lib.filter (n: n != name && isFlake n) (lib.intersectLists top sub);
    #          in
    #          lib.genAttrs shared (s: {
    #            follows = s;
    #          });
    #      in
    #      lib.mapAttrs (
    #        name: spec:
    #        if !(isFlake name) then
    #          spec
    #        else
    #          spec
    #          // {
    #            inputs = lib.recursiveUpdate (autoFollow name) (spec.inputs or { });
    #          }
    #      ) serialized;
    do-not-edit = ''
      #                                _
      #  __ _ _   _ ___  ___ _   _| |__   ___ _ __
      # / _` | | | / __|/ __| | | | '_ \ / _ \ '__|
      #| (_| | |_| \__ \ (__| |_| | |_) |  __/ |
      # \__,_|\__,_|___/\___|\__, |_.__/ \___|_|
      #                      |___/
                  #          This file is generated by the `write-flake` script. To make changes, edit the source files in `aspects/modules/` and run `nix run .#write-flake`.'';
    outputs = /* nix */ ''
      inputs:
      let
        lib = inputs.nixpkgs.lib.extend (import ./lib);

        # ONE call, one file: ./lib/inputs.nix resolves every
        # partitions/<layer>/flake.lock as DATA, merges each layer over the
        # root's own inputs, and patches the result. Layer nodes never enter
        # this flake's lock and stay `fetchTree` thunks only forced if something
        # references them; a shared pin resolves to the root's already-resolved
        # value rather than being fetched twice.
        #
        # ./patched-inputs.nix is generated (`nix run .#write-patched-inputs`)
        # and holds the only part that genuinely needs the module system: which
        # inputs are patched, and with which patches.
        # `checks.patched-inputs-generated-current` fails if it goes stale.
        #
        # Which layer an input is locked in comes from the `layers` tag on the
        # aspect declaring it (aspects/framework/layers.nix), never from a file
        # path. The layer DIRECTORIES are discovered, because resolving a
        # layer's lock is what produces `inputs` -- it happens before any
        # evaluation could read a tag.
        mergedInputs =
          (import ./lib/inputs.nix {
            inherit inputs lib;
            rootPath = ./.;
            patchSpecs = (import ./patched-inputs.nix).inputs;
          }).newInputs;
      in
      inputs.flake-parts.lib.mkFlake
        {
          inputs = mergedInputs;
          specialArgs = {
            realInputs = inputs;
            inherit lib;
          };
        }
        {
          imports = lib.aspectFiles ./aspects;

          _module.args.rootPath = ./.;
        }
    '';
  };
}
