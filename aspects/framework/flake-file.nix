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
      { host ? { }, ... }:
      let
        hostLayers = host.layers or [ ];
        layer =
          if hostLayers == [ ] then
            null
          else
            lib.concatStringsSep "-" (lib.sort (a: b: a < b) hostLayers);
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
