{
  den,
  lib,
  inputs,
  ...
}:
let
  inherit (den.lib.policy) route;

  overlayFn = lib.mkOptionType {
    name = "overlay-function";
    description = "nixpkgs overlay (final: prev: { ... })";
    check = x: builtins.isFunction x;
    # Overlays are functions and can't be compared with `==`, so `mergeEqualOption`
    # throws whenever the same overlay is collected from more than one entity
    # (every `den.default` overlay lands here once per host). They are the *same*
    # declaration, so dedup by taking the first definition rather than merging.
    merge = _loc: defs: (builtins.head defs).value;
  };

  # Wrap an overlay so every derivation it returns at the top level carries
  # `meta.overlayName = <name>`. Lets tooling (and `nix derivation show`) say
  # which overlay introduced any given package. Non-derivation values pass
  # through unchanged.
  tagOverlay =
    name: overlay: final: prev:
    let
      result = overlay final prev;
      tagOne =
        pkg:
        if lib.isDerivation pkg && (pkg ? overrideAttrs) then
          pkg.overrideAttrs (old: {
            meta = (old.meta or { }) // {
              overlayName = name;
            };
          })
        else
          pkg;
    in
    lib.mapAttrs (_: tagOne) result;

  # Flatten a `{ name = overlay-or-list; }` attrset into a flat list of
  # name-tagged overlay functions.
  tagOverlaySet =
    overlays:
    lib.concatMap (
      name:
      let
        v = overlays.${name};
      in
      if builtins.isList v then map (tagOverlay name) v else [ (tagOverlay name v) ]
    ) (lib.attrNames overlays);

  overlay-apply =
    {
      config,
      osConfig ? null,
      ...
    }:
    {
      options._overlays = lib.mkOption {
        type = lib.types.attrsOf (lib.types.either overlayFn (lib.types.listOf overlayFn));
        default = { };
        apply = tagOverlaySet;
        description = "Named overlays collected from aspects; each derivation gets meta.overlayName set to the entry name.";
      };
      config.nixpkgs.overlays = lib.mkIf (osConfig == null) config._overlays;
    };

  # Filter den-internal attrs (`_`, `__functor`, `name`, etc.) so we only iterate
  # the real package aspects under den.aspects.packages.
  isAspectName =
    name:
    !(lib.hasPrefix "_" name)
    && !(builtins.elem name [
      "classes"
      "description"
      "excludes"
      "includes"
      "meta"
      "name"
      "policies"
      "provides"
    ]);

  collectPackageOverlays =
    sources: system: pkgs:
    let
      packageAspects = lib.filterAttrs (n: _: isAspectName n) (den.aspects.packages or { });

      # Two declaration shapes are valid:
      #
      #   overlays = { sources, ... }: { foo = self: super: {...}; };
      #
      #     A configurator function that takes deps and returns a named attrset
      #     of overlay-fns. (ghostty, helium, cotabby, ivy-fetch, etc.)
      #
      #   overlays.foo = { sources, ... }: { foo = self: super: {...}; };
      #
      #     An attrset of configurators. (eagle-nvim.) Each value is invoked
      #     with deps and the union of all results is taken.
      #
      # Anything else falls through as empty.
      callDeps = fn: fn { inherit sources system pkgs; };
      invoke =
        v:
        if builtins.isFunction v then
          callDeps v
        else if builtins.isAttrs v then
          lib.foldl' (
            acc: name:
            let
              inner = v.${name};
              evaluated =
                if builtins.isFunction inner then
                  # Configurator that returns a named overlay-attrset.
                  callDeps inner
                else
                  { ${name} = inner; };
            in
            acc // evaluated
          ) { } (lib.attrNames v)
        else
          { };

      perAspect = name: aspect: if aspect ? overlays then invoke aspect.overlays else { };

      # Strip den-internal keys (e.g. `__provider`, `__providesForwarded`)
      # so we only keep real overlay-fn entries.
      stripInternal = lib.filterAttrs (n: _: !(lib.hasPrefix "_" n));
    in
    stripInternal (
      lib.foldl' (acc: name: acc // (perAspect name packageAspects.${name})) { } (
        lib.attrNames packageAspects
      )
    );
in
{
  den.aspects.overlays = {

    includes = [ den.policies.overlays-to-_overlays ];
    nixos = overlay-apply;
    darwin = overlay-apply;
    homeManager = overlay-apply;
  };

  den.default.includes = [
    den.aspects.overlays
    den.policies.overlays-to-flake-parts
  ];

  # Make `overlays` a first-class den class and collect every aspect's
  # `overlays` entries into a single flake-parts (perSystem) value —
  # mirroring the `devshell` -> `devshells.default` collection in
  # aspects/utils/devshells.nix. This lets the shared `pkgs` see overlays
  # declared anywhere, not just under `den.aspects.packages`.
  #
  # `adaptArgs` deliberately instantiates `sources` from a bare
  # `inputs.nixpkgs` and does NOT forward `pkgs`: the collected overlays are
  # what *builds* the perSystem `pkgs`, so feeding the final (overlaid) pkgs
  # back into their configurators would be an infinite recursion.
  den.classes.overlays = { };
  den.policies.overlays-to-_overlays = { aspect-chain, ... }: [

    (den.lib.policy.route {
      each = [
        { sys = "nixos"; }
        { sys = "darwin"; }
        { sys = "homeManager"; }
      ];
      fromClass = _: "overlays";
      intoClass = p: p.sys;
      intoPath = _: [ "_overlays" ];
      fromAspect = _item: lib.head aspect-chain;
      adaptArgs = args: args // { system = args.pkgs.stdenv.hostPlatform.system; };
    })
  ];
  den.policies.overlays-to-flake-parts = _: [
    (route {
      fromClass = "overlays";
      intoClass = "flake-parts";
      path = [ "_collectedOverlays" ];
      collectSubtree = true;
      # Overlay configurators declared as `{ sources, ... }: ...` need `sources`
      # (and may read `pkgs`). Provide both from a *bare* `inputs.nixpkgs` — never
      # the perSystem `pkgs`/`sources` in `args`, since those are what these very
      # overlays build. Forwarding the overlaid pkgs back in is infinite recursion.
      adaptArgs =
        args:
        let
          basePkgs = import inputs.nixpkgs { inherit (args) system; };
        in
        args
        // {
          pkgs = basePkgs;
          sources = basePkgs.callPackage ../../_sources/generated.nix { };
        };
    })
  ];
  den.schema.flake-parts.includes = [
    den.policies.overlays-to-flake-parts
    den.policies.flake-parts-to-host
  ];
  # `flake-parts-to-host` resolves each host *into* the perSystem/flake-parts
  # scope so the overlays route can collect their `overlays` class. But the
  # `inputs'`/`self'` batteries provide their module args via `withSystem system`
  # — which forces the very perSystem being built, an infinite recursion. They
  # are irrelevant to overlay collection (overlays use `inputs`/`sources`, never
  # `inputs'`/`self'`), so exclude them from *this* scope only. Schema-tier
  # excludes are scope-local, so real host configs (resolved under `flake-mod`)
  # keep `inputs'`/`self'`.
  den.schema.flake-parts.excludes = [
    den.batteries.inputs'
    den.batteries.self'
  ];

  perSystem =
    { system, config, ... }:
    let
      basePkgs = import inputs.nixpkgs {
        inherit system;
      };
      sources = basePkgs.callPackage ../../_sources/generated.nix { };

      # Registry walk over `den.aspects.packages` (configurator shapes that are
      # not included into any entity, so the class route below never sees them).
      packageOverlays = collectPackageOverlays sources system basePkgs;

      # Class-collected overlays from every other aspect (celler, auspc, zen,
      # gaming, nh, rift, kanata, …), routed in via `overlays-to-flake-parts`.
      classOverlays = config._collectedOverlays;

      allOverlays = packageOverlays // classOverlays;
    in
    {
      options._collectedOverlays = lib.mkOption {
        type = lib.types.attrsOf (lib.types.either overlayFn (lib.types.listOf overlayFn));
        default = { };
        description = "Overlays collected from every aspect's `overlays` class, keyed by entry name.";
      };

      config = {
        _module.args.pkgs = lib.mkForce (import inputs.nixpkgs {
          inherit system;
          overlays = tagOverlaySet allOverlays;
          config = {
            allowUnfree = true;
          };
        });
        _module.args.overlays = tagOverlaySet allOverlays;
      };
    };
}
