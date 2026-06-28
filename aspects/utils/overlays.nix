{
  den,
  lib,
  inputs,
  ...
}:
let
  overlayFn = lib.mkOptionType {
    name = "overlay-function";
    description = "nixpkgs overlay (final: prev: { ... })";
    check = x: builtins.isFunction x;
    merge = lib.mergeEqualOption;
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

  overlay-class =
    { aspect-chain, ... }:
    den.batteries.forward {
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
    };

  overlay-apply =
    { config, ... }:
    {
      options._overlays = lib.mkOption {
        type = lib.types.attrsOf (lib.types.either overlayFn (lib.types.listOf overlayFn));
        default = { };
        apply = tagOverlaySet;
        description = "Named overlays collected from aspects; each derivation gets meta.overlayName set to the entry name.";
      };
      config.nixpkgs.overlays = config._overlays;
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
    includes = [ overlay-class ];
    nixos = overlay-apply;
    darwin = overlay-apply;
    homeManager = overlay-apply;
  };

  den.default.includes = [ den.aspects.overlays ];

  perSystem =
    { system, ... }:
    let
      basePkgs = import inputs.nixpkgs {
        inherit system;

      };
      sources = basePkgs.callPackage ../../_sources/generated.nix { };
      packageOverlays = collectPackageOverlays sources system basePkgs;
    in
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = tagOverlaySet packageOverlays;
        config = {
          allowUnfree = true;
        };
      };
    };
}
