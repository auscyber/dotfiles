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
      adaptArgs = lib.id;
    };

  overlay-apply =
    { config, ... }:
    {
      options._overlays = lib.mkOption {
        type = lib.types.attrsOf (lib.types.either overlayFn (lib.types.listOf overlayFn));
        default = { };
        apply = f: lib.concatMap (v: if builtins.isList v then v else [ v ]) (lib.attrValues f);
        description = "Named overlays collected from aspects; merged into nixpkgs.overlays as a list.";
      };
      config.nixpkgs.overlays = config._overlays;
    };
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
      sysHosts = lib.attrValues (den.hosts.${system} or { });
      capturedFor =
        host:
        (den.lib.capture.captureWithPathsWith {
          classes = [ "overlay" ];
          root = den.lib.resolveEntity "host" { inherit host; };
          ctx = { inherit host; };
        }).entries or [ ];
      overlayAttrs = lib.foldl' lib.recursiveUpdate { } (
        lib.concatMap (h: map (e: e.value or { }) (capturedFor h)) sysHosts
      );
    in
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = lib.attrValues overlayAttrs;
      };
    };
}
