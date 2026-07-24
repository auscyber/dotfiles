{
  den,
  lib,
  inputs,
  ...
}:
let
  hmPlatforms =
    {
      class,
      aspect-chain,
      host,
      user,
    }:
    den.provides.forward {
      each = [
        #        "Linux"
        "Darwin"
      ];
      fromClass = platform: "hm${platform}";
      intoClass = _: "homeManager";
      intoPath = _: [ ];
      fromAspect = _: lib.head aspect-chain;
      guard = { pkgs, ... }: platform: lib.mkIf pkgs.stdenv."is${platform}";
      adaptArgs = lib.id;
    };

  hmAlias =
    {
      class,
      aspect-chain,
    }:
    den.batteries.forward {
      each = lib.singleton true;
      fromClass = _: "hm";
      intoClass = _: "homeManager";
      intoPath = _: [ ];
      fromAspect = _: lib.head aspect-chain;
      adaptArgs = { config, ... }: { osConfig = config; };
    };

  standaloneForward =
    {
      aspect-chain,
      class,
      home,
      user,
    }:
    den.batteries.forward {
      each = [ "standalone" ];
      fromClass = _: "hmStandalone";
      intoClass = _: "homeManager";
      intoPath = _: [ ];
      adaptArgs = lib.id;
      fromAspect = _: user.aspect;
    };
in
{
  ff.home-manager = {
    url = "github:nix-community/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
    meta.addRegistry = true;
  };
  # Patches are auto-included from ./patches/home-manager/*.patch.
  patchedInputs.home-manager = { };
  den.default.os.home-manager = {
    useGlobalPkgs = true;
    extraSpecialArgs.inputs = inputs;
  };
  den.default.includes = [
    hmPlatforms
    #  hmAlias
    standaloneForward
  ];
}
