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
    { class, aspect-chain }:
    den.batteries.forward {
      each = lib.singleton class;
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
      user,
    }:
    den.batteries.forward {
      each = [ "standalone" ];
      fromClass = _: "hmStandalone";
      intoClass = _: "homeManager";
      intoPath = _: [ ];
      adaptArgs = lib.id;
      fromAspect = _: user.aspect;
      guard =
        {
          osConfig ? null,
          ...
        }:
        _: lib.mkIf (osConfig == null);
    };

in
{
  flake-file.inputs.home-manager = {
    url = "github:nix-community/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  patchedInputs.home-manager = {
    #    patches = [
    #      #      ../../patches/home-manager/edit.patch
    #      #      ../../patches/home-manager/one.patch
    #    ];

  };
  den.default.os.home-manager = {
    useGlobalPkgs = true;
    extraSpecialArgs.inputs = inputs;
  };
  den.default.includes = [
    hmPlatforms
    #    hmAlias
    #    standaloneForward
  ];
}
