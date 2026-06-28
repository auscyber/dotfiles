{
  den,
  lib,
  inputs,
  ...
}:
let
  hmPlatforms =
    { class, aspect-chain }:
    den.batteries.forward {
      each = [
        "Linux"
        "Darwin"
        "Aarch64"
        "64bit"
      ];
      fromClass = platform: "hm${platform}";
      intoClass = _: "homeManager";
      intoPath = _: [ ];
      fromAspect = _: lib.head aspect-chain;
      guard = { pkgs, ... }: platform: lib.mkIf pkgs.stdenv."is${platform}";
      adaptArgs = { config, ... }: { osConfig = config; };
    };

  standaloneForward =
    { aspect-chain, ... }:
    den.batteries.forward {
      each = [ "standalone" ];
      fromClass = _: "standaloneHome";
      intoClass = _: "homeManager";
      intoPath = _: [ ];
      adaptArgs = lib.id;
      fromAspect = _: lib.head aspect-chain;
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
  den.default.os.home-manager.extraSpecialArgs.inputs = inputs;
  den.default.includes = [
    hmPlatforms
    standaloneForward
  ];
}
