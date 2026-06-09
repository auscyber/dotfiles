{ den, lib, ... }:
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

in
{
  flake-file.inputs.home-manager = {
    url = "github:auscyber/home-manager?ref=inputs/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  den.default.includes = [ hmPlatforms ];
}
