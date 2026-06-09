{
  lib,
  den,
  __find_file,
  ...
}:
let
  forwarded =
    { class, aspect-chain }:
    den.provides.forward {
      each = [
        "Linux"
        "Darwin"
      ];
      fromClass = platform: "nix${platform}";
      intoClass = _: "nix";
      intoPath = _: [ ];
      fromAspect = _: lib.head aspect-chain;
      guard = { pkgs, ... }: platform: lib.mkIf pkgs.stdenv."is${platform}";
    };

  nixClass =
    { class, aspect-chain, ... }:
    den.provides.forward {
      each = [
        "nixos"
        "homeManager"
        "darwin"
      ];
      fromClass = _: "nix";
      intoClass = lib.id;
      intoPath = _: [
        "nix"
      ];
      fromAspect = _: lib.head aspect-chain;
      adaptArgs = lib.id;
    };

in
{
  flake-file.inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    # all your other inputs
  };

  den.aspects.nix = {
    includes = [
      forwarded
      nixClass
    ];
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
      "pipe-operators"
    ];
    #nixDarwin.optimise = {
    #  automatic = true;
    #  interval = [
    #    {
    #      Hour = 4;
    #      Minute = 15;
    #      Weekday = 7;
    #    }
    #  ];
    #};
  };
  flake-file.nixConfig = {
    experimental-features = [
      "nix-command"
      "flakes"
      "pipe-operators"
    ];
    extra-substituters = [
      "https://cache.ivymect.in/main"
    ];
    extra-trusted-public-keys = [
      "main:4PgSIjmT7n9adSn4hDnnKXoERhCZR1dTlvj74k+6vT0="
    ];
  };

}
