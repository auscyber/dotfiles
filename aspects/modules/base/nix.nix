{
  lib,
  den,
  __find_file,
  rootPath,
  ...
}:
let

  nixClass =
    { class, aspect-chain, ... }:
    den.provides.forward {
      each = [
        "nixos"
        "darwin"
      ];
      fromClass = _: "nix";
      intoClass = lib.id;
      intoPath = _: [
        "nix"
      ];
      #      fromAspect = _: lib.head aspect-chain;
      adaptArgs = lib.id;
    };

in
{
  flake-file.inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    # all your other inputs
  };

  den.default.includes = [ den.aspects.nix ];

  den.aspects.nix = {

    includes = [
      nixClass
    ];
    nixos.nix.settings.trusted-users = [ "@wheel" ];
    os.nix = {
      gc.automatic = true;
      channel.enable = false;
    };
    nix.settings = {
      experimental-features = [
        "nix-command"
        "flakes"
        "pipe-operators"
      ];
    };
    darwin = {
      nix.enable = true;
      nix.settings.trusted-users = [ "@admin" ];
      nix.settings.auto-optimise-store = true;
      nix.optimise = {
        automatic = true;
        interval = [
          {
            Hour = 4;
            Minute = 15;
            Weekday = 7;
          }
        ];
      };
    };
  };

}
