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
  caches = {
    "https://nix-community.cachix.org" =
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
    "https://iohk.cachix.org" = "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=";
    "https://cache.nixos.org" = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
    "https://devenv.cachix.org" = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    "https://auscyber.cachix.org" =
      "auscyber.cachix.org-1:RPlENxXc/irvLimM0Yz8Au3ntk/sxZ8bwXPwuXL3v5c=";
    #  "https://cache.ivymect.in/main" = "main:4PgSIjmT7n9adSn4hDnnKXoERhCZR1dTlvj74k+6vT0=";
    ##    "https://attic.xuyh0120.win/lantian" = "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc=";

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
      trusted-substituters = lib.attrNames caches;
      trusted-public-keys = lib.attrValues caches;
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
