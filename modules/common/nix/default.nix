{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  cfg = config.auscybernix.nix;
in
{

  options.auscybernix.nix = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
    caches = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

  };
  config = lib.mkIf cfg.enable {

    nix = {
      # Binary Cache for Haskell.nix
      settings = lib.mkMerge [
        (lib.mkIf cfg.caches {
          substituters = [
            "https://nix-community.cachix.org"
            "https://iohk.cachix.org"
            "https://cache.nixos.org"
            "https://devenv.cachix.org"
            "https://cache.ivymect.in/main"
          ];
          trusted-public-keys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "main:4PgSIjmT7n9adSn4hDnnKXoERhCZR1dTlvj74k+6vT0="
            "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
            "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
          ];
        })
        #        {
        #          auto-optimise-store = true;
        #        }
      ];

      #    package = pkgs.nixVersions.latest;

      extraOptions = ''
                experimental-features = nix-command flakes
        		netrc-file = "${config.age.templates.netrc.path}"
      '';
      #	  auto-optimise-store = true;
      #optimise.automatic = true;
      #      gc = {
      #        automatic = true;
      #        options = "--delete-older-than 30d";
      #      };

      package = pkgs.nixVersions.latest;

    };
    age.secrets.attic_token = {
      rekeyFile = ../../../secrets/attickey.age;
    };

    age.templates.netrc = {
      dependencies = {
        inherit (config.age.secrets) attic_token;
      };
      content =
        {
          pkgs,
          placeholders,
          ...
        }:
        ''
          		machine https://cache.ivymect.in/.+
          		password ${placeholders.attic_token}
          	  '';
    };

    nixpkgs.config = {
      #      contentAddressedByDefault = true;
      allowUnfree = true;
    };
  };
}
