{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  cfg = config.auscybernix.nix;
  build-hook = pkgs.writeTextFile {
    name = "build-hook";
    executable = true;
    destination = "/bin/build-hook";
    text =
      # sh
      ''
                #!/bin/sh
                                	  set -eu
                                set -f # disable globbing
                                export IFS=' '
                  			  export PATH="$PATH:/nix/var/nix/profiles/default/bin:${pkgs.celler}/bin:${pkgs.ts}/bin"
                  			  celler login central https://cache.ivymect.in "$(cat ${config.age.secrets.attic_token.path})"

                                echo "Uploading paths" $OUT_PATHS
        						if [[ -n "''${OUT_PATHS:-}" ]]; then
           export TS_MAXFINISHED=1000
           export TS_SLOTS=10

           echo "Uploading $OUT_PATHS"
           printf "%s" "$OUT_PATHS" \
           | xargs ts celler push main
        fi

      '';

    meta.mainProgram = "build-hook";
  };

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
	    "main:6VI0mVQzSGSA9gB81hwXWllbWrP7ybLaP1Jd3sPUBf4="
            "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
            "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
          ];
        })
        #        {
        #          auto-optimise-store = true;
        #        }
      ];

      #package = pkgs.nix;
      package = pkgs.nixVersions.latest;

      extraOptions = ''
                        experimental-features = nix-command flakes ca-derivations
                		netrc-file = ${config.age.templates.netrc.path}
        				post-build-hook = ${build-hook}/bin/build-hook
      '';
      #	  auto-optimise-store = true;
      #optimise.automatic = true;
      #      gc = {
      #        automatic = true;
      #        options = "--delete-older-than 30d";
      #      };

      #package = pkgs.nixVersions.latest;

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
          		machine cache.ivymect.in
          		password ${placeholders.attic_token}
          	  '';
    };

    nixpkgs.config = {
      #      contentAddressedByDefault = true;
      allowUnfree = true;
    };
  };
}
