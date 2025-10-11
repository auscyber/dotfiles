{ config, pkgs, lib,... }:
let cfg = config.auscybernix.nix;
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
    settings = lib.mkIf cfg.caches {
      substituters = [
        "https://nix-community.cachix.org"
        "https://iohk.cachix.org"
        "https://cache.nixos.org"
        "https://devenv.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      ];
    };

    #    package = pkgs.lixPackageSets.latest.lix;

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };

    package = pkgs.nixVersions.stable;
  };

  nixpkgs.config.allowUnfree = true;
  };
}
