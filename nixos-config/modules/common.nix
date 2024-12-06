{ config, lib, pkgs, ... }:

{

  fonts.packages = with pkgs;
    (map (x: nerd-fonts.${x}) [ "fira-code" "inconsolata" "hasklug" "roboto-mono" ]);

  environment.systemPackages = with pkgs; [ git vim ];

  programs.zsh.enable = true;
  nix = {
    # Binary Cache for Haskell.nix
    settings.trusted-public-keys = [
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    settings.substituters = [
      "https://iohk.cachix.org"
      "https://nix-community.cachix.org"
    ];
    package = pkgs.nixVersions.latest;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs.config.allowUnfree = true;

}
