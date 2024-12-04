{ config, lib, pkgs, ... }:

{
  fonts.packages = with pkgs;
    [
      (nerdfonts.override {
        fonts = [ "FiraCode" "Inconsolata" "Hasklig" "RobotoMono" ];
      })
    ];


  programs.zsh.enable = true;
  nix = {
    # Binary Cache for Haskell.nix
    settings.trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    settings.substituters = [
      "https://hydra.iohk.io"
      "https://nix-community.cachix.org"
    ];
    package = pkgs.nixVersions.latest;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs.config.allowUnfree = true;

}
