{ config, pkgs, ... }:
{
  sops.age.keyFile = "/home/ivy/.config/sops/age/keys.txt";
  targets.genericLinux.enable = true;
  auscybernix = {
  nix.flake = "/home/ivy/dotfiles";
    secrets.enable = true;
    shell = {
      enable = true;
      fish = {
        enable = true;
      };
    };
    programs.neovim.enable = true;
  };
  home.packages = with pkgs; [ gnupg ];
  home.stateVersion = "24.05";

}
