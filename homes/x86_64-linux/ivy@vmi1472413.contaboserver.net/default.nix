{ config, pkgs, ... }:
{
  auscybernix.modules.enable = {
    allConfigs        = true;
    nix               = true;
    secrets           = true;
    secrets-platform  = true;
    standalone        = true;
    default           = true;
    file              = true;
    neovim            = true;
    fish              = true;
    shell             = true;
    ext-nixvim        = true;
    ext-sops          = true;
    ext-nix-index     = true;
  };
  auscybernix.standalone.enable = true;

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
