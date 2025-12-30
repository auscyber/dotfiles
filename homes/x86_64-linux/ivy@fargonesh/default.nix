{ config, pkgs, ... }:
{
  auscybernix = {
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
