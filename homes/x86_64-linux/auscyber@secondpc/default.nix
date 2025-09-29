{
  config,
  pkgs,
  lib,
  ...
}:
{
  programs.home-manager.enable = true;

  programs.gpg.enable = true;

  auscbernix = {
    shell = {
      enable = true;
      fish = {
        enable = true;
      };
    };
  };
  programs.neovim.enable = true;
  home.stateVersion = "25.05";
}
