{
  config,
  pkgs,
  system,
  lib,
  ...
}:

{
  nixpkgs.config.allowUnfree = true;
  targets.genericLinux.enable = true;
  home.packages = with pkgs; [
    discord
    #    rnix-lsp

    gnome-browser-connector

  ];

}
