{ config
, pkgs
, system
, lib
, ...
}:

{
  targets.genericLinux.enable = true;
  home.packages = with pkgs; [
    discord
    #    rnix-lsp

    gnome-browser-connector

  ];

}
