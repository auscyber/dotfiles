{
  config,
  pkgs,
  system,
  lib,
  ...
}:

{
  targets.genericLinux.enable = true;
  home.packages = with pkgs; [
    discord
    #    rnix-lsp

    gnome-browser-connector

  ];
  home.stateVersion = "25.05"; # Did you read the comment?

}
