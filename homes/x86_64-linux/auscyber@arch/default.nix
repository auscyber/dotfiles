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
    prismlauncher
    #    rnix-lsp
    (polybar.override {
      pulseSupport = true;
      iwSupport = true;
      githubSupport = true;
    })
    gnome-mines
    #google-chrome

  ];
  home.stateVersion = "25.05"; # Did you read the comment?
}
