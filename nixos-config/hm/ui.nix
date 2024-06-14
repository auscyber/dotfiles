{ config, pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;
  targets.genericLinux.enable = true;
  home.packages = with pkgs; [
    discord
    wezterm
    kalendar
    #    rnix-lsp
    (polybar.override {
      pulseSupport = true;
      iwSupport = true;
      githubSupport = true;
    })
    gnome.gnome-mines
    google-chrome
    
  ];
}
