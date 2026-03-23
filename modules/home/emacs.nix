{
  config,
  pkgs,
  system,
  lib,
  ...
}:
{
  programs.emacs = {
    enable = true;
    #    extraPackages = epkgs: with epkgs; [ magit vterm ];
    package = pkgs.emacsNativeComp;
  };
  #  home.packages = with pkgs; [ ];
  services.emacs = {
    enable = true;
  };
}
