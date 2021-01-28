{pkgs, ...}:
{
  programs.rofi = {
  enable = true;
  terminal = "alacritty";
  theme = "slate";
  font = "Inconsolata 10";
  extraConfig = {
    combi-modi = "window,run,drun,ssh";
  };
  };
}
