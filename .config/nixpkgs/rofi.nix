{pkgs, ...}:
{
  programs.rofi = {
  enable = true;
  terminal = "alacritty";
  theme = "slate";
  font = "Inconsolata 10";
  extraConfig = {
    modi = "combi";
    combi-modi = "window,run,drun,ssh";
  };
  };
}
