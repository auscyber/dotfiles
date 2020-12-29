{pkgs, ...}:
{

  enable = true;
  terminal = "alacritty";
  theme = "slate";
  font = "Inconsolata 10";
  extraConfig = ''
    combi-modiL "window,run,drun,ssh";
  '';

}
