{config,pkgs,...}:
{
  programs.kitty.enable = true; # required for the default Hyprland config
  wayland.windowManager.hyprland.enable = true; # enable Hyprland

}
