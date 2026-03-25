# Window-manager home-manager modules (defaults + per-WM).
{ ... }:
{
  flake.modules.homeManager = {
    wms-defaults = ../_home/wms/default.nix;
    wm-hyprland  = ../_home/wms/hyprland/default.nix;
    wm-niri      = ../_home/wms/niri/default.nix;
    wm-rift      = ../_home/wms/rift/default.nix;
    wm-yabai     = ../_home/wms/yabai/default.nix;
  };
}
