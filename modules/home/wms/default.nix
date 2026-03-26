# Window-manager home-manager modules (defaults + per-WM).
{ ... }:
{
  flake.modules.homeManager = {
    wm-defaults = ../_home/wms/default.nix;
    hyprland  = ../_home/wms/hyprland/default.nix;
    niri      = ../_home/wms/niri/default.nix;
    rift      = ../_home/wms/rift/default.nix;
    yabai     = ../_home/wms/yabai/default.nix;
  };
}
