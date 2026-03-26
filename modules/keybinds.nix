# Keyboard remapping tools — all keyboard-level remap tools in one place.
# System-level (nix-darwin): kmonad (via _common) + karabiner driver.
# Home Manager: kanata service + skhd hotkey daemon.
{ ... }:
{
  flake.modules = {
    darwin = {
      keybind-kmonad           = ../_common/kmonad/default.nix;
      keybind-karabiner-driver = ../_darwin/keybinds/karabiner_driver/default.nix;
    };
    homeManager = {
      keybind-kanata = ../_home/keybinds/kanata/default.nix;
      keybind-skhd   = ../_home/keybinds/skhd/default.nix;
    };
  };
}
