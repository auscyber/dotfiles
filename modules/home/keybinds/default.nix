# Keybind home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    keybind-kanata = ../_home/keybinds/kanata/default.nix;
    keybind-skhd   = ../_home/keybinds/skhd/default.nix;
  };
}
