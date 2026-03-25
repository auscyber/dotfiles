# Miscellaneous home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    misc-picom     = ../_home/picom.nix;
    misc-minecraft = ../_home/minecraft.nix;
    misc-vencord   = ../_home/vencord.nix;
  };
}
