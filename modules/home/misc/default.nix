# Miscellaneous home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    picom     = ../_home/picom.nix;
    minecraft = ../_home/minecraft.nix;
    vencord   = ../_home/vencord.nix;
  };
}
