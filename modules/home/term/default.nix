# Terminal home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    ghostty = ../_home/term/ghostty/default.nix;
  };
}
