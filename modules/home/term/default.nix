# Terminal home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    term-ghostty = ../_home/term/ghostty/default.nix;
  };
}
