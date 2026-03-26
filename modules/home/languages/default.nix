# Language home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    agda = ../_home/languages/agda/default.nix;
  };
}
