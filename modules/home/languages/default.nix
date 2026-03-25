# Language home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    lang-agda = ../_home/languages/agda/default.nix;
  };
}
