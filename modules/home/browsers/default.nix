# Browser home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    zen    = ../_home/browsers/zen/default.nix;
    helium = ../_home/browsers/helium/default.nix;
  };
}
