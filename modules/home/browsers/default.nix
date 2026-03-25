# Browser home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    browser-zen    = ../_home/browsers/zen/default.nix;
    browser-helium = ../_home/browsers/helium/default.nix;
  };
}
