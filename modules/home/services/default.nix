# Service home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    mopidy = ../_home/services/mopidy/default.nix;
  };
}
