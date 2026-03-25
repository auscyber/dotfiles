# Service home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    service-mopidy = ../_home/services/mopidy/default.nix;
  };
}
