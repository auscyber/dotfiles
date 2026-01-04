{
  self,
  inputs,
  lib,
  ...
}:
{
  imports = [
    ./systems.nix
    ../lib
    ../docs
    ../overlays
    ./homes.nix
    ./shells.nix
    ../ci
    ./packages.nix
    ./input-branches.nix
    inputs.flake-parts.flakeModules.partitions
    inputs.input-branches.flakeModules.default
    ./formatter.nix
  ];
  flake = {

  };

}
