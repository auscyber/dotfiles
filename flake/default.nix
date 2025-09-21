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
    ./packages.nix
    inputs.flake-parts.flakeModules.partitions
  ];
  flake = {
    partitions.dev.extraInputsFlake = ./dev;
    partitions.dev.module = ./dev;

  };
}
