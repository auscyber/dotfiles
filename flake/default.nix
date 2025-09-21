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
    inputs.flake-parts.flakeModules.partitions
  ];
  flake = {
    partitions.dev.extraInputsFlake = ./dev;
    partitions.dev.module = ./dev;

  };
}
