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
  ];
  flake = {

  };
}
