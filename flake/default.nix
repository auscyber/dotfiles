{
  self,
  inputs,
  lib,
  ...
}:
{
  imports = [

    inputs.agenix-rekey.flakeModule
    ./systems.nix
    ../lib
    ../docs
    ../overlays
    ./homes.nix

    ./secrets.nix
    ./shells.nix
    ../ci
    ./packages.nix
    ./input-branches.nix
    inputs.flake-parts.flakeModules.partitions
	inputs.flake-parts.flakeModules.flakeModules
    inputs.input-branches.flakeModules.default
    ./formatter.nix
  ];
  flake = {

  };

}
