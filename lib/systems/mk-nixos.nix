{
  inputs,
  importedNixosModules,
  common,
}:
{
  system,
  hostname,
  modules ? [ ],
  ...
}:
let

  flake = inputs.self;
  extendedLib = common.mkExtendedLib flake inputs.nixpkgs;
  matchingHomes = common.mkHomeConfigs {
    inherit
      flake
      system
      hostname
      ;
  };
  homeManagerConfig = common.mkHomeManagerConfig {
    inherit
      extendedLib
      inputs
      system
      hostname
      matchingHomes
      ;
    isNixOS = true;
  };

in
inputs.nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = common.mkSpecialArgs {
    inherit
      inputs
      hostname
      system
      extendedLib
      ;

 }// {
 systemIdentifier = "${hostname}-${system}";
};

modules = [
{ _module.args.lib = extendedLib; }
]
++ importedNixosModules
++ [

{
	nixpkgs = {
		inherit system;

	}
	// common.mkNixpkgsConfig flake;
}

../../modules/common/secrets.nix
../../modules/common/nix

../../modules/common/allConfigs.nix
../../modules/common/hm
../../modules/common/common
../../modules/common/ssh-keys.nix
(
 { config, lib, ... }:
 {
 auscybernix.secrets.enable = true;

 }
 )
homeManagerConfig
	]
++ (extendedLib.importModulesRecursive ../../modules/nixos)
	++ [
		../../systems/${system}/${hostname}
	]
	++ modules;

	}
