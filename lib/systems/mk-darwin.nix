{
  inputs,
  importedDarwinModules,
  common,
}:
{
  system,
  hostname,
  username ? "IvyPierlot",
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
      matchingHomes
      hostname
      ;
    isNixOS = false;
  };

in
inputs.darwin.lib.darwinSystem {
  specialArgs = common.mkSpecialArgs {
    inherit
      inputs
      hostname
      system
      extendedLib
      ;
 } // {
 systemIdentifier = "${hostname}-${system}";
};
inherit system;
modules = [
{ _module.args.lib = extendedLib; }
]
++ importedDarwinModules
++ [

{
	nixpkgs = {
		inherit system;

	}
	// common.mkNixpkgsConfig flake;

	auscybernix.secrets.enable = true;
}

../../modules/_common/nix
../../modules/_common/secrets.nix

../../modules/_common/hm
../../modules/_common/common

../../modules/_common/allConfigs.nix
../../modules/_common/kmonad
../../modules/_common/ssh-keys.nix
homeManagerConfig

	]
++ (extendedLib.importModulesRecursive ../../modules/_darwin)
	++ [

		../../systems/${system}/${hostname}
	]
	++ modules;

	}
