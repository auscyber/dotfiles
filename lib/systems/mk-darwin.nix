{ inputs }:
{
  system,
  hostname,
  username ? "IvyPierlot",
  modules ? [ ],
  ...

}:
let
  common = import ./common.nix { inherit inputs; };
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
  };
  inherit system;
  modules = [
    { _module.args.lib = extendedLib; }

    inputs.agenix.darwinModules.default
    inputs.stylix.darwinModules.stylix
    inputs.nix-homebrew.darwinModules.nix-homebrew
    inputs.home-manager.darwinModules.home-manager
    inputs.sops-nix.darwinModules.sops

    {
      nixpkgs = {
        inherit system;

      }
      // common.mkNixpkgsConfig flake;
    }

    ../../modules/common/nix
    ../../modules/common/secrets.nix
    ../../modules/common/hm
    ../../modules/common/common
    ../../modules/common/kmonad
    homeManagerConfig

  ]
  ++ (extendedLib.importModulesRecursive ../../modules/darwin)
  ++ [

    ../../systems/${system}/${hostname}
  ]
  ++ modules;

}
