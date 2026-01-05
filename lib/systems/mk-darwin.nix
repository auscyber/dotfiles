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

    ../../modules/common/nix
    ../../modules/common/secrets.nix

    ../../modules/common/hm
    ../../modules/common/common

    ../../modules/common/allConfigs.nix
    ../../modules/common/kmonad
    ../../modules/common/ssh-keys.nix
    homeManagerConfig

  ]
  ++ (extendedLib.importModulesRecursive ../../modules/darwin)
  ++ [

    ../../systems/${system}/${hostname}
  ]
  ++ modules
  ;

}
