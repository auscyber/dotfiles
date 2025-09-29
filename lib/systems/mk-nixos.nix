{
  inputs,
}:
{
  system,
  hostname,
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
  };

  modules = [
    { _module.args.lib = extendedLib; }

    inputs.agenix.nixosModules.default
    inputs.stylix.nixosModules.stylix
    inputs.lanzaboote.nixosModules.lanzaboote
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-wsl.nixosModules.default
    inputs.sops-nix.nixosModules.sops

    {
      nixpkgs = {
        inherit system;

      }
      // common.mkNixpkgsConfig flake;
    }

    ../../modules/common/secrets.nix
    ../../modules/common/nix
    ../../modules/common/hm
    ../../modules/common/common
    homeManagerConfig
  ]
  ++ (extendedLib.importModulesRecursive ../../modules/nixos)
  ++ [
    ../../systems/${system}/${hostname}
  ]
  ++ modules;

}
