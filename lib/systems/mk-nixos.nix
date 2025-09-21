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
    inputs.home-manager.nixosModules.home-manager
    {
      nixpkgs = {
        inherit system;

      }
      // common.mkNixpkgsConfig flake;
    }

    ../../modules/common/secrets.nix
    ../../modules/common/nix
    ../../modules/common/hm
  ]
  ++ (extendedLib.importModulesRecursive ../../modules/nixos)
  ++ [
    ../../systems/${system}/${hostname}
  ]
  ++ modules;

}
