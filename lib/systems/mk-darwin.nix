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
  }
  homeManagerConfig
]
++ [
  ../../systems/${system}/${hostname}
]
++ modules;

}
