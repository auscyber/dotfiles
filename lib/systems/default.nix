{
  inputs,
  importedDarwinModules,
  importedNixosModules,
  importedHomeModules,
  standaloneHomeModules,
  ...
}:
rec {
  mkDarwin = import ./mk-darwin.nix { inherit inputs importedDarwinModules common; };
  mkNixos = import ./mk-nixos.nix { inherit inputs importedNixosModules common; };
  rpi =
    let
      rpi = import ./mk-rpi.nix { inherit inputs importedNixosModules common; };
    in
    {
      mkSystem = args: (rpi args).mkSystem;
      mkInstaller = args: (rpi args).mkInstaller;
    };
  common = import ./common.nix { inherit inputs importedHomeModules; };
  mkHome = import ./mk-home.nix {
    inherit
      inputs
      importedHomeModules
      standaloneHomeModules
      common
      ;
  };
}
