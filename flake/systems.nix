{
  config,
  inputs,
  self,
  lib,
  ...
}:
let
  overlay = config.overlays.default;
  inherit (self.lib.file)
    parseSystemConfigurations
    filterNixOSSystems
    filterDarwinSystems
    filterRpiSystems
    ;
  inherit (inputs.nixpkgs.lib)
    hasSuffix
    ;
  systemsPath = ../systems;
  allSystems = parseSystemConfigurations systemsPath;
in
{
  flake = {

    nixosConfigurations = lib.mapAttrs' (
      name:
      { system, hostname, ... }:
      {
        name = hostname;
        value =
          if hasSuffix "rpi" system then
            self.lib.system.rpi.mkSystem {
              inherit inputs system hostname;
            }
          else

            self.lib.system.mkNixos {
              inherit inputs system hostname;
            };
      }
    ) (filterNixOSSystems allSystems);

    darwinConfigurations = lib.mapAttrs' (
      name:
      { system, hostname, ... }:
      {
        name = hostname;
        value = self.lib.system.mkDarwin {
          inherit inputs system hostname;
          username = "IvyPierlot";
        };
      }
    ) (filterDarwinSystems allSystems);
    installImages = lib.mapAttrs' (
      name:
      { system, hostname, ... }:
      {
        name = hostname;
        value =
          (self.lib.system.rpi.mkInstaller {
            inherit inputs system hostname;
          }).config.system.build.sdImage;
      }
    ) (filterRpiSystems allSystems);
  };
}
