{
  config,
  inputs,
  self,
  lib,
  ...
}:
let
  overlay = config.overlays.default;
  inherit (self.lib.file) parseSystemConfigurations filterNixOSSystems filterDarwinSystems;

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
        value = self.lib.system.mkNixos {
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
  };
}
