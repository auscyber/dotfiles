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
  imports = [
    inputs.nix-topology.flakeModule
    inputs.flake-parts.flakeModules.partitions
    inputs.flake-parts.flakeModules.flakeModules
    inputs.flake-parts.flakeModules.modules
  ];

  systems = [
    "aarch64-darwin"
    "x86_64-linux"
    "aarch64-linux"
  ];
  flake = {
    auscybernix = {
      # Module lists are now derived from config.flake.modules.* in lib/default.nix.
      # Only standalone-specific modules (not embedded in NixOS/Darwin) remain here.
      standaloneHomeModules = [
        inputs.stylix.homeModules.stylix
      ];
    };

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
