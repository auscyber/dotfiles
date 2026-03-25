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
  ];

  systems = [
    "aarch64-darwin"
    "x86_64-linux"
    "aarch64-linux"
  ];
  flake = {
    auscybernix = rec {
	containerModules = importedNixosModules ++ self.lib.importModulesRecursive ./_nixos;

      importedHomeModules = [
        inputs._1password-shell-plugins.hmModules.default
        inputs.zen-browser.homeModules.default
        inputs.nixvim.homeModules.default
        inputs.nix-index-database.homeModules.nix-index
        inputs.sops-nix.homeManagerModules.sops
        inputs.agenix.homeManagerModules.default
        inputs.agenix-rekey.homeManagerModules.default
        inputs.vscode-server.homeModules.default

		inputs.niri.homeModules.niri
      ];
      standaloneHomeModules = [
#		inputs.niri.homeModules.default
        inputs.stylix.homeModules.stylix
      ];
      importedNixosModules = [
	  ./_common/vpn.nix
	  ./_common/builders
    ./_common/secrets.nix
    ./_common/nix
    ./_common/common

#	  ./_common/builders/builder.nix
        inputs.stylix.nixosModules.stylix
        inputs.arion.nixosModules.arion
        inputs.lanzaboote.nixosModules.lanzaboote
        inputs.impermanence.nixosModules.impermanence
        inputs.home-manager.nixosModules.home-manager
        inputs.nixos-wsl.nixosModules.default
        inputs.agenix.nixosModules.default
        inputs.agenix-rekey.nixosModules.default
        inputs.sops-nix.nixosModules.sops
        inputs.attic.nixosModules.atticd

      ];
      importedDarwinModules = [

#	  ./_common/builders/builder.nix
	  ./_common/builders
	  ./_common/vpn.nix
        inputs.stylix.darwinModules.stylix
        inputs.nix-homebrew.darwinModules.nix-homebrew
        inputs.home-manager.darwinModules.home-manager
        inputs.sops-nix.darwinModules.sops
        inputs.agenix.darwinModules.default
        inputs.agenix-rekey.nixosModules.default

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
