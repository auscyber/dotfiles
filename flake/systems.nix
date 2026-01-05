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
    auscybernix = {

      importedHomeModules = [
        inputs._1password-shell-plugins.hmModules.default
        inputs.zen-browser.homeModules.default
        inputs.nixvim.homeModules.default
        inputs.nix-index-database.homeModules.nix-index
        inputs.sops-nix.homeManagerModules.sops
        inputs.agenix.homeManagerModules.default
        inputs.agenix-rekey.homeManagerModules.default

      ];
      standaloneHomeModules = [
        inputs.stylix.homeModules.stylix
      ];
      importedNixosModules = [
        inputs.stylix.nixosModules.stylix
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
