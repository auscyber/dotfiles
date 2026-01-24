inp@{
  config,
  inputs,
  self,
  ...
}:
let
  lib = inputs.nixpkgs.lib;
in
{

  flake.auscybernix.builders = {
    sshKeys =
          let
            filteredConfigs = lib.filterAttrs (
              name: value:
              if value.config.auscybernix.nix ? builders then
                value.config.auscybernix.nix.builders.enable
              else
                false
            ) config.flake.auscybernix.systems;
          in
          lib.flip lib.mapAttrsToList filteredConfigs (
            name: value: value.config.auscybernix.nix.builders.sshPublicKey
          );

    buildMachines =
      let
        filteredConfigs = lib.filterAttrs (
          name: value:
          if value.config.auscybernix.nix ? builders then
            value.config.auscybernix.nix.builders.builderConfig.enable
          else
            false
        ) config.flake.auscybernix.systems;
      in

      lib.flip lib.mapAttrs' filteredConfigs (
        name: value: {
          name = value._module.specialArgs.systemIdentifier;
          value = {
			publicHostKey = value.config.auscybernix.nix.builders.builderConfig.publicHostKey;
            username = value.config.auscybernix.nix.builders.builderConfig.builderUser;
            systems = value.config.auscybernix.nix.builders.builderConfig.systems;
            maxJobs = value.config.auscybernix.nix.builders.builderConfig.maxJobs;
            speedFactor = value.config.auscybernix.nix.builders.builderConfig.speedFactor;
            hostname = value.config.auscybernix.nix.builders.builderConfig.hostname;
            ipAddress = value.config.auscybernix.nix.builders.builderConfig.ipAddress;
            features = value.config.auscybernix.nix.builders.builderConfig.features;
          };
        }
      );
  };

}
