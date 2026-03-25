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
    extraBuildMachines = {
      "laptop-builder" =
        let
          darwinBuilderConfig = self.darwinConfigurations."Ivys-MacBook-Pro";
          builderConfig = self.darwinConfigurations."Ivys-MacBook-Pro".config.nix.linux-builder;
        in
        {
          inherit (builderConfig) maxJobs systems;
          absoluteSpeedFactor = 10;
          features = builderConfig.supportedFeatures;
          username = "builder";
          ipAddress = "${darwinBuilderConfig.config.auscybernix.nix.builders.builderConfig.ipAddress}:31022";
          hostname = "linux-builder";
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=";
        };
    };
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
      self.auscybernix.builders.extraBuildMachines
      // (
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
              absoluteSpeedFactor = value.config.auscybernix.nix.builders.absoluteSpeedFactor;
              hostname = value.config.auscybernix.nix.builders.builderConfig.hostname;
              ipAddress = value.config.auscybernix.nix.builders.builderConfig.ipAddress;
              features = value.config.auscybernix.nix.builders.builderConfig.features;
            };
          }
        )
      );
  };

}
