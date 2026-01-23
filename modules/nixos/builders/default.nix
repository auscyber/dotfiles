{
  config,
  lib,
  pkgs,
  flakeConfig,
  system,
  systemIdentifier,
  ...
}:
{

  nix.buildMachines =
    lib.flip lib.mapAttrsToList
      (lib.filterAttrs (
        name: _: name != systemIdentifier
      ) flakeConfig.flake.auscybernix.builders.buildMachines)
      (
        name: builder: {
          protocol = "ssh-ng";
          hostName = builder.ipAddress;
          systems = builder.systems;
          maxJobs = builder.maxJobs;
          speedFactor = builder.speedFactor;
          supportedFeatures = builder.features;
          sshUser = builder.username;
          sshKey = config.age.secrets."builder-ssh-key".path;
        }

      );

}
