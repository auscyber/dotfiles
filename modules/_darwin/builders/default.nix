{
  config,
  lib,
  pkgs,
  flakeConfig,
  system,
  systemIdentifier,
  ...
}:
let
  cfg = config.auscybernix.nix.builders;
in
{

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        nix.buildMachines =
          lib.flip lib.mapAttrsToList
            (lib.filterAttrs (
              name: x: name != systemIdentifier && cfg.absoluteSpeedFactor > x.absoluteSpeedFactor
            ) flakeConfig.flake.auscybernix.builders.buildMachines)
            (
              name: builder: {
                protocol = "ssh-ng";
                hostName = builder.ipAddress;
                systems = builder.systems;
                maxJobs = builder.maxJobs;
                speedFactor = builder.speedFactor / cfg.absoluteSpeedFactor;
                publicHostKey = builder.publicHostKey;
                supportedFeatures = builder.features;
                sshUser = builder.username;
                sshKey = config.age.secrets."builder-ssh-key".path;
              }

            );
      }
      (lib.mkIf cfg.builderConfig.enable {
        users.knownUsers = [ cfg.builderConfig.builderUser ];
        users.groups."com.apple.access_ssh".members = [ cfg.builderConfig.builderUser ];
        users.users."${cfg.builderConfig.builderUser}" = {
          uid = 3000;
        };
      })

    ]
  );

}
