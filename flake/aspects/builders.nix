{ den, ... }:
{
  # ── Builders aspect ───────────────────────────────────────────────────────────
  # Distributed Nix builds: declares the shared options (from modules/common/builders),
  # then adds platform-specific nix.buildMachines configuration for NixOS and Darwin.
  # Include in host aspects that participate in distributed builds.
  den.aspects.builders = {
    nixos =
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
        # Re-use the shared option declarations + common config (age key gen,
        # nix.distributedBuilds, builder user setup).
        imports = [ ../../modules/common/builders ];

        config = lib.mkIf cfg.enable {
          nix.buildMachines =
            lib.flip lib.mapAttrsToList
              (lib.filterAttrs
                (name: x:
                  name != systemIdentifier
                  && (cfg.absoluteSpeedFactor * 0.75) > x.absoluteSpeedFactor)
                flakeConfig.flake.auscybernix.builders.buildMachines)
              (name: builder: {
                protocol = "ssh-ng";
                hostName = builder.ipAddress;
                systems = builder.systems;
                publicHostKey = builder.publicHostKey;
                maxJobs = builder.maxJobs;
                speedFactor = builder.absoluteSpeedFactor / cfg.absoluteSpeedFactor;
                supportedFeatures = builder.features;
                sshUser = builder.username;
                sshKey = config.age.secrets."builder-ssh-key".path;
              });
        };
      };

    darwin =
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
        imports = [ ../../modules/common/builders ];

        config = lib.mkIf cfg.enable (
          lib.mkMerge [
            {
              nix.buildMachines =
                lib.flip lib.mapAttrsToList
                  (lib.filterAttrs
                    (name: x:
                      name != systemIdentifier
                      && cfg.absoluteSpeedFactor > x.absoluteSpeedFactor)
                    flakeConfig.flake.auscybernix.builders.buildMachines)
                  (name: builder: {
                    protocol = "ssh-ng";
                    hostName = builder.ipAddress;
                    systems = builder.systems;
                    maxJobs = builder.maxJobs;
                    speedFactor = builder.speedFactor / cfg.absoluteSpeedFactor;
                    publicHostKey = builder.publicHostKey;
                    supportedFeatures = builder.features;
                    sshUser = builder.username;
                    sshKey = config.age.secrets."builder-ssh-key".path;
                  });
            }
            (lib.mkIf cfg.builderConfig.enable {
              users.knownUsers = [ cfg.builderConfig.builderUser ];
              users.groups."com.apple.access_ssh".members = [ cfg.builderConfig.builderUser ];
              users.users."${cfg.builderConfig.builderUser}".uid = 3000;
            })
          ]
        );
      };
  };
}
