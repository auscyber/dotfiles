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
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in
lib.mkIf cfg.enable (
  lib.mkMerge [
    # ── Shared: register remote build machines ──────────────────────────────
    {
      nix.buildMachines =
        lib.flip lib.mapAttrsToList
          (lib.filterAttrs (
            name: x:
              name != systemIdentifier
              && (
                if isLinux then
                  (cfg.absoluteSpeedFactor * 0.75) > x.absoluteSpeedFactor
                else
                  cfg.absoluteSpeedFactor > x.absoluteSpeedFactor
              )
          ) flakeConfig.flake.auscybernix.builders.buildMachines)
          (
            name: builder: {
              protocol = "ssh-ng";
              hostName = builder.ipAddress;
              systems = builder.systems;
              publicHostKey = builder.publicHostKey;
              maxJobs = builder.maxJobs;
              speedFactor =
                if isLinux then
                  builder.absoluteSpeedFactor / cfg.absoluteSpeedFactor
                else
                  builder.speedFactor / cfg.absoluteSpeedFactor;
              supportedFeatures = builder.features;
              sshUser = builder.username;
              sshKey = config.age.secrets."builder-ssh-key".path;
            }
          );
    }

    # ── nix-darwin-only: register the builder user account ──────────────────
    (lib.mkIf (isDarwin && cfg.builderConfig.enable) {
      users.knownUsers = [ cfg.builderConfig.builderUser ];
      users.groups."com.apple.access_ssh".members = [ cfg.builderConfig.builderUser ];
      users.users."${cfg.builderConfig.builderUser}" = {
        uid = 3000;
      };
    })
  ]
)
