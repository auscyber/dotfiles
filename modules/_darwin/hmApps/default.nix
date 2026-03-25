{
  config,
  pkgs,
  lib,
  ...
}:
let
  hmApps = config.auscybernix.hmApps;
in
{
  options.auscybernix = {

    hmApps = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to link Home Manager apps to /Applications on macOS.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf hmApps {

      home-manager.sharedModules = [
        (
          { config, pkgs, ... }:
          {

            targets.darwin.linkApps.enable = false;
          }
        )
      ];

      system.build.applications = lib.mkForce (
        pkgs.buildEnv {
          name = "system-applications";
          pathsToLink = [
            "/Applications"
            "/Library/Application Support"
          ];
          paths =
            config.environment.systemPackages
            ++ (lib.concatMap (x: x.home.packages) (lib.attrsets.attrValues config.home-manager.users));
        }
      );
    })

  ];
}
