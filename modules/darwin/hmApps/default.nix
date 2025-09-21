{
  config,
  pkgs,
  lib,
  ...
}:
let
  hmApps = config.auscybernix.hmApps;
  libraryPaths = config.auscybernix.libraryPaths;
in
{
  options.auscybernix = {
    libraryPaths = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
    hmApps = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to link Home Manager apps to /Applications on macOS.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf hmApps {
      environment.systemPackages = with pkgs; [
        (runCommand "hm-apps" { } ''
          mkdir -p "$out/Library/Application Support"
          echo hi > "$out/Library/Application Support/.placeholder"
        '')
      ];

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
            "/Library"
          ];
          paths =
            config.environment.systemPackages
            ++ (lib.concatMap (x: x.home.packages) (lib.attrsets.attrValues config.home-manager.users));
        }
      );
    })

  ];
}
