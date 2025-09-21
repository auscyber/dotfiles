{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.hmApps;
in
{
  options.auscybernix.hmApps = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Whether to link Home Manager apps to /Applications on macOS.";
  };

  config = lib.mkIf cfg {

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
        pathsToLink = "/Applications";
        paths =
          config.environment.systemPackages
          ++ (lib.concatMap (x: x.home.packages) (lib.attrsets.attrValues config.home-manager.users));
      }
    );
  };
}
