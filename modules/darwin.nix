{
  config,
  pkgs,
  lib,
  ...
}:
{

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
}
