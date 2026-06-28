{ den, lib, ... }:
{
  den.aspects.darwin-hmApps = {
    darwin =
      { config, pkgs, ... }:
      {
        home-manager.sharedModules = [
          { targets.darwin.linkApps.enable = false; }
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
              ++ (lib.concatMap (x: x.home.packages) (
                lib.attrValues config.home-manager.users
              ));
          }
        );
      };
  };
}
