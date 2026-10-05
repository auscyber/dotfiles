{
  den,
  lib,
  ...
}:
{
  den.aspects.darwin-hmApps = {
    darwin =
      {
        config,
        pkgs,
        ...
      }:
      {
        home-manager.sharedModules = [
          { targets.darwin.linkApps.enable = false; }
        ];

        # ── READ `extraAppPaths`, DO NOT ADD TO IT ──
        #
        # `pathsToLink` had `/Library/Application Support` hard-coded beside
        # `/Applications`, which made `environment.extraAppPaths` a DEAD
        # CHANNEL: ../../patches/darwin/karabiner.patch adds that option and
        # builds the env as `[ "/Applications" ] ++ extraAppPaths`, and the
        # `mkForce` below replaced that env wholesale -- so
        # `services.karabiner-dk`'s own declared path
        # (`/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice`,
        # measured as present in `environment.extraAppPaths`) was never in the
        # env and its DriverKit bundle was never linked.
        #
        # ⚠ AND THE BROAD PATH IS NOT ADDED BACK, DELIBERATELY. That patch's
        # activation script runs, per entry of `extraAppPaths`:
        #
        #   rsync --archive --delete "$env/<path>/" "<path>"
        #
        # `--delete` makes the destination NIX-OWNED: anything there that is
        # not in the store env is removed. For the deep per-extension path that
        # is correct and contained. For `/Library/Application Support` it would
        # delete every other application's support data on the machine. So an
        # entry in that list is a specific directory we own, never a system
        # tree we merely want to write inside.
        #
        # The hard-coded broad entry was also INERT: the activation script only
        # copies `/Applications` plus the `extraAppPaths` loop, so a subtree in
        # `pathsToLink` that no entry names was built into the env and never
        # installed anywhere.
        system.build.applications = lib.mkForce (
          pkgs.buildEnv {
            name = "system-applications";
            pathsToLink = [ "/Applications" ] ++ config.environment.extraAppPaths;
            paths =
              config.environment.systemPackages
              ++ (lib.concatMap (x: x.home.packages) (lib.attrValues config.home-manager.users));
          }
        );
      };
  };
}
