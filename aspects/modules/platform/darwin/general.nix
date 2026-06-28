{ den, lib, ... }:
{
  den.aspects.darwin-general = {
    darwin =
      { config, pkgs, ... }:
      {
        stylix.enableReleaseChecks = false;

        environment.shellAliases.re = "nh darwin switch";

        nix.channel.enable = false;

        system.systemBuilderCommands =
          let
            packagelist = pkgs.writeText "extra-builder-commands" (
              lib.concatMapAttrsStringSep "\n" (x: _: x) (
                lib.genAttrs' config.environment.systemPackages (x: {
                  name = x.name;
                  value = x;
                })
              )
            );
          in
          ''
            ln -s ${packagelist} $out/packagelist
          '';
      };
  };
}
