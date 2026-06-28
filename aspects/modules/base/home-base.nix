{ den, lib, ... }:
{
  den.aspects.home-base = {
    includes = [
      den.aspects.shell
      den.aspects.jujutsu
    ];

    homeManager =
      { config, pkgs, ... }:
      {
        manual.manpages.enable = true;

        nix.settings.extra-experimental-features = [
          "nix-command"
          "flakes"
        ];

        home.shell.enableFishIntegration = true;

        home.sessionVariables = {
          _JAVA_AWT_WM_NONREPARENTING = 1;
          WLR_NO_HARDWARE_CURSORS = 1;
        };

        home.extraBuilderCommands =
          let
            packagelist = pkgs.writeText "extra-builder-commands" (
              lib.concatMapAttrsStringSep "\n" (x: _: x) (
                lib.genAttrs' config.home.packages (x: {
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

  den.default.includes = [ den.aspects.home-base ];
}
