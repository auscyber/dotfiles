{ lib, ... }:
{

  den.schema.user = {
    options.wallpaper = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = ../../../backgrounds/phoebebridgers-2.jpg;
      description = "Wallpaper image path";
    };

  };

  den.default = {
    provides.to-hosts =
      { user, ... }:
      {
        gui.darwin = lib.mkIf (user ? wallpaper) {

          programs.desktoppr.enable = true;
          programs.desktoppr.image = user.wallpaper;

        };
      };

  };

}
