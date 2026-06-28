{ den, __findFile, ... }:
{
  den.aspects.jankyborders = {
    includes = [ <packages/jankyborders> ];
    homeDarwin = { config, ... }: {
      services.jankyborders = {
        enable = true;
        settings = {
          active_color = "0xff${config.stylix.base16Scheme.base03}";
          inactive_color = "0xff${config.stylix.base16Scheme.base0D}";
          style = "round";
          #blur_radius = 5.0;
          width = 6.0;
          #        ax_focus = true;
        };
      };
    };
  };
}
