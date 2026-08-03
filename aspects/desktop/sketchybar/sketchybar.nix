{
  den,
  rootPath,
  ...
}:
{
  den.aspects.sketchybar = {
    darwin = { pkgs, ... }: {
      fonts.packages =
        with pkgs;
        [ sketchybar-app-font ]
        ++ (with nerd-fonts; [
          hack
          roboto-mono
        ]);
    };
    hmDarwin =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      let
        colors = config.stylix.base16Scheme;

        colourConfig = {
          background = colors.base00;
          black = colors.base01;
          selection = colors.base02;
          comment = colors.base03;
          darkgray = colors.base04;
          foreground = colors.base05;
          brightwhite = colors.base06;
          white = colors.base07;
          red = colors.base08;
          orange = colors.base09;
          yellow = colors.base0A;
          green = colors.base0B;
          cyan = colors.base0C;
          blue = colors.base0D;
          magenta = colors.base0E;
          brown = colors.base0F;
        };

        inherit (import ./_lua-modules.nix { inherit pkgs lib; }) mkColorsModule mkIconMapModule;
      in
      {
        home.file.".config/sketchybar" = {
          source = config.lib.file.linkLocalPath ../../../sketchybar;
          recursive = true;
        };

        programs.sketchybar = {
          service.enable = true;
          enable = true;
          configType = "lua";
          extraLuaPackages = luaPs: [
            (mkColorsModule colourConfig luaPs)
            (mkIconMapModule luaPs)
          ];
          extraPackages = with pkgs; [
            jq
            yq
            nowplaying-cli
          ];
        };
      };
    includes = [
      den.aspects.packages.sketchybar
      den.aspects.packages.sketchybar_app_font
    ];
  };
}
