{ den, rootPath, ... }:

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
    homeManager =
      {
        config,
        pkgs,
        sources,
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

        # toLua emits a *value*, so prepend `return` to make it a loadable module
        sketchybar-app-fontModule =
          luaPs:
          luaPs.toLuaModule (
            pkgs.runCommand "sketchybar-app-fontModule" { } ''
              mkdir -p $out
              install -Dm644 ${pkgs.sketchybar-app-font}/lib/sketchybar-app-font/icon_map.lua "$out/share/lua/${luaPs.lua.luaversion}/icon_map.lua"
            ''
          );

      in
      {
        home.file.".config/sketchybar" = {
          source = config.lib.file.linkLocalPath "${rootPath}/sketchybar";
          recursive = true;
        };

        programs.sketchybar = {
          service.enable = true;
          enable = true;
          luaPackage = pkgs.lua5_5;
          configType = "lua";
          extraLuaPackages = luaPs: [
            (luaPs.callPackage ./_colorPackage.nix { config = colourConfig; })
            (sketchybar-app-fontModule luaPs)
          ];
          extraPackages = with pkgs; [
            jq
            yq
            nowplaying-cli
            rift
          ];
        };
      };
    includes = [
      den.aspects.packages.sketchybar
      den.aspects.packages.sketchybar_app_font
    ];
  };
}
