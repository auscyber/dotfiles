{
  den,
  rootPath,
  config,
  ...
}:
let
  # Alias for the flake-parts top-level `config`, captured here before
  # `hmDarwin` below binds its own (home-manager) `config` of the same name --
  # which would otherwise shadow this one and put `codesign.mkSignedWrapper`
  # out of reach inside it.
  flakeParts = config;
in
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

        # `programs.sketchybar.finalPackage` (home-manager's own module) wraps
        # `programs.sketchybar.package` a *second* time -- for the
        # `extraPackages`/`extraLuaPackages` set below -- through a plain
        # `symlinkJoin` + `wrapProgram` that has never heard of
        # `mkSignedWrapper`. `programs.sketchybar.package` is left unsigned
        # (see the `signed` list in aspects/darwin/codesign.nix) so that wrap
        # is the *only* one `finalPackage` carries: a single hidden-sibling
        # layer over the real Mach-O, the shape `mkSignedWrapper` already
        # handles. Signing it here, rather than through codesign's own
        # overlay, is what makes the actual launched entry point (below)
        # stable across rebuilds instead of the raw Mach-O two hops further
        # in. Identical to the call in `aspects/darwin/codesign.nix`'s
        # activation script (same inputs -> same derivation), which is what
        # plants it.
        sketchybarSigned = flakeParts.flake.lib.codesign.mkSignedWrapper pkgs {
          package = config.programs.sketchybar.finalPackage;
          entitlements = flakeParts.flake.lib.codesign.entitlementsFor.sketchybar or { };
        };
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

        # home-manager's own module (modules/programs/sketchybar.nix) points
        # this launchd job's `Program` at `programs.sketchybar.finalPackage`
        # directly, which is the UNSIGNED wrapProgram wrap (see
        # `sketchybarSigned` above). Forced to the signed equivalent instead,
        # so the process launchd actually execs bottoms out at
        # `${trustedDir}/sketchybar` rather than a raw, rebuild-varying store
        # path -- TCC's Accessibility/Screen-Recording grants are worthless to
        # a client whose path never survives a rebuild. Only `Program` is
        # overridden; `KeepAlive` and the rest keep whatever that module set.
        launchd.agents.sketchybar.config.Program = lib.mkForce (lib.getExe sketchybarSigned);
      };
    includes = [
      den.aspects.packages.sketchybar
      den.aspects.packages.sketchybar_app_font
    ];
  };
}
