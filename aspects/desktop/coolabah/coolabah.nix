{
  inputs,
  lib,
  den,
  config,
  ...
}:
let
  # Alias for the flake-parts top-level `config`, captured here before
  # `homeManager` below binds its own (home-manager) `config` of the same name --
  # which would otherwise shadow this one and put `codesign.mkSignedWrapper`
  # out of reach inside it.
  flakeParts = config;
in
{
  den.aspects.coolabah = {
    # Only `gui` is declared -- `darwin` is inferred from the hosts that
    # include it, so this lands in darwin-gui without restating the platform.
    layers = [ "gui" ];

    # Declared on the aspect, not the file: the partition generator reads which
    # aspect owns an input, and which platforms pull that aspect in. coolabah
    # is a window-server daemon and builds for darwin only, so this lives in the
    # `darwin` bucket (../../../partition-map.nix).
    # The project renamed itself coolabah -- package, `programs.coolabah`,
    # `coolabahrc`, `coolabah-lua` -- but the GitHub repository is still
    # `auscyber/rsbar`, so the URL keeps the old name while everything this
    # aspect touches uses the new one.
    inputs.coolabah.url = "github:auscyber/rsbar";
    inputs.coolabah.inputs.nixpkgs.follows = "nixpkgs";
    inputs.coolabah.inputs.crane.follows = "crane";
    inputs.coolabah.inputs.flake-parts.follows = "flake-parts";
    inputs.coolabah.inputs.rust-overlay.follows = "rust-overlay";

    overlays = {
      # Built from coolabah's `nix/package.nix` directly rather than taken from
      # its flake's `packages.<system>.coolabah`.
      #
      # That output cannot be evaluated: coolabah's flake.nix builds its toolchain
      # with `(import inputs.rust-overlay { inherit pkgs; }).rust-bin`, and
      # rust-overlay's default.nix is a bare `final: prev:` overlay -- applying
      # it to one attrset leaves a *function*, so selecting `.rust-bin` off it
      # fails with "expected a set but found a function". Nothing here can fix
      # that from the outside, and it only bites once something forces the
      # derivation (a `.name` read does not), which is why it surfaces at
      # activation rather than at eval.
      #
      # `nix/package.nix` itself is fine -- it takes `craneLib` and `src` as
      # arguments. So supply them from this flake's own `crane` and
      # `rust-overlay`, which are root inputs the partition already sees, and
      # skip the broken flake output entirely. coolabah's home-manager module
      # defaults `package` to that same output, so ./. sets
      # `programs.coolabah.package = pkgs.coolabah` and the default is never
      # forced.
      coolabah = lib.optional (inputs ? coolabah) (
        final: _prev: {
          coolabah = final.callPackage "${inputs.coolabah}/nix/package.nix" {
            craneLib = (inputs.crane.mkLib final).overrideToolchain (
              p: (p.extend (import inputs.rust-overlay)).rust-bin.stable.latest.default
            );
            src = inputs.coolabah;
          };
        }
      );
    };

    darwin = { pkgs, ... }: {
      fonts.packages =
        with pkgs;
        [ sketchybar-app-font ]
        ++ (with nerd-fonts; [
          hack
          roboto-mono
        ]);
    };

    # `homeManager`, not `hmDarwin`: a den class body routed through `hmDarwin`
    # is merged as a bare config fragment, so it cannot carry the `imports` that
    # brings coolabah's own home-manager module in. Same shape den.aspects.paneru
    # uses, and safe for the same reason -- coolabah builds for darwin only and
    # is only ever included on a darwin host.
    homeManager =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        imports = [ inputs.coolabah.homeManagerModules.coolabah ];

        config =
          let
            colors = config.stylix.base16Scheme;

            # The same stylix-derived shape aspects/desktop/sketchybar and
            # aspects/wms/paneru build, fed to the same generator -- one theme
            # for whichever bar a host runs.
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

            # The `*File` half of the shared builders, not `mk*Module`:
            # sketchybar takes these as Lua *packages* through
            # `extraLuaPackages`, coolabah as plain files next to `coolabahrc`.
            # Same content either way, so `require("colors")` in ../../../
            # sketchybar/bar.lua resolves identically under both bars.
            inherit (import ../sketchybar/_lua-modules.nix { inherit pkgs lib; })
              colorsFile
              iconMapFile
              ;

            # ../../../sketchybar is written against SketchyBar's CLI --
            # `sbar.exec("sketchybar --subscribe ...")` in its init.lua, and
            # `sketchybar --set "$NAME" ...` in every plugins/*.sh. coolabah's
            # CLI is argument-compatible but not named that, so rather than
            # rewriting a shared config per bar, the old name is put on the PATH
            # coolabah hands its children (`coolabah::script::child_path`, which
            # keeps the daemon's inherited PATH) pointing at the new one.
            sketchybarCompat = pkgs.writeShellScriptBin "sketchybar" ''
              exec coolabah "$@"
            '';

            # Every top-level entry of ../../../sketchybar, linked individually
            # rather than as one recursive directory: the config directory is
            # also where the generated `colors.lua`/`icon_map.lua` and the WM
            # provider's files (aspects/wms/paneru drops `wm.lua`,
            # `paneru_bar.lua` and `paneru.so` here) have to land, and a single
            # symlink for the whole directory leaves nowhere to put them.
            # `linkLocalPath` keeps each one pointing at the working tree, so
            # editing the bar config is still a reload rather than a rebuild --
            # the property aspects/desktop/sketchybar/sketchybar.nix has.
            #
            # `sketchybarrc` is left out because coolabah's entry point is
            # `coolabahrc`, generated from `programs.coolabah.config` below;
            # dotfiles because a `.gitignore` in a config directory is clutter
            # that coolabah would never read.
            sharedConfig =
              lib.mapAttrs'
                (
                  name: _:
                  lib.nameValuePair "coolabah/${name}" {
                    source = config.lib.file.linkLocalPath (../../../sketchybar + "/${name}");
                  }
                )
                (
                  lib.filterAttrs (name: _: name != "sketchybarrc" && !(lib.hasPrefix "." name)) (
                    builtins.readDir ../../../sketchybar
                  )
                );

            # `programs.coolabah.finalPackage` (coolabah's own home-manager
            # module, `nix/hm-module.nix`) wraps `programs.coolabah.package` a
            # second time -- for `extraPackages` on PATH -- through a plain
            # `symlinkJoin` + `wrapProgram` that has never heard of
            # `mkSignedWrapper`. Same shape, and for the same reason, as
            # sketchybar and paneru: `pkgs.coolabah` is deliberately absent from
            # the `signed` list in ../../darwin/codesign.nix so that wrap is
            # the ONLY one `finalPackage` carries, and it is signed here
            # instead. coolabah drives SkyLight and the Accessibility API, so
            # its TCC grant is worth exactly as much as the stability of the
            # path launchd execs.
            coolabahSigned = flakeParts.flake.lib.codesign.mkSignedWrapper pkgs {
              package = config.programs.coolabah.finalPackage;
              entitlements = flakeParts.flake.lib.codesign.entitlementsFor.coolabah or { };
            };
          in
          {
            programs.coolabah = {
              enable = true;
              package = pkgs.coolabah;
              configType = "lua";
              service.enable = true;
              extraPackages = [
                sketchybarCompat
              ]
              ++ (with pkgs; [
                jq
                yq
                nowplaying-cli
              ]);

              # coolabah runs `coolabahrc` as a subprocess with the config's
              # own directory appended to `package.path`/`package.cpath` (see
              # `coolabah_lua::host::add_search_paths`), so every `require`
              # below resolves to a file in that directory -- there is no
              # `extraLuaPackages` to push a nixpkgs Lua package onto, the way
              # sketchybar's module has.
              #
              # This is ../../../sketchybar/sketchybarrc, and deliberately
              # nothing more: the bar itself -- `bar`, `default`, `items.*` and
              # the `wm` provider -- is that same shared tree, linked next to
              # this file by `sharedConfig` above. coolabah binds its API to
              # `sketchybar` and `sbar` as well as its own name, so the tree
              # runs unmodified; only the entry point differs, because coolabah
              # looks for `coolabahrc` and SketchyBar for `sketchybarrc`.
              config =
                # lua
                ''
                  local home = os.getenv("HOME") or ""
                  local config_dir = os.getenv("CONFIG_DIR") or (home .. "/.config/coolabah")
                  package.path = package.path
                  	.. ";" .. config_dir .. "/?.lua"
                  	.. ";" .. config_dir .. "/?/init.lua"

                  require("helpers")
                  require("init")
                '';
            };

            # The theme and the app-icon lookup, as files in the config
            # directory. `colors` is generated from stylix
            # (`config.stylix.base16Scheme` -> `colourConfig` above), so the bar
            # follows the system theme instead of carrying its own copy;
            # `icon_map` comes straight out of `pkgs.sketchybar-app-font`
            # (den.aspects.packages.sketchybar_app_font, included below).
            # ../../../sketchybar/bar.lua requires the first and
            # aspects/wms/paneru/sketchybar/paneru-bar.lua the second.
            xdg.configFile = sharedConfig // {
              "coolabah/colors.lua".source = colorsFile colourConfig;
              "coolabah/icon_map.lua".source = iconMapFile;
            };

            # coolabah's own home-manager module points this launchd job's
            # `Program` at `programs.coolabah.finalPackage` directly, which is the
            # UNSIGNED wrapProgram wrap (see `coolabahSigned` above). Routed
            # through `wrapperd-wait` instead, so the job blocks until `wrapperd`
            # has planted the wrappers and then execs the signed copy at
            # `${trustedDir}/coolabah` -- TCC's Accessibility/Screen-Recording
            # grants are worthless to a client whose path never survives a
            # rebuild, or is missing after a reboot (see
            # `aspects/darwin/wrapperd.nix`). `KeepAlive` and the rest keep
            # whatever that module set.
            launchd.agents.coolabah.config = flakeParts.flake.lib.wrapperd.waitConfig {
              inherit pkgs;
              label = "coolabah";
              name = "coolabah";
            };
          };
      };

    includes = [ den.aspects.packages.sketchybar_app_font ];
  };
}
