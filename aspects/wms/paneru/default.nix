{
  inputs,
  lib,
  den,
  ...
}:
{
  den.aspects.paneru = {
    # Declared on the aspect, not the file: the partition generator reads
    # which aspect owns an input, and which platforms pull that aspect in.
    flake-file = _: {
      inputs.paneru.url = "github:auscyber/paneru?ref=add-lua-stuff";
      inputs.paneru.inputs.nixpkgs.follows = "nixpkgs";
      inputs.paneru.inputs.crane.follows = "crane";
    };

    #    includes = [ den.aspects.jankyborders ];
    overlays = {
      paneru = lib.optional (inputs ? paneru) inputs.paneru.overlays.default;
    };
    homeManager =
      {
        config,
        options,
        pkgs,
        lib,
        user,
        ...
      }:
      {
        imports = [ inputs.paneru.homeModules.paneru ];
        config =
          let
            # Paneru's own colour theme, independent of
            # `aspects/desktop/sketchybar/sketchybar.nix`'s copy so paneru's
            # `wm.lua` (which runs inside paneru's own Lua VM, not
            # sketchybar's) can `require("colors")` without reaching into the
            # sketchybar aspect's config-dir files. The module builders
            # themselves (`mkColorsModule`/`mkIconMapModule`) are shared —
            # both aspects push the same `colors`/`icon_map` shape onto a
            # sketchybar-Lua-compatible require path.
            colourConfig =
              let
                colors = config.stylix.base16Scheme;
              in
              {
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

            inherit (import ../../desktop/sketchybar/_lua-modules.nix { inherit pkgs lib; })
              mkColorsModule
              mkIconMapModule
              ;

            # kanata's sexpr parser has no string escapes, so a nested-quote
            # osascript can't live inline in the keybind config.
            killFocus = pkgs.writeShellScript "paneru-kill-focus" ''
              exec /usr/bin/osascript -e 'tell application "System Events" to keystroke "w" using command down'
            '';

            # Native paneru hotkeys for virtual-workspace navigation, as a
            # fallback path alongside the kanata layer (which drives the bare
            # arrow keys via `send-cmd`). alt+up/down move between rows;
            # alt+shift+up/down carry the focused window and follow; alt+<n>
            # jumps to a numbered row (creating it if absent). Keys are
            # `window_<argv, "_"-joined>` -> chord, the same shape paneru.toml's
            # `[bindings]` table used; rendered below as `paneru.bind` calls in
            # paneru's own init.lua instead, so the daemon's native embedded
            # runtime carries them rather than the TOML declarative table.
            bindings = {
              #              window_virtual_north = "alt - up";
              #              window_virtual_south = "alt - down";
              #              window_virtualmove_north = "alt + shift - up";
              #              window_virtualmove_south = "alt + shift - down";
              #              window_virtualnum_1 = "alt - 1";
              #              window_virtualnum_2 = "alt - 2";
              #              window_virtualnum_3 = "alt - 3";
              #              window_virtualnum_4 = "alt - 4";
              #              window_virtualnum_5 = "alt - 5";
              #              window_virtualnum_6 = "alt - 6";
              #              window_virtualnum_7 = "alt - 7";
              #              window_virtualnum_8 = "alt - 8";
              #              window_virtualnum_9 = "alt - 9";
            };

            # `paneru.bind(chord, "argv string")` per binding, discovered by
            # paneru at `${XDG_CONFIG_HOME:-~/.config}/paneru/init.lua` (or
            # `~/.paneru.lua` without XDG). The command name is the binding key
            # with underscores turned back into the argv spaces `paneru.run`
            # expects (`window_virtualnum_1` -> `"window virtualnum 1"`).
            paneruInitLua = pkgs.writeText "paneru-init.lua" ''
              -- Generated from aspects/wms/paneru/default.nix (services.paneru
              -- bindings) — do not edit by hand.
              ${lib.optionalString (options.programs ? sketchybar) ''
                -- Drives sketchybar directly from paneru's own event loop
                -- (services.paneru.extraLuaPackages, below); see sketchybar/wm.lua.
                require("wm")
              ''}
            '';
          in
          lib.mkMerge [
            (lib.optionalAttrs (options.programs ? kanata) {
              programs.kanata.extraPackages = [ config.services.paneru.finalPackage ];
              programs.kanata.extraConfigPaths = [
                (pkgs.writeText "paneru-keybinds"
                  # commonlisp
                  ''
                    (defalias
                      ;; paneru has no "toggle space activated"; restart is the
                      ;; closest best-effort analog to rift's enable_spaces.
                      enable_spaces (t! runasuser "paneru send-cmd restart")

                      ;; paneru has no scratchpads; best-effort is to just open
                      ;; the app (paneru will manage/float it per window rules).
                      toggle_discord_scratchpad (t! runasuser "open -a ~/Nix\ Apps/Discord.app")
                      toggle_fantastical_scratchpad (t! runasuser "open -a Fantastical.app")
                      toggle_beeper_scratchpad (t! runasuser "open -a 'Beeper\ Desktop'")
                      toggle_music_scratchpad (t! runasuser "open -a 'Tidal'")
                      toggle_1password_scratchpad (t! runasuser "open -a '1Password'")

                      minimise (t! runasuser "yabai -m window --minimize")
                      switch-focus (t! runasuser "paneru send-cmd window focus east")
                      reverse-switch-focus (t! runasuser "paneru send-cmd window focus west")

                      ;; paneru has no close-window command; best-effort Cmd+W.
                      kill-focus (t! runasuser "${killFocus}")

                      1s (t! runasuser "paneru send-cmd window virtualnum 1")
                      2s (t! runasuser "paneru send-cmd window virtualnum 2")
                      3s (t! runasuser "paneru send-cmd window virtualnum 3")
                      4s (t! runasuser "paneru send-cmd window virtualnum 4")
                      5s (t! runasuser "paneru send-cmd window virtualnum 5")
                      6s (t! runasuser "paneru send-cmd window virtualnum 6")
                      7s (t! runasuser "paneru send-cmd window virtualnum 7")
                      8s (t! runasuser "paneru send-cmd window virtualnum 8")
                      9s (t! runasuser "paneru send-cmd window virtualnum 9")
                      10s (t! runasuser "paneru send-cmd window virtualnum 10")

                      1m (t! runasuser "paneru send-cmd window virtualmovenum 1")
                      2m (t! runasuser "paneru send-cmd window virtualmovenum 2")
                      3m (t! runasuser "paneru send-cmd window virtualmovenum 3")
                      4m (t! runasuser "paneru send-cmd window virtualmovenum 4")
                      5m (t! runasuser "paneru send-cmd window virtualmovenum 5")
                      6m (t! runasuser "paneru send-cmd window virtualmovenum 6")
                      7m (t! runasuser "paneru send-cmd window virtualmovenum 7")
                      8m (t! runasuser "paneru send-cmd window virtualmovenum 8")
                      9m (t! runasuser "paneru send-cmd window virtualmovenum 9")
                      10m (t! runasuser "paneru send-cmd window virtualmovenum 10")

                      shiftUp (t! runasuser "paneru send-cmd window swap north")
                      shiftDown (t! runasuser "paneru send-cmd window swap south")
                      shiftLeft (t! runasuser "paneru send-cmd window swap west")
                      shiftRight (t! runasuser "paneru send-cmd window swap east")

                      focusUp (t! runasuser "paneru send-cmd window focus north")
                      focusDown (t! runasuser "paneru send-cmd window focus south")
                      focusLeft (t! runasuser "paneru send-cmd window focus west")
                      focusRight (t! runasuser "paneru send-cmd window focus east"))
                  ''
                )
              ];
            })
            (lib.optionalAttrs (options.programs ? sketchybar) {
              # sketchybar stays WM-agnostic: the only thing it needs from
              # paneru is the CLI on PATH, for the `click_script`s wm.lua
              # attaches to bar items (`paneru send-cmd ...`). Everything else
              # — the bar items themselves, their content, their colours — is
              # driven from paneru's own Lua runtime via
              # `services.paneru.extraLuaPackages` below, not pushed into
              # sketchybar's process.
              programs.sketchybar.extraPackages = [ config.services.paneru.finalPackage ];
            })
            {
              services.paneru = {
                enable = true;
                package = pkgs.paneru;
                # SbarLua (nixpkgs `sbarlua`) is only built against Lua 5.5
                # (`pkgs/by-name/sb/sbarlua/package.nix` hardcodes
                # `lua55Packages`); point `extraLuaPackages`' resolution at it
                # to match, per `services.paneru.lua`'s own doc comment
                # ("override this directly if you need extraLuaPackages to
                # resolve against a different interpreter than package was
                # built with").
                lua = lib.mkIf (options.programs ? sketchybar) pkgs.lua5_5;
                luaConfig.enable = true;
                extraPackages = [ pkgs.sketchybar ];
                extraLuaPackages = lib.mkIf (options.programs ? sketchybar) (luaPs: [
                  pkgs.sbarlua
                  (mkColorsModule colourConfig luaPs)
                  (mkIconMapModule luaPs)
                  (luaPs.toLuaModule (
                    pkgs.runCommandLocal "paneru-wm" { } ''
                      install -Dm644 ${./sketchybar/wm.lua} "$out/share/lua/${luaPs.lua.luaversion}/wm.lua"
                    ''
                  ))
                ]);
                settings = {
                  default_workspaces = 1;
                  inherit bindings;
                  #bindings = { };
                  decorations = {
                    active.border = {
                      enabled = true;
                      color = config.stylix.base16Scheme.base03;
                      width = 2.0;
                      radius = 12.0;
                    };
                    inactive.dim = {
                      opacity = 0.3;
                      color = "#000000";
                    };
                    inactive.border = {
                      enabled = true;
                      color = config.stylix.base16Scheme.base0D;
                      opacity = 0.5;
                      width = 2.0;
                      radius = 12.0;
                    };

                    # Both default to true
                    workspace_menu_status = false;
                    workspace_popup_status = false;

                  };
                  options = {
                    focus_follows_mouse = false;
                    mouse_follows_focus = false;
                    virtual_workspace_animations = true;
                    # rift ran with animate=false; paneru disables animation with
                    # a high animation_speed (8-20 comfortable, higher ~= off).
                    animation_speed = 20.0;
                    # false: keep a virtual-workspace row alive even when empty.
                    # With reap=true a freshly-switched-to (empty) row is deleted
                    # instantly, so nothing ever shows in the popup/menubar/bar.
                    reap_empty_workspaces = true;
                  };
                  # rift's layout.gaps.outer.
                  padding = {
                    top = 15;
                    left = 20;
                    right = 10;
                    bottom = 5;
                  };
                  # rift's 3-finger gestures. vertical=true lets a 3-finger
                  # up/down swipe switch virtual-workspace rows (paneru reads
                  # the trackpad directly). macOS's own 3-finger vertical swipe
                  # (Mission Control / App Expose) is disabled below so it does
                  # not fire alongside paneru.
                  swipe.gesture = {
                    fingers_count = 3;
                    direction = "Natural";
                    vertical = true;
                  };
                };
              };

              # paneru's own embedded Lua config (paneru.bind/.on/.window/...),
              # generated from `bindings` above rather than hand-written.
              #              xdg.configFile."paneru/init.lua" = lib.mkIf config.xdg.enable { source = paneruInitLua; };
              home.file.".paneru.lua".source = config.lib.file.linkLocalPath ./sketchybar/wm.lua;
              # Stop macOS from also acting on a 3-finger vertical swipe
              # (Mission Control up / App Expose down) so paneru's vertical
              # swipe -> virtual-workspace switch isn't shadowed by it. 0
              # disables the gesture; both the built-in and Bluetooth trackpad
              # domains carry the key. Takes effect on next login (Dock reads
              # these at startup).
              targets.darwin.defaults = {
                "com.apple.AppleMultitouchTrackpad".TrackpadThreeFingerVertSwipeGesture = 0;
                "com.apple.driver.AppleBluetoothMultitouch.trackpad".TrackpadThreeFingerVertSwipeGesture = 0;
              };
            }
          ];
      };
  };
}
