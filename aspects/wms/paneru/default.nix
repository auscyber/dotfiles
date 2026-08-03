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
            # `aspects/desktop/sketchybar/sketchybar.nix`'s copy so the drawing
            # code paneru loads (`paneru_bar`, in paneru's own Lua VM rather
            # than sketchybar's) can `require("colors")` without reaching into
            # the sketchybar aspect's config-dir files. The module builders
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

            # A single .lua file as a Lua module named `name`, for either host's
            # `extraLuaPackages` (paneru's and sketchybar's take the same
            # `luaPs: [ package ]` shape). `paneru_bar` goes onto both paths —
            # the two processes draw the same items with the same code.
            mkLuaFileModule =
              name: file: luaPs:
              luaPs.toLuaModule (
                pkgs.runCommandLocal "paneru-lua-${name}" { } ''
                  install -Dm644 ${file} "$out/share/lua/${luaPs.lua.luaversion}/${name}.lua"
                ''
              );

            # kanata's sexpr parser has no string escapes, so a nested-quote
            # osascript can't live inline in the keybind config.
            killFocus = pkgs.writeShellScript "paneru-kill-focus" ''
              exec /usr/bin/osascript -e 'tell application "System Events" to keystroke "w" using command down'
            '';

            # Named scratchpads, the five den.aspects.rift declared as
            # `app_rules[].scratchpad`. paneru has no scratchpad primitive, so
            # these are implemented in Lua on top of its window-set API —
            # ./scratchpad.lua, a port of xmonad's NamedScratchpad. It diverges
            # from the worked example in paneru's own CONFIGURATION.md on where
            # a hidden pad goes — off the side of the display rather than onto
            # an NSP workspace, for the reason set out at the top of that file.
            # Declared here as plain data:
            # `bundle`/`app`/`title` are Rust `regex` patterns compiled into
            # predicates on the Lua side, so the whole table survives
            # `lib.generators.toLua` (a `paneru.match{...}` call would not).
            #
            # `float` is a rect in *fractions of the display* (paneru's
            # RelativeRect), which is also what makes a pad an overlay rather
            # than another window in the tiling strip. Floats have to be written
            # with a decimal point or paneru's deserializer sees an integer.
            #
            # `key` is bound with `paneru.bind`, not through kanata's
            # `send-cmd`: `Command::Lua` has no argv encoding, so no Lua
            # callback is reachable over paneru's socket. See the kanata aliases
            # below for how the chord gets there.
            scratchpadFloat = {
              x = 0.15;
              y = 0.1;
              width = 0.7;
              height = 0.8;
            };
            scratchpads = lib.mapAttrs (_: pad: { float = scratchpadFloat; } // pad) {
              # Bundle ids are what `paneru query state` reports for a live
              # window, not what the vendor's marketing name suggests: a
              # pattern that matches nothing sends every toggle down the spawn
              # branch, where `open -a` re-activates the already-running app
              # and the pad silently never moves. Check a new one against the
              # running window rather than guessing.
              #
              # 1Password makes itself frontmost every minute or so on its own.
              # ./scratchpad.lua reads a pad taking the focus as a summon, so it
              # comes out at its rect rather than sitting off screen with the
              # focus on it; alt+shift-p puts it back.
              "1password" = {
                bundle = "com\\.1password\\.1password";
                spawn = "open -a '1Password'";
                key = "alt + shift - p";
              };
              beeper = {
                bundle = "com\\.automattic\\.beeper\\.desktop";
                spawn = "open -a 'Beeper Desktop'";
                key = "alt + shift - b";
              };
              discord = {
                bundle = "com\\.hnc\\.Discord";
                spawn = "open -a 'Discord'";
                key = "alt + shift - d";
              };
              fantastical = {
                bundle = "com\\.flexibits\\.fantastical2\\.mac";
                spawn = "open -a 'Fantastical'";
                key = "alt + shift - f";
              };
              music = {
                bundle = "com\\.tidal\\.desktop";
                spawn = "open -a 'Tidal'";
                key = "alt + shift - s";
              };
            };

            # No stash workspace: a hidden pad stays floating and is parked off
            # the side of its display instead. ./scratchpad.lua explains why the
            # NSP-workspace port cannot work here — sinking a pad and shifting
            # it in one returned set flush together, and `window_managed_trigger`
            # pins the window at its float rect before the move lands.
            scratchpadSpec = {
              hide_on_focus_loss = true;
              # Every toggle decision through `paneru.log` (/tmp/paneru.err.log
              # under the launchd agent). On because a pad that matches nothing
              # is otherwise indistinguishable from a dead keybind: both fall
              # into the spawn branch and do nothing you can see.
              debug = true;
              order = lib.attrNames scratchpads;
              pads = scratchpads;
            };

            # Native paneru hotkeys for virtual-workspace navigation, as a
            # fallback path alongside the kanata layer (which drives the bare
            # arrow keys via `send-cmd`). alt+up/down move between rows;
            # alt+shift+up/down carry the focused window and follow; alt+<n>
            # jumps to a numbered row (creating it if absent). In the
            # `paneru.setup` style the key is the command exactly as `paneru
            # run` takes it (spaces, not the TOML `[bindings]` table's
            # underscores) and the value is the chord.
            bindings = {
              #              "window virtual north" = "alt - up";
              #              "window virtual south" = "alt - down";
              #              "window virtualmove north" = "alt + shift - up";
              #              "window virtualmove south" = "alt + shift - down";
              #              "window virtualnum 1" = "alt - 1";
              #              "window virtualnum 2" = "alt - 2";
              #              "window virtualnum 3" = "alt - 3";
              #              "window virtualnum 4" = "alt - 4";
              #              "window virtualnum 5" = "alt - 5";
              #              "window virtualnum 6" = "alt - 6";
              #              "window virtualnum 7" = "alt - 7";
              #              "window virtualnum 8" = "alt - 8";
              #              "window virtualnum 9" = "alt - 9";
            };

            # The whole configuration, declared from Lua rather than a
            # paneru.toml: the table mirrors the TOML sections one-for-one, and
            # a `paneru.setup{...}` in init.lua is authoritative (services.paneru
            # .settings would be ignored, so it is left unset below).
            setupConfig = {
              default_workspaces = 1;
              inherit bindings;
              decorations = {
                active.border = {
                  enabled = true;
                  color = config.stylix.base16Scheme.base03;
                  width = 2.0;
                  radius = 12.0;
                };
                inactive.dim = {
                  opacity = 0.1;
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

              # rift's 3-finger gestures. vertical=true lets a 3-finger
              # up/down swipe switch virtual-workspace rows (paneru reads
              # the trackpad directly). macOS's own 3-finger vertical swipe
              # (Mission Control / App Expose) is disabled below so it does
              # not fire alongside paneru.
              #
              # This block is the one part of the configuration that must ALSO
              # go into the TOML `settings` below. paneru's CGEventTap holds the
              # `Config` handle built from the TOML file in
              # `PlatformCallbacks::setup_handlers` (src/platform.rs), while a
              # `paneru.setup{...}` config is inserted as a *separate* bevy
              # resource (src/ecs.rs, "wins over the TOML InitialConfig") — the
              # two never share an ArcSwap at startup, so the tap only ever
              # reads TOML. `handle_swipe` bails out early when
              # `swipe_gesture_fingers()` is None, which means a Lua-only swipe
              # section leaves *all* trackpad swiping dead, horizontal included.
              swipe.gesture = {
                fingers_count = 3;
                direction = "Natural";
                vertical = true;
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
            };

            # paneru's `init.lua`, discovered at
            # `${XDG_CONFIG_HOME:-~/.config}/paneru/init.lua` (or `~/.paneru.lua`
            # without XDG) — written out by services.paneru.config below.
            paneruInitLua =
              # lua
              ''

                          -- Generated from aspects/wms/paneru/default.nix — do not edit by hand.
                          paneru.setup ${lib.generators.toLua { } setupConfig}

                          -- Named scratchpads (aspects/wms/paneru/scratchpad.lua, shipped as
                          -- the `paneru_scratchpad` module via services.paneru
                          -- .extraLuaPackages below). `setup` compiles each pad's match
                          -- patterns, registers the placement/hide-on-focus-loss hooks and
                          -- binds each pad's chord.
                require("paneru_scratchpad").setup ${lib.generators.toLua { } scratchpadSpec}
                          ${lib.optionalString (options.programs ? sketchybar) ''
                            -- Incremental bar repaints, driven from paneru's own event loop
                            -- (services.paneru.extraLuaPackages, below). Creating the items and
                            -- the initial paint are sketchybar's, in sketchybar's process — see
                            -- sketchybar/wm.lua and sketchybar/paneru-events.lua.
                            require("paneru_events")
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

                      ;; Scratchpads are Lua-side, bound with paneru.bind in
                      ;; init.lua (see scratchpads above): Command::Lua has no
                      ;; argv encoding, so no send-cmd can reach a Lua callback.
                      ;; These aliases therefore just emit the bare letter — the
                      ;; space_manip layer already holds lalt and forks on
                      ;; lshift, so what leaves kanata's virtual HID is the
                      ;; alt+shift-<letter> chord paneru's own event tap binds.
                      toggle_discord_scratchpad d
                      toggle_fantastical_scratchpad f
                      toggle_beeper_scratchpad b
                      toggle_music_scratchpad s
                      toggle_1password_scratchpad p

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
              # sketchybar's own config stays WM-agnostic; the paneru-specific
              # half arrives as the `wm` module its `require("wm")` picks up,
              # exactly as den.aspects.rift does it. That module creates the
              # workspace items and paints them from state it queries itself,
              # through paneru's loadable Lua client module (`require("paneru")`,
              # `pkgs.paneru.luaModule`, built against sketchybar's own
              # interpreter) — at config load and again on each `paneru_load`.
              # paneru's process only pushes the incremental repaints on top
              # (see services.paneru.extraLuaPackages below).
              programs.sketchybar.extraPackages = [ config.services.paneru.finalPackage ];
              programs.sketchybar.sbarLuaPackage = pkgs.sbarlua.override { luaPackages = pkgs.luajitPackages; };
              # Overrides the sketchybar aspect's own lua5_5 pick: paneru's
              # loadable client module is built against LuaJIT, and both
              # processes have to agree on the interpreter.
              programs.sketchybar.extraLuaPackages = luaPs: [
                (config.services.paneru.finalPackage.passthru.luaModule.override { inherit (luaPs) lua; })
                (mkLuaFileModule "paneru_bar" ./sketchybar/paneru-bar.lua luaPs)
                (mkLuaFileModule "wm" ./sketchybar/wm.lua luaPs)
              ];
            })
            {
              services.paneru = {
                enable = true;
                package = pkgs.paneru;
                lua = lib.mkIf (options.programs ? sketchybar) config.programs.sketchybar.luaPackage;
                luaConfig.enable = true;
                extraPackages = [ pkgs.sketchybar ];
                # `paneru_scratchpad` is unconditional — init.lua always
                # requires it. Only the bar-drawing half is gated on the
                # sketchybar aspect being present.
                extraLuaPackages =
                  luaPs:
                  [
                    (mkLuaFileModule "paneru_scratchpad" ./scratchpad.lua luaPs)
                  ]
                  ++ lib.optionals (options.programs ? sketchybar) [
                    (pkgs.sbarlua.override { luaPackages = luaPs; })
                    (mkColorsModule colourConfig luaPs)
                    (mkIconMapModule luaPs)
                    (mkLuaFileModule "paneru_bar" ./sketchybar/paneru-bar.lua luaPs)
                    (mkLuaFileModule "paneru_events" ./sketchybar/paneru-events.lua luaPs)
                  ];
                # Everything else is declared from Lua instead (`paneru.setup`
                # in `config`), which takes precedence over a paneru.toml. The
                # swipe section is the exception — see its comment above: the
                # event tap reads the TOML config and nothing else, so the
                # gesture finger count has to be here or no swipe is ever
                # intercepted.
                config = paneruInitLua;
              };

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
