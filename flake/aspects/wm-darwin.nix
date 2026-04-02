{ den, ... }:
{
  # ── macOS Window Manager aspect ───────────────────────────────────────────────
  # Home-manager: Yabai (tiling WM), Rift (tiling WM), Sketchybar (status bar),
  #               JankyBorders (window borders).
  # Darwin: Yabai scripting-addition sudo rule (via darwin-desktop sudoagents).
  # Include in a user aspect to enable a macOS tiling desktop environment.
  den.aspects."wm-darwin" = {
    homeManager =
      { config, lib, pkgs, ... }:
      let
        cfgYabai = config.auscybernix.wms.yabai;
        cfgRift = config.auscybernix.wms.rift;
        cfgSbar = config.auscybernix.programs.sketchybar;
        sources = pkgs.callPackage ../../_sources/generated.nix { };
      in
      {
        # ── services.yabai option declaration (inline) ────────────────────
        options.services.yabai = with lib.types; {
          enable = lib.mkOption { type = bool; default = false; };
          package = lib.mkOption { type = path; default = pkgs.yabai; };
          enableScriptingAddition = lib.mkOption { type = bool; default = false; };
          errorLogFile = lib.mkOption {
            type = with lib.types; nullOr (either path str);
            defaultText = lib.literalExpression
              "\${config.home.homeDirectory}/Library/Logs/yabai/err.log";
          };
          outLogFile = lib.mkOption {
            type = with lib.types; nullOr (either path str);
            defaultText = lib.literalExpression
              "\${config.home.homeDirectory}/Library/Logs/yabai/out.log";
          };
          config = lib.mkOption { type = attrs; default = { }; };
          extraConfig = lib.mkOption { type = lib.types.lines; default = ""; };
        };

        # ── auscybernix WM options ────────────────────────────────────────
        options.auscybernix.wms.yabai = {
          enable = lib.mkEnableOption "yabai window manager";
          scratchpads = lib.mkOption {
            description = "Scratchpad windows to configure in yabai.";
            type = with lib.types; attrsOf (submodule {
              options = {
                command = lib.mkOption { type = str; default = ""; };
                detectionRules = lib.mkOption { type = attrsOf str; default = [ ]; };
                rules = lib.mkOption { type = listOf str; default = [ ]; };
              };
            });
          };
        };
        options.auscybernix.wms.rift.enable =
          lib.mkEnableOption "Rift window manager";
        options.auscybernix.programs.sketchybar.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
        };

        config = lib.mkMerge [
          # ── Yabai ─────────────────────────────────────────────────────────
          (lib.mkIf cfgYabai.enable {
            assertions = [
              (lib.hm.assertions.assertPlatform "services.yabai" pkgs lib.platforms.darwin)
            ];
            services.jankyborders = {
              enable = true;
              settings = {
                active_color = "0xff${config.stylix.base16Scheme.base03}";
                inactive_color = "0xff${config.stylix.base16Scheme.base0D}";
                style = "round";
                width = 6.0;
              };
            };
            auscybernix.keybinds.kanata.extraConfigPaths = [
              (pkgs.writeText "yabai-bindings" ''
                toggle_beeper_scratchpad (t! runasuser "yabai -m window --toggle beeper || open -a 'Beeper\ Desktop'")
                minimise (t! runasuser "yabai -m window --minimize")
                toggle_1password_scratchpad (t! runasuser "yabai -m window --toggle 1password || open -a '1Password'")
                toggle_discord_scratchpad (t! runasuser "yabai -m window --toggle discord || open -a Discord")
                switch-focus (t! runasuser "yabai -m window --focus next")
                reverse-switch-focus (t! runasuser "yabai -m window --focus prev")
                kill-focus (t! runasuser "yabai -m window --close")
                1s (t! runasuser "yabai -m space --focus 1")
                2s (t! runasuser "yabai -m space --focus 2")
                3s (t! runasuser "yabai -m space --focus 3")
                4s (t! runasuser "yabai -m space --focus 4")
                5s (t! runasuser "yabai -m space --focus 5")
                6s (t! runasuser "yabai -m space --focus 6")
                7s (t! runasuser "yabai -m space --focus 7")
                8s (t! runasuser "yabai -m space --focus 8")
                9s (t! runasuser "yabai -m space --focus 9")
                10s (t! runasuser "yabai -m space --focus 10")
                1m (t! runasuser "yabai -m window --space 1")
                2m (t! runasuser "yabai -m window --space 2")
                3m (t! runasuser "yabai -m window --space 3")
                4m (t! runasuser "yabai -m window --space 4")
                5m (t! runasuser "yabai -m window --space 5")
                6m (t! runasuser "yabai -m window --space 6")
                7m (t! runasuser "yabai -m window --space 7")
                8m (t! runasuser "yabai -m window --space 8")
                9m (t! runasuser "yabai -m window --space 9")
                10m (t! runasuser "yabai -m window --space 10")
                dm (t! runasuser "yabai -m window --display recent")
              '')
            ];
            services.yabai = {
              enable = true;
              enableScriptingAddition = true;
              config = {
                focus_follows_mouse = "off";
                mouse_follows_focus = "off";
                window_placement = "second_child";
                window_opacity = "off";
                external_bar = "main:40:0";
                layout = "bsp";
                top_padding = 10;
                bottom_padding = 6;
                left_padding = 10;
                right_padding = 10;
                window_gap = 10;
              };
              extraConfig =
                (lib.concatMapAttrsStringSep "\n"
                  (name: sp:
                    let
                      rules = lib.concatStringsSep " " sp.rules;
                      value = "${
                        lib.concatMapAttrsStringSep " " (k: v: "${k}=\"${v}\"") sp.detectionRules
                      } manage=off sticky=on scratchpad=${name} ${rules}";
                    in
                    ''
                      # Scratchpad: ${name}
                      yabai -m rule --add ${value}
                      yabai -m rule --apply ${value}
                    '')
                  cfgYabai.scratchpads)
                + ''
                  yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
                  sudo yabai --load-sa
                  yabai -m rule --add app="^System Preferences$" manage=off
                  yabai -m signal --add app='^Ghostty$' event=window_created action='yabai -m space --layout bsp'
                  yabai -m signal --add app='^Ghostty$' event=window_destroyed action='yabai -m space --layout bsp'
                '';
              errorLogFile = lib.mkOptionDefault
                "${config.home.homeDirectory}/Library/Logs/yabai/yabai.err.log";
              outLogFile = lib.mkOptionDefault
                "${config.home.homeDirectory}/Library/Logs/yabai/yabai.out.log";
            };
            home.packages = [ config.services.yabai.package ];
            launchd.agents.yabai = {
              enable = true;
              config = {
                ProcessType = "Interactive";
                ProgramArguments =
                  [ "${config.services.yabai.package}/bin/yabai" ]
                  ++ lib.optionals
                    (config.services.yabai.config != { } || config.services.yabai.extraConfig != "")
                    [
                      "-c"
                      (
                        let
                          toYabaiConfig = opts:
                            lib.concatStringsSep "\n"
                              (lib.mapAttrsToList (p: v: "yabai -m config ${p} ${toString v}") opts);
                          configFile =
                            pkgs.writeScript "yabairc"
                              ((if config.services.yabai.config != { } then
                                "${toYabaiConfig config.services.yabai.config}"
                              else "") +
                              lib.optionalString (config.services.yabai.extraConfig != "")
                                ("\n" + config.services.yabai.extraConfig + "\n"));
                        in
                        "${configFile}"
                      )
                    ];
                EnvironmentVariables.PATH =
                  "${config.services.yabai.package}/bin:/usr/bin:/bin:/usr/sbin:/sbin";
                KeepAlive = true;
                RunAtLoad = true;
                StandardErrorPath = config.services.yabai.errorLogFile;
                StandardOutPath = config.services.yabai.outLogFile;
              };
            };
            auscybernix.wms.yabai.scratchpads = {
              "1password" = { detectionRules = { app = "^1Password$"; }; rules = [ "grid=11:11:1:1:9:9" ]; };
              "discord" = { detectionRules = { app = "^Discord$"; "title!" = "^Discord Updater$"; }; rules = [ "grid=11:11:1:1:9:9" ]; };
              "beeper" = { detectionRules = { app = "^Beeper$"; "title!" = "^Beeper Updater$"; }; rules = [ "grid=11:11:1:1:9:9" ]; };
            };
          })

          # ── Rift ──────────────────────────────────────────────────────────
          (lib.mkIf cfgRift.enable {
            imports = [ ../../modules/home/wms/rift/internal ];
            services.jankyborders = {
              enable = true;
              settings = {
                active_color = "0xff${config.stylix.base16Scheme.base03}";
                inactive_color = "0xff${config.stylix.base16Scheme.base0D}";
                style = "round";
                width = 6.0;
              };
            };
            auscybernix.keybinds.kanata.extraConfigPaths = [
              (pkgs.writeText "rift-keybinds"
                # commonlisp
                ''
                  (defalias
                    enable_spaces (t! runasuser "rift-cli execute window toggle-space-activated ")
                    toggle_discord_scratchpad (t! runasuser "rift-cli execute window toggle-scratchpad --name discord || open -a Discord")
                    toggle_beeper_scratchpad (t! runasuser " rift-cli execute window toggle-scratchpad --name beeper || open -a 'Beeper\ Desktop'")
                    minimise (t! runasuser "yabai -m window --minimize")
                    toggle_1password_scratchpad (t! runasuser "rift-cli execute window toggle-scratchpad --name 1password  || open -a '1Password'")
                    switch-focus (t! runasuser "yabai -m window --focus next")
                    reverse-switch-focus (t! runasuser "yabai -m window --focus prev")
                    kill-focus (t! runasuser "rift-cli query windows | jq '.[] | select(.is_focused) | .window_server_id' | xargs -I{} rift-cli execute window close --window-id {}")
                    1s (t! runasuser "rift-cli execute workspace switch 0")
                    2s (t! runasuser "rift-cli execute workspace switch 1")
                    3s (t! runasuser "rift-cli execute workspace switch 2")
                    4s (t! runasuser "rift-cli execute workspace switch 3")
                    5s (t! runasuser "rift-cli execute workspace switch 4")
                    6s (t! runasuser "rift-cli execute workspace switch 5")
                    7s (t! runasuser "rift-cli execute workspace switch 6")
                    8s (t! runasuser "rift-cli execute workspace switch 7")
                    9s (t! runasuser "rift-cli execute workspace switch 8")
                    10s (t! runasuser "rift-cli execute workspace switch 10")
                    1m (t! runasuser "rift-cli execute workspace move-window 0")
                    2m (t! runasuser "rift-cli execute workspace move-window 1")
                    3m (t! runasuser "rift-cli execute workspace move-window 2")
                    4m (t! runasuser "rift-cli execute workspace move-window 3")
                    5m (t! runasuser "rift-cli execute workspace move-window 4")
                    6m (t! runasuser "rift-cli execute workspace move-window 5")
                    7m (t! runasuser "rift-cli execute workspace move-window 6")
                    8m (t! runasuser "rift-cli execute workspace move-window 7")
                    9m (t! runasuser "rift-cli execute workspace move-window 8")
                    10m (t! runasuser "rift-cli execute workspace move-window 10")
                    shiftUp (t! runasuser "rift-cli execute display move-window --direction up")
                    shiftDown (t! runasuser "rift-cli execute display move-window --direction down")
                    shiftLeft (t! runasuser "rift-cli execute display move-window --direction left")
                    shiftRight (t! runasuser "rift-cli execute display move-window --direction right")
                    focusUp (t! runasuser "rift-cli execute display focus --direction up")
                    focusDown (t! runasuser "rift-cli execute display focus --direction down")
                    focusLeft (t! runasuser "rift-cli execute display focus --direction left")
                    focusRight (t! runasuser "rift-cli execute display focus --direction right"))
                '')
            ];
            services.rift = {
              enable = true;
              package = pkgs.rift;
              extraPackages = with pkgs; [ rift sketchybar ];
              settings = {
                settings = {
                  run_on_start = [
                    "rift-cli subscribe cli --event workspace_changed --command /bin/sh --args -c --args 'sketchybar --trigger rift_workspace_changed FOCUSED_WORKSPACE=\\\"$RIFT_WORKSPACE_NAME\\\"'"
                    "rift-cli subscribe cli --event windows_changed --command /bin/sh --args -c --args 'sketchybar --trigger rift_windows_changed RIFT_WORKSPACE_NAME=\\\"$RIFT_WORKSPACE_NAME\\\" RIFT_WINDOW_COUNT=\\\"$RIFT_WINDOW_COUNT\\\"'"
                    "rift-cli subscribe cli --event window_title_changed --command /bin/sh --args -c --args 'sketchybar --trigger rift_windows_title'"
                  ];
                  default_disable = false;
                  layout.gaps.outer = { top = 15; left = 10; right = 10; bottom = 5; };
                  layout.mode = "bsp";
                  focus_follows_mouse = false;
                  mouse_follows_focus = false;
                  gestures = { enabled = true; fingers = 3; };
                };
                virtual_workspaces = {
                  enabled = true;
                  default_workspace_count = 5;
                  workspace_names = [ "Main" "Dev" "Chat" "Media" "Misc" ];
                  app_rules = [
                    { app_id = "com.hnc.Discord"; scratchpad = "discord"; }
                    { app_id = "com.automattic.beeper.desktop"; scratchpad = "beeper"; }
                    { app_id = "com.agilebits.onepassword7"; scratchpad = "1password"; }
                  ];
                };
                keys."Alt + Z" = "toggle_space_activated";
              };
            };
          })

          # ── Sketchybar ────────────────────────────────────────────────────
          (lib.mkIf cfgSbar.enable {
            home.packages = with pkgs; [ yq jq ];
            home.file."${config.auscybernix.flakeConfig.flakeFolder}/.config/sketchybar/colors.sh" = {
              text =
                let colors = config.stylix.base16Scheme; in
                ''
                  #!/bin/bash
                  # generated by stylix — do not edit manually
                  export COLOR_BACKGROUND='${colors.base00}'
                  export COLOR_BLACK='${colors.base01}'
                  export COLOR_SELECTION='${colors.base02}'
                  export COLOR_COMMENT='${colors.base03}'
                  export COLOR_DARKGRAY='${colors.base04}'
                  export COLOR_FOREGROUND='${colors.base05}'
                  export COLOR_BRIGHTWHITE='${colors.base06}'
                  export COLOR_WHITE='${colors.base07}'
                  export COLOR_RED='${colors.base08}'
                  export COLOR_ORANGE='${colors.base09}'
                  export COLOR_YELLOW='${colors.base0A}'
                  export COLOR_GREEN='${colors.base0B}'
                  export COLOR_CYAN='${colors.base0C}'
                  export COLOR_BLUE='${colors.base0D}'
                  export COLOR_MAGENTA='${colors.base0E}'
                  export COLOR_BROWN='${colors.base0F}'
                '';
            };
            home.file.".config/sketchybar".source =
              config.lib.file.linkLocalPath ../../.config/sketchybar;
            home.file.".config/icon_map.sh".source = sources.icon_map.src;
            programs.sketchybar = {
              service.enable = true;
              enable = true;
              extraPackages = with pkgs; [ jq yq yabai nowplaying-cli rift ];
            };
          })
        ];
      };
  };
}
