{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.wms.rift;
in
{
  options.auscybernix.wms.rift = {
    enable = lib.mkEnableOption "Enable rift window manager ";

  };

  config = lib.mkIf cfg.enable {
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
    auscybernix.keybinds.kanata.extraConfigPaths = [
      (pkgs.writeText "rift-keybinds" ''
        (defalias
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
          dm (t! runasuser "rift-cli execute window move-to-display recent"))
      '')
    ];

    services.rift = {
      enable = true;
      package = pkgs.rift;
      extraPackages = with pkgs; [
        rift
        sketchybar
      ];
      settings = {

        settings = {
          run_on_start = [
            "rift-cli subscribe cli --event workspace_changed --command /bin/sh --args -c --args 'sketchybar --trigger rift_workspace_changed FOCUSED_WORKSPACE=\\\"$RIFT_WORKSPACE_NAME\\\"'"
            "rift-cli subscribe cli --event windows_changed --command /bin/sh --args -c --args 'sketchybar --trigger rift_windows_changed RIFT_WORKSPACE_NAME=\\\"$RIFT_WORKSPACE_NAME\\\" RIFT_WINDOW_COUNT=\\\"$RIFT_WINDOW_COUNT\\\"'"

            "rift-cli subscribe cli --event window_title_changed --command /bin/sh --args -c --args 'sketchybar --trigger rift_windows_title'"
          ];

          default_disable = false;
          layout.gaps.outer = {
            top = 15;
            left = 10;
            right = 10;
            bottom = 5;
          };
          layout.mode = "bsp";
          focus_follows_mouse = false;
          mouse_follows_focus = false;
          gestures = {
            enabled = true;
            fingers = 3;

          };
        };
        virtual_workspaces = {
          enabled = true;
          default_workspace_count = 5;
		  workspace_names = ["Main" "Dev" "Chat" "Media" "Misc"];
          app_rules = [
            {
              app_id = "com.hnc.Discord";
              scratchpad = "discord";
            }
            {
              app_id = "com.beeper.Beeper-Desktop";
              scratchpad = "beeper";
            }
            {
              app_id = "com.agilebits.onepassword7";
              scratchpad = "1password";
            }

          ];

        };
        keys = {
          "Alt + Z" = "toggle_space_activated";
        };
      };
    };
  };
}
