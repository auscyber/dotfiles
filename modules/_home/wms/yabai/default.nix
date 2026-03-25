{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.wms.yabai;
in
{

  options.auscybernix.wms.yabai = {
    enable = lib.mkEnableOption "Enable yabai window manager";
    scratchpads = lib.mkOption {
      description = "List of scratchpads to configure.";
      type =
        with lib.types;
        attrsOf (submodule {
          options = {
            command = lib.mkOption {
              type = str;
              default = "";
              description = "The command to run the scratchpad.";
            };
            detectionRules = lib.mkOption {
              type = attrsOf str;
              default = [ ];
              description = "yabai detection rules to apply.";
            };
            rules = lib.mkOption {
              type = listOf str;
              default = [ ];
              description = "Additional yabai rules to apply.";
            };
          };
        });
    };
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
	(pkgs.writeText "yabai-bindinds" ''
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
        (lib.concatMapAttrsStringSep "\n" (
          name: sp:
          let
            rules = lib.concatStringsSep " " sp.rules;
            value = "${
              lib.concatMapAttrsStringSep " " (k: v: "${k}=\"${v}\"") sp.detectionRules
            } manage=off sticky=on   scratchpad=${name} ${rules}";

          in
          ''
            # Scratchpad: ${name}
            yabai -m rule --add  ${value}
            yabai -m rule --apply ${value}
          ''
        ) cfg.scratchpads)
        + ''
                yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
                sudo yabai --load-sa
                                  yabai -m rule --add app="^System Preferences$" manage=off
                                  yabai -m signal --add app='^Ghostty$' event=window_created action='yabai -m space --layout bsp'
                                  yabai -m signal --add app='^Ghostty$' event=window_destroyed action='yabai -m space --layout bsp'
          						                                      	'';

    };
  };
}
