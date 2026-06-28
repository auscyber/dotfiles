{ den, lib, ... }:
{
  den.aspects.yabai = {
    homeManager =
      { config, ... }:
      let
        scratchpads = config.yabai.scratchpads or { };
      in
      {
        options.yabai.scratchpads = lib.mkOption {
          default = { };
          description = "yabai scratchpads keyed by name.";
          type =
            with lib.types;
            attrsOf (submodule {
              options = {
                command = lib.mkOption {
                  type = str;
                  default = "";
                };
                detectionRules = lib.mkOption {
                  type = attrsOf str;
                  default = { };
                };
                rules = lib.mkOption {
                  type = listOf str;
                  default = [ ];
                };
              };
            });
        };

        config = {
          services.jankyborders = {
            enable = true;
            settings = lib.mkIf (config.stylix.enable or false) {
              active_color = "0xff${config.stylix.base16Scheme.base03}";
              inactive_color = "0xff${config.stylix.base16Scheme.base0D}";
              style = "round";
              width = 6.0;
            };
          };
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
                scratchpads)
              + ''
                yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
                sudo yabai --load-sa
                yabai -m rule --add app="^System Preferences$" manage=off
                yabai -m signal --add app='^Ghostty$' event=window_created action='yabai -m space --layout bsp'
                yabai -m signal --add app='^Ghostty$' event=window_destroyed action='yabai -m space --layout bsp'
              '';
          };
        };
      };
  };
}
