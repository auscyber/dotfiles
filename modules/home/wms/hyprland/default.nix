{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.wms.hyprland;
in
{
  options.auscybernix.wms.hyprland = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Hyprland as the Wayland compositor.";
    };
    scratchpads = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            command = lib.mkOption {
              type = lib.types.str;
              default = "";
              description = "The command to run the scratchpad.";
            };
            windowClass = lib.mkOption {
              type = lib.types.str;
              default = "";
              description = "The window class of the scratchpad.";
            };
            extraRules = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Additional Hyprland rules to apply.";
            };
          };
        }
      );
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ hyprpolkitagent ];
    #home.packages = with pkgs; [ wezterm];
    programs.wofi.enable = true;
    #    programs.kitty.enable = true; # required for the default Hyprland config
    wayland.windowManager.hyprland = {

      package = null;
      portalPackage = null;
      enable = true; # enable Hyprland
      xwayland.enable = true; # enable Hyprland
      plugins = with pkgs.hyprlandPlugins; [
        hyprbars
      ];
      extraConfig = ''
                              monitor = , highres, auto, 1.5

                              # unscale XWayland
                              xwayland {
                                force_zero_scaling = true
                              }

                              # toolkit-specific scale
                              env = GDK_SCALE,2
                              env = XCURSOR_SIZE,32
        					    decoration {
                                  rounding = 10
                                  rounding_power = 2

                                  # Change transparency of focused and unfocused windows
                                  active_opacity = 1.0
                                  inactive_opacity = 1.0

                                  shadow {
                                      enabled = true
                                      range = 4
                                      render_power = 3
                                      color = rgba(1a1a1aee)
                                  }

                                  # https://wiki.hyprland.org/Configuring/Variables/#blur
                                  blur {
                                      enabled = true
                                      size = 3
                                      passes = 1

                                      vibrancy = 0.1696
                                  }
                              }
                        general {
                            gaps_in = 5
                            gaps_out = 20

                            border_size = 2

                            # https://wiki.hyprland.org/Configuring/Variables/#variable-types for info about colors
                #            col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
                #            col.inactive_border = rgba(595959aa)

                            # Set to true enable resizing windows by clicking and dragging on borders and gaps
                            resize_on_border = false

                            # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
                            allow_tearing = false

                            layout = dwindle
                        }

      '';
      settings =

        # what does this

        lib.foldAttrs (item: acc: if builtins.isList item then acc ++ item else item)
          [ ]
          [
            (lib.foldAttrs (item: acc: acc ++ item) [ ] (
              lib.attrsets.mapAttrsToList (name: config: {
                windowrule = [
                  "workspace special:${name}, class:${config.windowClass}"
                  "float, class:${config.windowClass}"
                ]
                ++ builtins.map (rule: "${rule}, class:${config.windowClass}") config.extraRules;
                exec-once = [ ("[workspace special:${name}]" + config.command) ];
              }) cfg.scratchpads
            ))

            {
              "$mod" = "SUPER";

              env = [
                "LIBVA_DRIVER_NAME,nvidia"
                "__GLX_VENDOR_LIBRARY_NAME,nvidia"
                "NVD_BACKEND,direct"
                "ELECTRON_OZONE_PLATFORM_HINT,auto"
              ];

              exec-once = [
                "1password"
                # "waybar"
                "systemctl --user start hyprpolkitagent"
              ];
            }
          ];
    };

    programs.hyprpanel = {
      enable = true;
      systemd.enable = true;
      # Configure and theme almost all options from the GUI.
      # Options that require '{}' or '[]' are not yet implemented,
      # except for the layout above.
      # See 'https://hyprpanel.com/configuration/settings.html'.
      # Default: <same as gui>
      settings = {
        bar.launcher.autoDetectIcon = true;
        bar.workspaces.show_icons = true;

        menus.clock = {
          time = {
            military = true;
            hideSeconds = true;
          };
          weather.unit = "metric";
        };

        menus.dashboard.directories.enabled = false;
        menus.dashboard.stats.enable_gpu = true;

        theme.bar.transparent = true;

        theme.font = {
          name = "CaskaydiaCove NF";
          size = "16px";
        };
      };
    };
  };

}
