{ den, lib, ... }:
{
  den.aspects.hyprland = {
    homeManager =
      { config, pkgs, ... }:
      let
        scratchpads = config.hyprland.scratchpads or { };
      in
      {
        options.hyprland.scratchpads = lib.mkOption {
          default = { };
          type = lib.types.attrsOf (
            lib.types.submodule {
              options = {
                command = lib.mkOption {
                  type = lib.types.str;
                  default = "";
                };
                windowClass = lib.mkOption {
                  type = lib.types.str;
                  default = "";
                };
                extraRules = lib.mkOption {
                  type = lib.types.listOf lib.types.str;
                  default = [ ];
                };
              };
            }
          );
        };

        config = {
          home.packages = with pkgs; [ hyprpolkitagent ];
          programs.wofi.enable = true;

          wayland.windowManager.hyprland = {
            package = null;
            portalPackage = null;
            enable = true;
            xwayland.enable = true;
            plugins = with pkgs.hyprlandPlugins; [ hyprbars ];
            extraConfig = ''
              monitor = , highres, auto, 1.5
              xwayland {
                force_zero_scaling = true
              }
              env = GDK_SCALE,2
              env = XCURSOR_SIZE,32
              decoration {
                rounding = 10
                rounding_power = 2
                active_opacity = 1.0
                inactive_opacity = 1.0
                shadow {
                  enabled = true
                  range = 4
                  render_power = 3
                  color = rgba(1a1a1aee)
                }
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
                resize_on_border = false
                allow_tearing = false
                layout = dwindle
              }
            '';
            settings = lib.foldAttrs
              (item: acc: if builtins.isList item then acc ++ item else item)
              [ ]
              [
                (lib.foldAttrs (item: acc: acc ++ item) [ ] (
                  lib.attrsets.mapAttrsToList (name: sp: {
                    windowrule = [
                      "workspace special:${name}, class:${sp.windowClass}"
                      "float, class:${sp.windowClass}"
                    ]
                    ++ builtins.map (rule: "${rule}, class:${sp.windowClass}") sp.extraRules;
                    exec-once = [ ("[workspace special:${name}]" + sp.command) ];
                  }) scratchpads
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
                    "systemctl --user stop graphical-session.target && systemctl --user start hyprland-session.target"
                    "1password"
                    "systemctl --user start hyprpolkitagent"
                  ];
                }
              ];
          };

          programs.hyprpanel = {
            enable = true;
            systemd.enable = true;
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
      };
  };
}
