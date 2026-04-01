{ den, ... }:
{
  # ── Linux Window Manager aspect ───────────────────────────────────────────────
  # Home-manager: Hyprland (Wayland), Niri (Wayland), Picom (X11 compositor).
  # Include in a user aspect to configure Linux desktop window management.
  den.aspects."wm-linux" = {
    homeManager =
      { config, lib, pkgs, inputs, ... }:
      let
        cfgHypr = config.auscybernix.wms.hyprland;
        cfgNiri = config.auscybernix.wms.niri;
        cfgPicom = config.auscybernix.programs.picom;
      in
      {
        options.auscybernix.wms.hyprland = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable Hyprland Wayland compositor.";
          };
          scratchpads = lib.mkOption {
            default = { };
            type = lib.types.attrsOf (lib.types.submodule {
              options = {
                command = lib.mkOption { type = lib.types.str; default = ""; };
                windowClass = lib.mkOption { type = lib.types.str; default = ""; };
                extraRules = lib.mkOption { type = lib.types.listOf lib.types.str; default = [ ]; };
              };
            });
          };
        };
        options.auscybernix.wms.niri.enable =
          lib.mkEnableOption "Niri Wayland compositor";
        options.auscybernix.programs.picom = lib.mkOption {
          type = lib.types.attrsOf lib.types.any;
          default = { };
          description = "Picom compositor configuration options.";
        };

        config = lib.mkMerge [
          # ── Hyprland ─────────────────────────────────────────────────────
          (lib.mkIf cfgHypr.enable {
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
              settings =
                lib.foldAttrs (item: acc: if builtins.isList item then acc ++ item else item) [ ]
                  [
                    (lib.foldAttrs (item: acc: acc ++ item) [ ] (
                      lib.attrsets.mapAttrsToList (name: sc: {
                        windowrule = [
                          "workspace special:${name}, class:${sc.windowClass}"
                          "float, class:${sc.windowClass}"
                        ] ++ builtins.map (rule: "${rule}, class:${sc.windowClass}") sc.extraRules;
                        exec-once = [ ("[workspace special:${name}]" + sc.command) ];
                      }) cfgHypr.scratchpads
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
                  time = { military = true; hideSeconds = true; };
                  weather.unit = "metric";
                };
                menus.dashboard.directories.enabled = false;
                menus.dashboard.stats.enable_gpu = true;
                theme.bar.transparent = true;
                theme.font = { name = "CaskaydiaCove NF"; size = "16px"; };
              };
            };
          })

          # ── Niri ──────────────────────────────────────────────────────────
          (lib.mkIf cfgNiri.enable {
            programs.niri = {
              enable = true;
              settings = { };
            };
          })

          # ── Picom ─────────────────────────────────────────────────────────
          (lib.mkIf cfgPicom.enable {
            services.picom = {
              enable = true;
              experimentalBackends = true;
              fade = false;
              shadow = false;
              shadowOffsets = [ (-30) (-30) ];
              shadowOpacity = "0.25";
              shadowExclude = [ "name = 'xmonad'" ];
              blur = true;
              blurExclude = [ "class_g = 'slop'" ];
              vSync = true;
              refreshRate = 60;
              extraOptions = ''
                corner-radius = 5.0;
                rounded-corners-exclude = [
                  "class_g = 'Polybar'",
                  "class_g = 'Minecraft* 1.16.4'",
                  "class_g = 'xmobar'"
                ]
                round-borders = 1;
              '';
            };
          })
        ];
      };
  };
}
