{
  config,
  pkgs,
  inputs,
  ...
}:
{
  #home.packages = with pkgs; [ wezterm];
  programs.kitty.enable = true; # required for the default Hyprland config
  wayland.windowManager.hyprland = {

    package = pkgs.hyprland;
    portalPackage = pkgs.xdg-desktop-portal-hyprland;
    enable = true; # enable Hyprland
    xwayland.enable = true; # enable Hyprland
    plugins = [
      inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprbars
    ];
    extraConfig = ''
      # change monitor to high resolution, the last argument is the scale factor
      monitor = , highres, auto, 1

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


    '';
    settings = {
      "$mod" = "SUPER";
      env = [
        "LIBVA_DRIVER_NAME,nvidia"
        "__GLX_VENDOR_LIBRARY_NAME,nvidia"
        "ELECTRON_OZONE_PLATFORM_HINT,auto"
      ];
      bind =
        [
          "$mod, T, exec, ghostty"
          "$mod, C, exec, google-chrome-stable"
          "$mod, F, exec, dolphin"
          "$mod, C, killactive"

        ]
        ++ (
          # workspaces
          # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
          builtins.concatLists (
            builtins.genList (
              i:
              let
                ws = i + 1;
              in
              [
                "$mod, code:1${toString i}, workspace, ${toString ws}"
                "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
              ]
            ) 9
          )
        );
    };
  };

  programs.hyprpanel = {
    enable = true;
    #	systemd.enable = true;
    hyprland.enable = true;
    overwrite.enable = true;
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

}
