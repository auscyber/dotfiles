{
  config,
  pkgs,
  system,
  lib,
  inputs,
  modulesPath,
  ...
}:

{
  imports = [
    #    ../keybind_program
  ];

  config = {

    stylix.targets.fish.enable = false;
    #  services.keybindControl = {
    #    enable = true;
    #    applications = {
    #      "skhd" = {
    #        transformer = import ../keybind_program/transformers/skhd.nix;
    #        keybinds = [
    #          {
    #            Key = "ta";
    #            modifiers = [ "Ctrl" "Alt" ];
    #            action =
    #               "open -a Terminal";
    #            description = "Open Terminal with Ctrl + Alt + T";
    #            group = "utilities";
    #          }
    #        ];
    #        name = "Terminal Keybinds";
    #        description = "Keybinds for opening Terminal applications.";
    #      };
    #
    #    };
    #  };
    nix = {
      #              package = pkgs.nixVersions.latest;
      settings = {
        experimental-features = [
          "nix-command"
          "flakes"
        ];
      };
    };
    home.shell.enableFishIntegration = true;
    manual.manpages.enable = true;
    manual.html.enable = true;

    services.lorri = {
      enable = false;
    };

    home.packages = with pkgs; [
      cachix
      ivy-fetch
      devenv
      shellify
      ripgrep
      nil
      nh
      vim
      nil
      neovim
      nixfmt-rfc-style
      starship
      treefmt
      coreutils
    ];
    home.file = {
      "Music/Phoebe/lyricslist" = {
        source = ../../phoebelyrics/lyricslist;
      };
      ".config/wezterm" = {
        source = ../../.config/wezterm;
        recursive = true;
      };

    };

    home.sessionVariables = {
      #      BROWSER = "firefox";
      _JAVA_AWT_WM_NONREPARENTING = 1;
      WLR_NO_HARDWARE_CURSORS = 1;

    };

  };
  options = with lib; {
    packagenames = mkOption {
      type = types.str;
      default = "
          ";
    };
    packagecount = mkOption {
      type = types.int;
      default = 0;
      description = "
          package
          count
          ";
    };
  };

}
