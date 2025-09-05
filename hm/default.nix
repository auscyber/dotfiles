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
    inputs.nix-index-database.homeModules.nix-index
    #    ../keybind_program
  ];

  config = {
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
    home.stateVersion = "23.11";
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
    programs.fish.enable = true;
    programs = {
      #      command-not-found.enable = true;
      direnv = {
        enable = true;
        nix-direnv = {
          enable = true;
        };
      };
      home-manager.enable = true;
      gpg.enable = true;

    };

    services.lorri = {
      enable = false;
    };
    programs.git = {
      enable = true;
      userName = "Ivy Pierlot";
      userEmail = "ivyp@outlook.com.au";
    };
    programs.eza = {
      enable = true;
      git = true;
      icons = "auto";
    };
    home.packages = with pkgs; [
      ivy-fetch
      devenv
      agenix
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
      ".config/nvim" = {
        source = ../.config/nvim;
        recursive = true;
      };
      ".config/wezterm" = {
        source = ../.config/wezterm;
        recursive = true;
      };
      ".config/wezterm/lume.lua" = {
        source = ../libs/lume.lua;
      };
      ".config/wezterm/fennel.lua" = {
        source = ../libs/fennel.lua;
      };
      ".config/starship.toml" = {
        source = ../.config/starship.toml;
      };

    };
    xdg.configFile."nvim/lua/treesitter_compiler.lua".text = ''
      			return "${pkgs.stdenv.cc}/bin/cc"
            	'';
    home.sessionVariables = {
      EDITOR = "nvim";
      editor = "$EDITOR";
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
