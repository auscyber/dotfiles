{ config
, pkgs
, system
, lib
, modulesPath
, ...
}:
{
  config = {
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
    manual.manpages.enable = true;
    programs.zsh.enable = true;
    programs = {
      command-not-found.enable = true;
      direnv = {
        enable = true;
        nix-direnv = {
          enable = true;
        };
      };
      home-manager.enable = true;

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
      enableZshIntegration = true;
    };
    home.packages = with pkgs; [
      shellify
      ripgrep
      nil
      nh
      vim
      neovim
      rnix-lsp
      nixfmt-rfc-style
      starship
      treefmt
      coreutils
    ];
    home.file = {
      ".local/bin/fetch" = {
        source = ../../fetch;
      };
      ".config/nvim" = {
        source = ../../.config/nvim;
        recursive = true;
      };
      ".config/wezterm" = {
        source = ../../.config/wezterm;
        recursive = true;
      };
      ".config/wezterm/lume.lua" = {
        source = ../../libs/lume.lua;
      };
      ".config/wezterm/fennel.lua" = {
        source = ../../libs/fennel.lua;
      };
      ".config/starship.toml" = {
        source = ../../.config/starship.toml;
      };

    };
    xdg.configFile."nvim/lua/compiler.lua".text = ''
      			return "${pkgs.stdenv.cc}/bin/cc"
            	'';

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
