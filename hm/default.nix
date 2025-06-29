{
  config,
  pkgs,
  system,
  lib,
  modulesPath,
  ...
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
    home.shell.enableZshIntegration = true;
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
      enableZshIntegration = true;
    };
    home.packages = with pkgs; [
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
      ".local/bin/fetch" = {
        source = ../fetch;
      };
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
