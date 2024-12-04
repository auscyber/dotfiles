{ config, pkgs, system, lib, modulesPath, ... }:
{
  config = {
    home.stateVersion = "23.11";
    nix =
      {
        #        package = pkgs.nixUnstable;
        settings =
          {
            experimental-features = [
              "nix-command"
              "flakes"
            ];
          };
      };
    manual.manpages.enable = false;
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
    home.packages = with pkgs; [
      rnix-lsp
      nixfmt-rfc-style
      starship
      eza
    ];

    xdg.configFile."
          nvim/lua/compiler.lua
          ".text = ''
      			return "${pkgs.stdenv.cc}/bin/cc
          "
            	'';

  };
  options = with lib;{
    packagenames = mkOption
      {
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


