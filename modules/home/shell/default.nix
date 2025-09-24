{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.auscybernix.shell;
in
{
  options.auscybernix.shell = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable custom shell configuration.";
    };

  };

  config = lib.mkIf cfg.enable {
    home.file = {
      ".config/starship.toml" = {
        source = ../../../.config/starship.toml;
      };
    };
    programs.gh = {
      enable = true;
      package = pkgs.hello;
      settings = {

        git_protocol = "ssh";
      };

    };
    programs.nh.enable = true;
    programs = {
      ssh = {

        enable = true;
        enableDefaultConfig = true;
        matchBlocks = {
          "imflo.pet" = {
            forwardAgent = true;
          };
        };

      };
      direnv = {
        enable = true;
        nix-direnv = {
          enable = true;
        };
      };
      home-manager.enable = true;
      gpg.enable = true;

      git = {
        enable = true;
        userName = "Ivy Pierlot";
        userEmail = "ivyp@outlook.com.au";

      };
      eza = {
        enable = true;
        git = true;
        icons = "auto";
      };
    };

  };
}
