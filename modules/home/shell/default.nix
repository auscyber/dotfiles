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
    programs.zoxide.enable = true;
    programs.fzf.enable = true;
    programs.nix-your-shell = {
      enable = true;
      nix-output-monitor.enable = true;
    };
    programs.nh.enable = true;

    programs = {
      ssh = {

        enable = true;
        enableDefaultConfig = false;
        matchBlocks = {
          "imflo.pet" = {
            forwardAgent = true;
#            extraOptions = {
#              "RemoteForward" =
#                "/run/user/1001/gnupg/S.gpg-agent ${config.launchd.agents.gpg-agent.config.Sockets.Extra.SockPathName}";
#            };
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
        settings = {
          user = {
            name = "Ivy Pierlot";
            email = "ivyp@outlook.com.au";
          };
        };
      };
      eza = {
        enable = true;
        git = true;
        icons = "auto";
      };
    };

  };
}
