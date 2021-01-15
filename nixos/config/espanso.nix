{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.espanso;
in {
  disabledModules = [<nixos/nixos/modules/services/desktops/espanso.nix> ];
  options = {
    services.espanso = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable espanso
        '';
      };

      package = mkOption {
        type = types.package;
        default = pkgs.espanso;
        description = ''
          Espanso package to use
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.espanso = {
      description = "Espanso daemon";
      path = with pkgs; [ cfg.package libnotify xclip ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/espanso daemon";
        Restart = "on-failure";
      };
    };

    environment.systemPackages = [ cfg.package ];
  };
  }
