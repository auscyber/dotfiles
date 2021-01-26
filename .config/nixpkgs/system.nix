{config, lib, pkgs, ... }:
with lib;

let 
  cfg = config.system.auscyber;
in {
  options = {
    system.auscyber = {
        enable = mkEnableOption "AusCyber System";
        fullSetup = mkOption {
          type = types.bool;
          default = false;
          description = "Configure bulk packages";
        }; 

    };

  };
config = mkIf cfg.enable {
  home.packages = 


};



  }
