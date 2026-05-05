{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.auscybernix.services.rclone;
in
{

  options.auscybernix.services.rclone = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to enable the rclone service.";
    };
  };

  config = lib.mkIf cfg.enable {

    programs.rclone = {
      enable = true;
      remotes.gdrive = {
        config = {
          type = "drive";

        };
      };
    };

  };
}
