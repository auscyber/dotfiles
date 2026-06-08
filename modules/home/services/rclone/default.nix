{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.auscybernix.services.rclone;

  remoteNames = [
    "gdrive"
    "onedrive-personal"
    #    "onedrive-school"
  ];

  secretFor = name: config.age.secrets."rclone/secrets/${name}".path;
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

    age.secrets = lib.listToAttrs (
      map (name: {
        name = "rclone/secrets/${name}";
        value = {
          rekeyFile = ./${name}.age;
        };
      }) remoteNames
    );

    programs.rclone = {
      enable = true;

      remotes = {
        gdrive = {
          config = {
            type = "drive";
            scope = "drive";
          };
          secrets.token = secretFor "gdrive";
          mounts."" = {
            enable = true;
            mountPoint = "${config.home.homeDirectory}/mnts/gdrive";
          };
        };

        onedrive-personal = {
          config = {
            type = "onedrive";
            drive_id = "7BEE81A1BAA61272";
            drive_type = "personal";
          };
          secrets.token = secretFor "onedrive-personal";
          mounts."" = {
            enable = true;
            mountPoint = "${config.home.homeDirectory}/mnts/onedrive-personal";
          };
        };

      };
    };

  };
}
