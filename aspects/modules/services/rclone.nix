{ den, lib, ... }:
let
  remoteNames = [
    "gdrive"
    "onedrive-personal"
  ];
in
{
  den.aspects.rclone = {
    includes = [ den.aspects.agenix-rekey ];

    # Route into age.secrets.<name> via the agenix-rekey `secrets` forward class.
    secrets = lib.listToAttrs (
      map (name: {
        name = "rclone/secrets/${name}";
        value.rekeyFile = ../../../secrets/rclone + "/${name}.age";
      }) remoteNames
    );

    homeManager =
      { config, ... }:
      let
        secretFor = name: config.age.secrets."rclone/secrets/${name}".path;
      in
      {
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
  };
}
