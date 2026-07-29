{
  den,
  lib,
  ...
}:
let
  remoteNames = [
    "gdrive"
    "onedrive-personal"
  ];
in
{
  den.aspects.rclone = {
    includes = [ den.aspects.agenix-rekey ];

    # Nested under this aspect's own scope by aspects/security/age-scope.nix, so
    # each remote lands at `age.secrets."rclone/<remote>"` -- the key here is
    # just the remote name, no hand-written prefix.
    secrets = lib.listToAttrs (
      map (name: {
        inherit name;
        value.rekeyFile = ../../secrets/rclone + "/${name}.age";
      }) remoteNames
    );

    homeManager =
      {
        config,
        scoped,
        ...
      }:
      let
        secretFor = name: scoped.rclone.access.${name}.path;
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
