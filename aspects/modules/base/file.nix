{ lib, ... }:

with lib;
let
  userFileOptions =
    { user, ... }:
    {
      options = {
        flakeFolder = lib.mkOption {
          type = lib.types.str;
          default = "/Users/${user.name}/dotfiles";
          defaultText = lib.literalExpression ''"/Users/''${user.name}/dotfiles"'';
          description = "Folder where this user's flake.nix is checked out.";
        };
      };
    };
  attrsWith' =
    placeholder: elemType:
    types.attrsWith {
      inherit elemType placeholder;
    };
  settingsOption = {
    description = ''
      Declare systemd-tmpfiles rules to create, delete, and clean up volatile
      and temporary files and directories.

      Even though the service is called `*tmp*files` you can also create
      persistent files.
    '';
    example = {
      "10-mypackage" = {
        "/var/lib/my-service/statefolder".d = {
          mode = "0755";
          user = "root";
          group = "root";
        };
      };
    };
    default = { };
    type = attrsWith' "config-name" (
      attrsWith' "path" (
        attrsWith' "tmpfiles-type" (
          types.submodule (
            { name, config, ... }:
            {
              options.type = mkOption {
                type = types.str;
                default = name;
                defaultText = "‹tmpfiles-type›";
                example = "d";
                description = ''
                  The type of operation to perform on the file.

                  The type consists of a single letter and optionally one or more
                  modifier characters.

                  Please see the upstream documentation for the available types and
                  more details:
                  {manpage}`tmpfiles.d(5)`
                '';
              };
              options.mode = mkOption {
                type = types.str;
                default = "-";
                example = "0755";
                description = ''
                  The file access mode to use when creating this file or directory.
                '';
              };
              options.user = mkOption {
                type = types.str;
                default = "-";
                example = "root";
                description = ''
                  The user of the file.

                  This may either be a numeric ID or a user/group name.

                  If omitted or when set to `"-"`, the user and group of the user who
                  invokes systemd-tmpfiles is used.
                '';
              };
              options.group = mkOption {
                type = types.str;
                default = "-";
                example = "root";
                description = ''
                  The group of the file.

                  This may either be a numeric ID or a user/group name.

                  If omitted or when set to `"-"`, the user and group of the user who
                  invokes systemd-tmpfiles is used.
                '';
              };
              options.age = mkOption {
                type = types.str;
                default = "-";
                example = "10d";
                description = ''
                  Delete a file when it reaches a certain age.

                  If a file or directory is older than the current time minus the age
                  field, it is deleted.

                  If set to `"-"` no automatic clean-up is done.
                '';
              };

            }
          )
        )
      )
    );
  };
in
{
  den.schema.user = userFileOptions;

}
