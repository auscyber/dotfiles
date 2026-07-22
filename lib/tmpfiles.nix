# Shared schema for the cross-platform `auscybernix.tmpfiles` option.
#
# The shape is deliberately identical to NixOS's `systemd.tmpfiles.settings`
# (see tmpfiles.d(5)):
#
#   auscybernix.tmpfiles.settings.<config-name>.<path>.<tmpfiles-type> = {
#     mode = "0770"; user = "root"; group = "nixbld"; age = "-";
#   };
#
# On NixOS `extraModules/nixos/tmpfiles` hands the settings to systemd-tmpfiles
# verbatim. On darwin `extraModules/darwin/tmpfiles` compiles them into a
# nix-darwin activation script, since nix-darwin has no tmpfiles equivalent.
#
# Lives outside `aspects/` (dendritic auto-import) and outside `extraModules/`
# (whose top-level dirs are read as *class names* by aspects/utils/extraModules.nix),
# so this file is only ever pulled in by an explicit `import`.
{ lib }:
let
  attrsWith' =
    placeholder: elemType:
    lib.types.attrsWith {
      inherit elemType placeholder;
    };

  entryType = lib.types.submodule (
    { name, ... }:
    {
      options.type = lib.mkOption {
        type = lib.types.str;
        default = name;
        defaultText = "‹tmpfiles-type›";
        example = "d";
        description = ''
          The type of operation to perform on the file: a single letter,
          optionally followed by modifier characters. See {manpage}`tmpfiles.d(5)`.

          The darwin backend implements a subset -- see
          `extraModules/darwin/tmpfiles/default.nix`.
        '';
      };
      options.mode = lib.mkOption {
        type = lib.types.str;
        default = "-";
        example = "0755";
        description = ''
          File access mode to use when creating this file or directory.
          `"-"` leaves the mode untouched.
        '';
      };
      options.user = lib.mkOption {
        type = lib.types.str;
        default = "-";
        example = "root";
        description = ''
          The user of the file, as a numeric ID or a name.

          `"-"` leaves ownership untouched. (On NixOS systemd-tmpfiles reads this
          as "the user invoking systemd-tmpfiles"; since darwin activation always
          runs as root, "untouched" is the closer analogue and is what both
          backends here do.)
        '';
      };
      options.group = lib.mkOption {
        type = lib.types.str;
        default = "-";
        example = "nixbld";
        description = ''
          The group of the file, as a numeric ID or a name.
          `"-"` leaves group ownership untouched.
        '';
      };
      options.age = lib.mkOption {
        type = lib.types.str;
        default = "-";
        example = "10d";
        description = ''
          Delete the file once it is older than this. `"-"` disables clean-up.

          NOTE: unimplemented on darwin -- there is no systemd-tmpfiles timer to
          hang it off. The darwin backend raises an eval error rather than
          silently ignoring a non-`"-"` age.
        '';
      };
    }
  );

  settingsType = attrsWith' "config-name" (attrsWith' "path" (attrsWith' "tmpfiles-type" entryType));
in
{
  inherit entryType settingsType;

  settingsOption = lib.mkOption {
    type = settingsType;
    default = { };
    example = {
      "10-ccache" = {
        "/var/cache/ccache".d = {
          mode = "0770";
          user = "root";
          group = "nixbld";
        };
      };
    };
    description = ''
      Declare tmpfiles.d-style rules to create and own files and directories.

      Despite the name these are not necessarily *temporary* -- this is the
      normal way to declare a persistent state directory that a service needs.

      Implemented by systemd-tmpfiles on NixOS and by a generated activation
      script on darwin.
    '';
  };

  # settings -> flat list of { config, path, type, mode, user, group, age }.
  # Sorted by config name then path so the generated darwin script is stable
  # across evaluations (attrset iteration order is already lexicographic, but
  # being explicit keeps the flattening obvious).
  flatten =
    settings:
    lib.concatMap (
      config:
      lib.concatMap (
        path: lib.mapAttrsToList (_type: entry: entry // { inherit config path; }) settings.${config}.${path}
      ) (lib.attrNames settings.${config})
    ) (lib.attrNames settings);

  # tmpfiles.d(5) spells "leave this alone" as "-"; empty/null are accepted too
  # so that a caller can write `user = null` without tripping the str type.
  isUnset = v: v == null || v == "-" || v == "";
}
