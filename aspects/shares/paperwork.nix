{ den, ... }:
# One person's own share. Same model as ./photos.nix, one includer -- so
# nothing here says "private": the member set IS the include list, and a list of
# one is a private folder.
#
# Backed by a group folder with a one-member group rather than a directory in
# the account's own `files`, and that is a Nextcloud limitation rather than a
# preference: a folder inside primary storage is not declarable at all (nothing
# but a `files:scan` would ever notice it, see ../services/nextcloud.nix's note
# on the file index), while a group folder is `occ groupfolders:create` and is
# reconciled on every activation. One mechanism for both cases also means the
# share keeps working unchanged the day a second person is added to it.
{
  den.aspects.paperwork.takes.share = {
    displayName = "Paperwork";
    # Read-only is `permissions = [ "read" ]`; these four are groupfolders' own
    # words (Command/Group.php `PERMISSION_VALUES`) and are already the take's
    # default (../base/capability-types.nix), spelled out here only because this
    # is the file a reader looks in to find them.
    permissions = [
      "read"
      "write"
      "share"
      "delete"
    ];
  };

  den.persons.ivy.includes = [ den.aspects.paperwork ];
}
