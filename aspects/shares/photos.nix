{ den, ... }:
# One file per share. The model is ../services/shares.nix; this is the share.
#
# A share aspect carries nothing but its `share` emission -- no nixos, no
# homeManager -- because a person's `includes` are delivered to EVERY account
# they have (../base/person.nix), so anything else in here would land on four
# machines that do not serve files.
#
# The includers are listed BELOW, in this file, rather than as a line in
# ../persons/<p>.nix. `includes` is a list option so either end may declare the
# edge, and declaring it here is what makes "drop the share" a single deletion:
# the folder and every grant of it are the same file. The cost is that
# ../persons/ivy.nix no longer shows everything that person has, which is
# why the aspect name has to say what it is.
{
  # NOT NAMED AGAIN. A take is keyed by the TAKING ASPECT, so this attribute
  # path IS the share's name -- where the quirk this replaces pooled emissions
  # with no aspect identity in them and every share had to repeat its own name.
  den.aspects.photos.takes.share = {
    displayName = "Photos";
    quota = "200G";

    # No `path`. groupfolders keeps its bytes under
    # `appdata_<instanceid>/__groupfolders/<id>` and has no say in it, so a path
    # here would be honoured by nothing and created by nothing; samba is the
    # backend that would use one, and it falls back to its own share root.
  };

  # ── `photos`, NOT `family-photos` ──
  #
  # This household is not a family, and the old name said it was. The sibling
  # share is `paperwork`, so a plain noun is also the convention here: what the
  # folder holds, with the includers below saying whose it is.
  #
  # ⚠ RENAMING A SHARE IS NOT FREE, and this one has already been provisioned.
  # The name is the identity everywhere downstream: ../services/access.nix mints
  # the kanidm group `share-<n>`, so `share-family-photos` exists and
  # `share-photos` does not; samba keys its section on it; and nextcloud's
  # groupfolder is bound to the group. So a deploy after this rename creates the
  # new group and share and leaves the old group behind with the bytes still
  # under the old groupfolder id. Moving the contents and deleting
  # `share-family-photos` is a one-off by hand, not something this file can do.
  #
  # Who has it. Two names here instead of one is the whole difference between
  # "my folder" and "ours" -- there is no `shared = true` anywhere, because the
  # number of includers already says it.
  #
  # `jonesy` is included even though she has no account on secondpc: since
  # ../services/samba-users.nix became a capability she does not need one -- her
  # MEMBERSHIP of this household resolves the take -- so the only thing still
  # missing is her NT hash, which needs a `gen-secrets` that cannot run while
  # the master key is mid-recovery.
  den.persons.ivy.includes = [ den.aspects.photos ];
  den.persons.jonesy.includes = [ den.aspects.photos ];
}
