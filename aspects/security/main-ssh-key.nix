{ den, lib, ... }:
# THE KEYS AN ACCOUNT TRUSTS, READ OFF THE HUMAN BEHIND IT.
#
# Used for both:
#   1. pam_rssh sudo authentication (via /etc/authorized_keys/<u>.keys)
#   2. OpenSSH login (via users.users.<u>.openssh.authorizedKeys.keys on nixos)
#
# Intentionally distinct from `hostPublicKey`: `hostPublicKey` is the
# *on-system* identity (agenix encryption target), declared per machine. These
# are *off-system* -- credentials the human authenticates *with*, declared once
# on the person (../base/person.nix's `sshKeys`) and delivered to every account
# that names them. So `ivypierlot` on the mac and `auscyber` on auspc trust the
# same set, and neither host names a key.
#
# ── WHY THE FALLBACK EXISTS, AND WHAT IT IS ──
#
# This aspect is attached to EVERY user (`den.schema.user.includes` below), and
# not every account has a person -- a builder account, a service account. Those
# used to get ./_main-key.nix, which was the only key there was, and dropping
# it from them would be a lockout delivered as a refactor.
#
# So: a person's own keys when they declare any, and the master key otherwise.
# That makes the person's list AUTHORITATIVE where it exists -- removing a key
# from it revokes that key, which a `++ mainKey` floor would have quietly
# prevented -- while an account with no human behind it keeps exactly what it
# had.
let
  mainKey = import ./_main-key.nix;

  keysFor =
    user:
    let
      declared = user.person.sshKeys or [ ];
    in
    if declared == [ ] then [ mainKey ] else lib.unique declared;
in
{
  den.aspects.main-ssh-key = {
    provides.to-hosts =
      { user, ... }:
      let
        keys = keysFor user;
      in
      {
        # pam_rssh authorized_keys file (sudo via ssh-agent), both classes.
        os.environment.etc."authorized_keys/${user.name}.keys".text = lib.concatMapStrings (
          k: k + "\n"
        ) keys;
        # SSH login authorized key, nixos only (nix-darwin doesn't model this).
        nixos.users.users.${user.name}.openssh.authorizedKeys.keys = keys;
      };

    includes = [ den.aspects.pam-rssh ];
  };

  # Auto-attach to every user so the human's keys are always authorised for
  # both sudo and SSH login, no per-user declaration required.
  den.schema.user.includes = [ den.aspects.main-ssh-key ];
}
