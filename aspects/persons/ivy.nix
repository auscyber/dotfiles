{ den, ... }:
{
  # One file per human. The schema is ../base/person.nix; this is data.
  #
  # ── THE KEY IS THE HUMAN; `kanidm.name` IS THE ACCOUNT ──
  #
  # `ivy` is who this is. `auscyber` is what her account is called -- in
  # kanidm, in NSS, in samba's passdb and on auspc -- and it has to stay that
  # string: kanidm keys `provision.persons` by it, the live instance already
  # has it with enrolled passkeys and `idm_admins` membership, and a rename
  # there is a delete-and-recreate under `autoRemove`.
  #
  # Those were the same string until the two facts were separated, which is why
  # this file used to be called auscyber.nix and why the household's member was
  # too. Everything that resolves an ACCOUNT reads `kanidm.name`
  # (../services/access.nix, ../services/shares.nix, ../services/samba-users.nix,
  # ../services/kanidm.nix); everything that means the person reads the key.
  #
  # Note what is NOT here: the accounts this person owns. Each ACCOUNT names its
  # person (`users.<account>.person = den.persons.ivy`), never the reverse.
  # See ../base/person.nix for why -- the other direction deadlocked. Today that
  # is `ivypierlot` on the mac, `auscyber` on auspc and secondpc, and `ivy` on
  # the installer image: three logins, one human, one set of keys.
  den.persons.ivy = {
    preferredName = "Ivy";
    displayName = "Ivy Pierlot";
    # FIRST IS KANIDM'S PRIMARY, and that is why the external one stays at the
    # head of the list: it is the address every OIDC `email` claim already
    # carries, so Vikunja, Donetick, SplitPro and Nextcloud all match accounts
    # on it. `mail.primary` below is what decouples "which address is the
    # mailbox" from "which address kanidm calls primary".
    mailAddresses = [
      "ivyp@outlook.com.au"
      # LOCAL: ../services/mail.nix hosts ivymect.in, so this one is a mailbox.
      "ivy@ivymect.in"
    ];

    # The mailbox, and the opt-in that pushes it to whichever host serves mail
    # (../services/mail.nix -- the emission is stamped at this person's own
    # account scope, so nothing walks the registry).
    mail.primary = "ivy@ivymect.in";

    kanidm = {
      name = "auscyber";
      groups = [
        "media-users"
        "idm_admins"
      ];
    };

    # ── THE KEYS, HERE AND NOWHERE ELSE ──
    #
    # The private halves live in 1Password and never touch disk. Declared on
    # the human, so every account that names her trusts the same set --
    # ../security/main-ssh-key.nix delivers them to each one, for SSH login and
    # for pam_rssh sudo.
    #
    # A LIST because there is more than one of them in the vault; add the rest
    # here and they merge. This first entry is what ../security/_main-key.nix
    # held as a file-level constant attached to every account on every machine,
    # which could say neither "these are Ivy's" nor "that one is revoked".
    sshKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"
    ];

    # ── THE ONE PLAINTEXT ──
    #
    # `user-pwd/ivy-password` (../security/user-pwd.nix): the real login
    # password, already generated, and the value ../services/samba-users.nix
    # mints the NT hash from -- so the SMB credential and the human's password
    # are one credential rather than two that have to be kept in step.
    #
    # Pinned to the EXISTING secret rather than named after the person: a
    # rename means regenerating it, and ../../CLAUDE.md freezes that while the
    # master key is mid-recovery. Dropping this line is not cosmetic -- samba
    # falls back to minting `samba-password-auscyber`, which is a different
    # password and a secret that does not exist yet.
    passwordSecret = {
      scope = "user-pwd";
      key = "ivy-password";
    };

    includes = [ den.aspects.mailbox ];

    # Preferences that travel with the human are ASPECTS, not options -- see
    # `includes` in ../base/person.nix. e.g. a `tidal` aspect contributing a
    # Music Assistant provider would go here:
    #   includes = [ den.aspects.tidal ];

    # `enableUnix`/`gidNumber` deliberately absent. POSIX-extending a person is
    # one-way in kanidm and the number is baked into file ownership from the
    # moment it lands, so it has to match what the live instance and the live
    # filesystems already use. Read it off the box first.
  };
}
