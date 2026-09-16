{ den, ... }:
let
  # Used for both:
  #   1. pam_rssh sudo authentication (via /etc/authorized_keys/<u>.keys)
  #   2. OpenSSH login (via users.users.<u>.openssh.authorizedKeys.keys on nixos)
  #
  # Intentionally distinct from `hostPublicKey`: `hostPublicKey` is the
  # *on-system* identity (agenix encryption target). This one is *off-system* --
  # a credential the user authenticates *with*.
  mainKey = import ./_main-key.nix;
in
{
  den.aspects.main-ssh-key = {
    includes = [ den.aspects.pam-rssh ];

    provides.to-hosts = { user, ... }: {
      # pam_rssh authorized_keys file (sudo via ssh-agent), both classes.
      os.environment.etc."authorized_keys/${user.name}.keys".text = mainKey;
      # SSH login authorized key, nixos only (nix-darwin doesn't model this).
      nixos.users.users.${user.name}.openssh.authorizedKeys.keys = [ mainKey ];
    };
  };

  # Auto-attach to every user so the master key is always authorised for
  # both sudo and SSH login, no per-user declaration required.
  den.schema.user.includes = [ den.aspects.main-ssh-key ];
}
