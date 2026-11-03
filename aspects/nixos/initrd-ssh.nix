{ den, ... }:
# SSH into the initrd on port 2222, to type the ZFS passphrase from another
# machine. Its own generated host key, never the real one: the initrd sits
# unencrypted on /boot. Trusts the host owner's `sshKeys`. Logging in lands on
# the passphrase prompt.
{
  den.aspects.initrd-ssh = {
    includes = [ den.aspects.agenix-rekey ];

    nixos =
      { host, scoped, ... }:
      {
        # Public half beside it as `.pub`, for known_hosts.
        age.scoped.initrd-ssh.secrets.host-key.generator.script = "ssh_ed25519_pair";

        boot.initrd.network = {
          enable = true;
          ssh = {
            enable = true;
            port = 2222;
            hostKeys = [ scoped.initrd-ssh.secrets.host-key.path ];
            authorizedKeys = host.owner.sshKeys or [ ];
          };
        };

        boot.initrd.systemd = {
          network = {
            enable = true;
            networks."10-wired" = {
              matchConfig.Type = "ether";
              networkConfig.DHCP = "yes";
            };
          };
          users.root.shell = "/bin/systemd-tty-ask-password-agent";
        };
      };
  };
}
