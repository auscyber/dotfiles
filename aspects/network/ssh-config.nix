{ den, lib, ... }:
let
  inherit (den.lib.policy) include;

  # Every machine: den hosts and `fleet.external`, not containers.
  machines = lib.filterAttrs (_: e: (e.parent or null) == null) den.lib.capability.hostsOf;
  machineFor = e: machines.${e.name or ""} or e;

  # Every other machine's sshd, dialled from this one: the carrier is the best
  # wire the two share, the key and the accounts are the giver's.
  logins =
    from:
    lib.filterAttrs (_: d: d != null) (
      lib.mapAttrs (
        n: _:
        den.lib.capability.dial "ssh" {
          name = "openssh";
          where = c: (c.host.name or n) == n;
        } from
      ) (removeAttrs machines [ (from.name or "") ])
    );

  namesOf = n: d: [ n ] ++ d.link.aliases;

  localExtraSocket =
    pkgs:
    if pkgs.stdenv.hostPlatform.isDarwin then
      "/private/var/run/org.nix-community.home.gpg-agent/S.gpg-agent.extra"
    else
      "/run/user/1000/gnupg/S.gpg-agent.extra";

  # `ssh <machine>` lands in the account of `person` there, with both agents
  # forwarded.
  sshFor = from: person: {
    homeManager =
      { pkgs, ... }:
      {
        programs.ssh.settings = lib.listToAttrs (
          lib.concatLists (
            lib.mapAttrsToList (
              n: d:
              let
                account = d.link.accounts.${person.name};
                block = {
                  hostname = d.address;
                  inherit (account) user;
                  forwardAgent = true;
                  RemoteForward = {
                    bind.address = account.gpgAgentSocket;
                    host.address = localExtraSocket pkgs;
                  };
                }
                // lib.optionalAttrs (d.port != 22) { inherit (d) port; };
              in
              map (a: lib.nameValuePair a block) (namesOf n d)
            ) (lib.filterAttrs (_: d: d.link.accounts ? ${person.name}) (logins from))
          )
        );
      };
  };
in
{
  den.aspects.openssh.includes = [ den.aspects.ssh-client ];

  den.aspects.ssh-client = {
    # `os`, so every account on the machine trusts the same keys.
    os =
      { host, ... }:
      {
        programs.ssh.knownHosts = lib.mapAttrs (n: d: {
          hostNames = namesOf n d ++ [ d.address ];
          publicKey = d.link.hostKey;
        }) (lib.filterAttrs (_: d: d.link.hostKey != null) (logins (machineFor host)));
      };

    provides.to-users =
      { host, user, ... }:
      lib.optionalAttrs ((user.person or null) != null) (sshFor (machineFor host) user.person);
  };

  # A standalone home on a machine in the fleet gets the same, as its person.
  den.policies.ssh-client-home =
    {
      home ? null,
      ...
    }:
    let
      machine = machines.${home.hostName or ""} or null;
    in
    lib.optional (home != null && machine != null && (home.person or null) != null) (
      include (
        {
          name = "ssh-client-home";
        }
        // sshFor machine home.person
      )
    );

  den.schema.home.includes = [ den.policies.ssh-client-home ];
}
