{
  den,
  lib,
  ...
}:
let
  inherit (import ./_lib.nix { inherit lib; })
    registry
    clientNames
    hasKey
    tunnelIpByName
    ;

  # The login user on each peer, and the peer's own UID (needed for its
  # gpg-agent socket path when forwarding GPG over the tunnel). Both come from
  # ../hosts/_registry.nix, which is readable from every partition — the reason
  # this used to be a hand-kept table here is that a darwin host cannot see a
  # NixOS host's `den.hosts` entry to ask.
  hostMeta = registry;

  # gpg-agent's extra socket (`services.gpg-agent.enableExtraSocket`, see
  # ../security/gpg.nix), mirrored to the peer's real agent socket path via
  # `RemoteForward` + `StreamLocalBindUnlink` (../security/openssh.nix).
  # Path formula matches home-manager's `services.gpg-agent` module: Linux
  # sockets live under the systemd runtime dir (per-uid), Darwin's live under
  # a single fixed launchd-managed directory.
  # The same formula on the PEER's side, for `RemoteForward`'s bind address.
  # Which half of it applies is a fact about the peer's platform, and until the
  # registry existed there was no way to ask: a peer was only ever a name here,
  # so every peer got the Linux path and forwarding into a Mac bound a
  # `/run/user/<uid>` that does not exist on macOS at all.
  peerAgentSocket =
    peerName:
    let
      meta = hostMeta.${peerName} or { };
    in
    if lib.hasSuffix "darwin" (meta.system or "") then
      "/private/var/run/org.nix-community.home.gpg-agent/S.gpg-agent"
    else
      "/run/user/${toString (meta.uid or 1000)}/gnupg/S.gpg-agent";

  localExtraSocket =
    {
      pkgs,
      host,
      ...
    }:
    if pkgs.stdenv.hostPlatform.isDarwin then
      "/private/var/run/org.nix-community.home.gpg-agent/S.gpg-agent.extra"
    else
      "/run/user/${toString (hostMeta.${host.name}.uid or 1000)}/gnupg/S.gpg-agent.extra";
in
{
  den.aspects.vpn.includes = [ den.aspects.vpn-ssh-config ];

  den.aspects.vpn-ssh-config = {
    # Host keys, pinned from the same registry the aliases come from.
    #
    # Without this `ssh secondpc` is trust-on-first-use: the first connection
    # from a fresh machine (or after `~/.ssh/known_hosts` is cleared, or after
    # the host is reinstalled) asks, and a prompt is the good case — the bad
    # one is the stale entry that makes ssh refuse outright. The key is
    # already declared: it is the same `hostPublicKey` agenix-rekey encrypts
    # that host's secrets to, so if it were wrong the host could not decrypt
    # its own secrets at boot. Pinning it costs nothing and is what makes the
    # alias usable from a machine that has never dialled the peer before.
    #
    # `os`, so it lands in /etc/ssh/ssh_known_hosts for every account on the
    # machine: home-manager's `programs.ssh` has no `knownHosts` of its own,
    # only `userKnownHostsFile`, while both NixOS and nix-darwin carry the
    # system-wide option.
    os = { host, ... }: {
      programs.ssh.knownHosts = lib.mapAttrs (peerName: meta: {
        # Both spellings of the peer: the bare alias the config below defines,
        # and the tunnel address it resolves to — ssh checks known_hosts
        # against whatever it actually connected to. The tunnel address only
        # for an actual peer: an external machine in the registry (faggot.sh)
        # is dialled by DNS, and `tunnelIpByName` would invent an address for
        # it that nothing listens on.
        hostNames = [
          peerName
        ]
        ++ lib.optional (hasKey peerName) (tunnelIpByName peerName);
        publicKey = meta.hostPublicKey;
      }) (lib.filterAttrs (peerName: meta: meta ? hostPublicKey && peerName != host.name) registry);
    };

    provides.to-users = { host, ... }: {
      homeManager = { pkgs, ... }: {
        programs.ssh.settings = lib.genAttrs (clientNames host.name) (
          peerName:
          {
            hostname = tunnelIpByName peerName;
            forwardAgent = true;
            RemoteForward = {
              bind.address = peerAgentSocket peerName;
              host.address = localExtraSocket { inherit pkgs host; };
            };
          }
          // lib.optionalAttrs (builtins.hasAttr peerName hostMeta) {
            user = hostMeta.${peerName}.user;
          }
        );
      };
    };
  };
}
