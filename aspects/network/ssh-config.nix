{ den, lib, ... }:
let
  inherit (import ./_lib.nix { inherit lib den; }) clientNames tunnelIpByName;

  # Metadata the VPN module doesn't carry: the login user on each peer, and
  # the peer's own UID (needed for its gpg-agent socket path when forwarding
  # GPG over the tunnel). Neither is declared anywhere else in the repo —
  # `uid` is assumed to be the default first-normal-user UID and should be
  # checked with `id -u auscyber` on `auspc`/`secondpc` if forwarding breaks.
  hostMeta = {
    auspc = {
      user = "auscyber";
      uid = 1000;
    };
    secondpc = {
      user = "auscyber";
      uid = 1000;
    };
  };

  # gpg-agent's extra socket (`services.gpg-agent.enableExtraSocket`, see
  # ../security/gpg.nix), mirrored to the peer's real agent socket path via
  # `RemoteForward` + `StreamLocalBindUnlink` (../security/openssh.nix).
  # Path formula matches home-manager's `services.gpg-agent` module: Linux
  # sockets live under the systemd runtime dir (per-uid), Darwin's live under
  # a single fixed launchd-managed directory.
  localExtraSocket =
    { pkgs, host, ... }:
    if pkgs.stdenv.hostPlatform.isDarwin then
      "/private/var/run/org.nix-community.home.gpg-agent/S.gpg-agent.extra"
    else
      "/run/user/${toString (hostMeta.${host.name}.uid or 1000)}/gnupg/S.gpg-agent.extra";
in
{
  den.aspects.vpn.includes = [ den.aspects.vpn-ssh-config ];

  den.aspects.vpn-ssh-config = {
    provides.to-users =
      { host, ... }:
      {
        homeManager =
          { pkgs, ... }:
          {
            programs.ssh.settings = lib.genAttrs (clientNames host.name) (peerName: {
              hostname = tunnelIpByName peerName;
              forwardAgent = true;
              RemoteForward = {
                bind.address = "/run/user/${toString (hostMeta.${peerName}.uid or 1000)}/gnupg/S.gpg-agent";
                host.address = localExtraSocket { inherit pkgs host; };
              };
            }
            // lib.optionalAttrs (builtins.hasAttr peerName hostMeta) {
              user = hostMeta.${peerName}.user;
            });
          };
      };
  };
}
