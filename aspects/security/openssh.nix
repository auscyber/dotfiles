{ den, lib, ... }:
let
  # home-manager's `services.gpg-agent` paths. Linux's is per uid; 1000 is the
  # first normal user's.
  agentSocket = {
    darwin = "/private/var/run/org.nix-community.home.gpg-agent/S.gpg-agent";
    nixos = "/run/user/1000/gnupg/S.gpg-agent";
  };
in
# ── SSH AS A CAPABILITY ──
#
# It was `services.openssh.enable = true` and nothing else, while three other
# files each worked out where ssh is and who to connect as: ../network/ssh-config.nix
# builds every shortcut alias from `tunnelIpByName` (a wireguard address,
# hardcoded), ../base/builders.nix had `hostName` on each builder record, and
# `authorizedKeys` is set per host. Those are all the same two facts -- where
# sshd answers and which account you land in -- so they are declared once here
# and resolved like anything else.
{
  # ── A NARROWING OF `tcp`, AND THE PROBE THAT GOES WITH IT ──
  #
  # ssh is a tcp service, so the address comes from the carrier the two ends
  # share and the port from the giver. What ssh adds is the account and the
  # host key -- the two things a client needs that the socket does not say.
  #
  # ⚠ "SOMETIMES NEEDS TCP" IS LITERALLY TRUE, and it was measured today:
  # Tailscale SSH intercepts port 22 on the tailnet, so a connect to that
  # address answers from tailscaled and never reaches sshd -- `nc -z` said OPEN
  # while ssh said "tailnet policy does not permit you to ssh to this node".
  # The tcp endpoint is then not sshd's at all. That is a fact about the
  # CARRIER rather than about ssh, which is why the narrowing stays and the
  # probe below is what distinguishes the two cases.
  den.capabilities.ssh = {
    meta.description = "An ssh login: where sshd answers, which account you land in, and its host key.";
    includes = [ den.capabilities.tcp ];

    options = {
      # Never a host-wide default: whoever dials lands in their own account.
      user = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "The account of the dialing entity's person on this host. Null for a dialer with no person, or a person with no account here.";
      };

      accounts = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.submodule {
            options = {
              user = lib.mkOption {
                type = lib.types.str;
                description = "The account name.";
              };
              gpgAgentSocket = lib.mkOption {
                type = lib.types.str;
                description = "Where that account's gpg-agent listens, for a client's `RemoteForward`.";
              };
            };
          }
        );
        default = { };
        description = "Each person's account on this host, keyed by person name.";
      };

      aliases = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Other names a client may call this host by.";
      };

      hostKey = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "The host's public key LINE, for pinning. Null where it is not known yet.";
      };
    };

    # ── CAN IT ACTUALLY BE USED, WHICH A LISTENING SOCKET DOES NOT SAY ──
    #
    # `tcp`'s own probe has already run (./capability.nix's `probesFor` walks
    # the closure broadest-first), so the socket is known to answer. This is
    # the part that was measured to differ: an authentication attempt.
    #
    # ⚠ IT CAN CAUSE WHAT IT REPORTS. A failed authentication is what trips a
    # fail2ban jail, which is how port 22 on one box in this fleet came to be
    # refused -- it reads as `No route to host` on 22 while 80 and 443 stay
    # open. So it runs last, only once the cheap probes pass, and `BatchMode`
    # keeps it from ever prompting.
    probe =
      { link, ... }:
      "ssh -o BatchMode=yes -o ConnectTimeout=5 ${
        lib.optionalString (link.user != null) "${link.user}@"
      }${link.address} true";
  };

  den.aspects.openssh = {
    # ── WHERE SSHD ANSWERS AND WHO YOU LAND AS ──
    # The port is declared so ../base/firewall.nix opens it from the same
    # declaration everything else uses, and so `socket-exclusions` can see it.
    # No `expose`: resolution adds the wires consumers actually reach it over
    # (./capability.nix's `exposeFor`), which is what keeps a lan-only box from
    # advertising ssh to the tunnel.
    gives =
      {
        self,
        host ? { },
        ...
      }:
      {
        ssh =
          {
            peer ? { },
            ...
          }:
          rec {
            name = self.name;
            ports.ssh = {
              port = 22;
              meta.description = "sshd. Who may reach it is resolution's answer, not a hand-opened hole.";
            };
            accounts = lib.mapAttrs' (
              n: u:
              lib.nameValuePair u.person.name {
                user = n;
                gpgAgentSocket = agentSocket.${host.class or "nixos"} or agentSocket.nixos;
              }
            ) (lib.filterAttrs (_: u: (u.person or null) != null) (host.users or { }));
            user = accounts.${(peer.person or { }).name or ""}.user or null;
            aliases = host.aliases or [ ];
            hostKey = host.hostPublicKey or null;
          };
      };

    # ANY host in the fleet may dial it; who may actually log in is the
    # server's own business (`authorizedKeys`), not the resolver's.
    reach = "fleet";

    nixos = {
      services.openssh = {
        enable = true;
        settings.StreamLocalBindUnlink = "yes";
        settings.AllowAgentForwarding = true;
        forwardX11 = true;
      };
      security.sudo.enable = true;
    };
  };
}
