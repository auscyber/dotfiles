{ ... }:
# ── A MULTICAST GROUP IS A THING, NOT A FIELD ON EACH SPEAKER ──
#
# mDNS and SSDP each have several participants on one box -- avahi, the home
# automation stack and the media server all speak both -- and a socket is
# exclusive per host, per port, per interface (../base/capability-types.nix).
# So three aspects declaring 5353 is either three collisions or one accidental
# owner: before this, ONE of them declared it and the others' hole came from a
# declaration that had nothing to do with them, which would have closed
# silently the day that aspect moved.
#
# The group owns the port. A speaker INCLUDES the group, which is also the
# honest shape of the machine: a box should run one responder and have the
# rest register through it, not three listeners on one socket.
{
  den.aspects.mdns = {
    description = "The mDNS group on this host. Included by everything that speaks it.";

    gives =
      { self, ... }:
      {
        udp = _: {
          ports.mdns = {
            port = 5353;
            # A discovery protocol the lan cannot reach does nothing at all.
            expose = [ "lan-home" ];
            meta.description = "mDNS/Bonjour service discovery -- HomeKit, ESPHome, Matter, and share advertisement.";
          };
          carrier = self.name;
        };
      };

    reach = "host";
  };

  den.aspects.ssdp = {
    description = "The SSDP/UPnP group on this host. Included by everything that speaks it.";

    gives =
      { self, ... }:
      {
        udp = _: {
          ports.ssdp = {
            port = 1900;
            expose = [ "lan-home" ];
            meta.description = "SSDP/UPnP discovery -- how DLNA clients on the lan find a renderer or server.";
          };
          carrier = self.name;
        };
      };

    reach = "host";
  };
}
