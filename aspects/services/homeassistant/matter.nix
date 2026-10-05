{ den, ... }:
# What used to be the matter-server add-on.
let
  # The matter-server websocket Home Assistant talks to. This aspect runs
  # it, so this aspect declares it.
  matterPort = 5580;
in
{
  # Its own aspect for the reason ./mosquitto.nix records: a file that writes
  # into `den.aspects.homeassistant.nixos` has nowhere to declare the port it
  # listens on, because that aspect's `gives` lives in ./core.nix.
  den.aspects.homeassistant.includes = [ den.aspects.matter-server ];

  # The component, contributed by the thing that runs the server -- see
  # ./mosquitto.nix's note.
  #
  # `thread` COMES WITH IT. It was a static entry in ./core.nix, and it is not
  # independent of this aspect: Thread is the transport Matter devices join
  # over, HA's `thread` integration exists to manage the border router's
  # dataset, and a Matter fabric without it is Wi-Fi/ethernet devices only. So
  # the aspect that runs the fabric contributes both.
  den.aspects.matter-server.takes.home-assistant.extraComponents = [
    "matter"
    "thread"
  ];

  den.aspects.matter-server.gives =
    { self, ... }:
    {
      tcp = _: {
        name = self.name;
        ports.matter = {
          port = matterPort;
          meta.description = "python-matter-server's websocket API, which Home Assistant dials over the loopback on this same box.";
          # NOT EXPOSED. Home Assistant dials it over the loopback on this same
          # box, and opening it would publish fabric CONTROL to the flat /16.
          expose = [ ];
        };
      };

      # ── COMMISSIONING, WHICH IS THIS SERVER'S PORT AND NOT HA'S ──
      #
      # 5540 was declared in ./core.nix's `gives.udp` and ALSO opened by a
      # `networking.firewall.allowedUDPPorts` literal in this file -- the same
      # port in two files, one of them the wrong aspect. Nothing in Home
      # Assistant listens on it: python-matter-server does the commissioning
      # and operational discovery, so the port belongs to the aspect that runs
      # it, and ../../base/firewall.nix opens it from the `expose` below.
      udp = _: {
        name = self.name;
        ports.matter = {
          port = 5540;
          meta.description = "Matter itself: commissioning and operational discovery with devices on the house lan.";
          # Devices being commissioned are on the house lan, so a port the lan
          # cannot reach commissions nothing.
          expose = [ "lan-home" ];
        };
      };
    };

  den.aspects.matter-server.nixos = {
    # `openFirewall` is left at its default (false) and the port is not opened:
    # the websocket on `matterPort` is for HA, which is on this host, and
    # nixpkgs' module renders `openFirewall` as a plain
    # `allowedTCPPorts = [ cfg.port ]` with no address restriction -- so
    # turning it on would publish fabric control to the whole flat /16.
    #
    # HA's `matter` integration is config-flow-only, so the URL it dials
    # (`ws://127.0.0.1:${toString matterPort}/ws`) is typed in the UI once and
    # is not declarable from here. It must be the loopback address and not
    # `home.<domain>`: that leg would otherwise run out through the cloudflare
    # tunnel and back for a service on the same box, which is the hazard
    # ../nextcloud.nix:327 and ../../hosts/secondpc/sso.nix:385 document.
    services.matter-server = {
      enable = true;
      port = matterPort;
      # logLevel = "debug";
    };
  };
}
