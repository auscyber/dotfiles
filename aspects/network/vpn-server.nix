{
  den,
  lib,
  ...
}:
let
  # The hub's listen port, named once so `gives.udp` and `vpn.listenPort`
  # cannot disagree.
  listenPort = 51820;
in
{
  den.aspects.vpn-server = {
    includes = [ den.aspects.vpn ];

    vpn.role = "server";
    vpn.listenPort = listenPort;

    # ── THE HUB'S ENDPOINT, DECLARED RATHER THAN OPENED GLOBALLY ──
    #
    # This was `networking.firewall.allowedUDPPorts = [ cfg.listenPort ]`
    # below -- nixpkgs' pseudo-interface `default`, no `-i`, so it also sat on
    # the tunnel it is the endpoint for.
    #
    # `lan-home` is the house lan AND the uplink: clients dial a fixed public
    # endpoint and the router forwards it to this host's lan address, so the
    # packet arrives on the lan interface. There is no "internet" carrier to
    # name instead. A hub not cabled to this segment gets ../base/firewall.nix's
    # unresolved-carrier assertion, which is the right way to find out.
    gives =
      { self, ... }:
      {
        udp = _: {
          name = self.name;
          ports.wireguard = {
            port = listenPort;
            meta.description = "The wireguard hub's endpoint. Clients dial `vpn.endpoint` and the house router forwards it here, so the packet lands on the lan interface.";
            expose = [ "lan-home" ];
          };
          carrier = self.name;
        };
      };

    os =
      { config, ... }:
      let
        cfg = config.vpn;
      in
      {
        # Masquerading for tunnel clients. Under the networkd backend this is
        # `IPMasquerade` on the tunnel's .network (see vpn.nix), so the nat
        # module is left disabled and only records the internal interface.
        # Under wg-quick there is no such hook, so the nat module has to be
        # pointed at the real uplink — which is host-specific, hence the option
        # rather than a hardcoded guess.
        networking.nat = {
          internalInterfaces = [ cfg.interface ];
        }
        // lib.optionalAttrs (cfg.backend == "wg-quick" && cfg.natExternalInterface != null) {
          enable = true;
          externalInterface = cfg.natExternalInterface;
        };

        boot.kernel.sysctl = {
          "net.ipv4.ip_forward" = 1;
          "net.ipv6.conf.all.forwarding" = 1;
        };
      };
  };
}
