{
  den,
  lib,
  ...
}:
{
  den.aspects.vpn-server = {
    includes = [ den.aspects.vpn ];

    vpn.role = "server";
    vpn.listenPort = 51820;

    os =
      { config, ... }:
      let
        cfg = config.vpn;
      in
      {
        networking.firewall.allowedUDPPorts = [ cfg.listenPort ];

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
