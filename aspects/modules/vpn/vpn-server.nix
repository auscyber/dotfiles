{ den, ... }:
{
  den.aspects.vpn-server = {
    includes = [ den.aspects.vpn ];

    vpn.role = "server";

    os = {
      networking.firewall.allowedUDPPorts = [ 51820 ];
      networking.nat = {
        enable = true;
        externalInterface = "eth0";
        internalInterfaces = [ "wg0" ];
      };

      boot.kernel.sysctl = {
        "net.ipv4.ip_forward" = 1;
        "net.ipv6.conf.all.forwarding" = 1;
      };

      networking.wg-quick.interfaces.wg0.listenPort = 51820;
    };
  };
}
