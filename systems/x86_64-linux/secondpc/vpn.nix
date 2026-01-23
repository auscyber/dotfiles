{
  config,
  pkgs,
  inputs,
  lib,
  flakeConfig,
  ...
}:
let
in
{

  networking.nat.internalInterfaces = [ "wg0" ];
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
  };
  age.secrets."wg_private_key" = {
    rekeyFile = ./wg_private_key.age;
    generator.script =
      { pkgs, ... }:
      ''
        ${pkgs.wireguard-tools}/bin/wg genkey
      '';
  };
  networking.wireguard = {
    enable = true;
    interfaces = {
      wg0 = {
        ips = [ "10.100.0.1/24" ];

        listenPort = 51820;
        postSetup = ''
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';

        # This undoes the above command
        postShutdown = ''
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';
        privateKeyFile = config.age.secrets."wg_private_key".path;
        peers = lib.flip lib.mapAttrsToList flakeConfig.flake.auscybernix.vpn.configMap (
          name: peerConfig: {
            publicKey = peerConfig.pubkey;
            name = peerConfig.description;
            allowedIPs = [ peerConfig.ipAddress ];
          }
        );

      };
    };
  };

}
