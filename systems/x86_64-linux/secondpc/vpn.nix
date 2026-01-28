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
	owner = "systemd-network";
	group = "systemd-network";
    generator.script =
      { pkgs, file, ... }:
      ''
        privkey="$(${pkgs.wireguard-tools}/bin/wg genkey)"
		echo "$privkey" | ${pkgs.wireguard-tools}/bin/wg pubkey > ${lib.escapeShellArg (lib.removeSuffix ".age" file) + ".pub"}
		echo "$privkey"
      '';
  };
  networking.defaultGateway = {
  address = "192.168.0.1";
  interface = "br0";
  };
  systemd.network.wait-online.ignoredInterfaces = [ "br0" ];
  networking.useNetworkd = true;
  systemd.network = {
    enable = true;

    netdevs = {
      "50-wg0" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = "wg0";
          MTUBytes = "1500";
        };
        wireguardConfig = {
		RouteTable = "main";

        PrivateKeyFile = config.age.secrets."wg_private_key".path;
          ListenPort = 51820;
        };

wireguardPeers = lib.flip lib.mapAttrsToList flakeConfig.flake.auscybernix.vpn.configMap (
          name: peerConfig: {
            PublicKey = peerConfig.pubkey;
#            Name = peerConfig.description;
            AllowedIPs = [ "${peerConfig.ipAddress}/32" ];
          }
        );
      };
    };
    networks.wg0 = {
      matchConfig.Name = "wg0";
      address = ["10.100.0.1/24"];
      networkConfig = {
        IPMasquerade = "ipv4";
        IPv4Forwarding = true;
      };
    };
	networks."40-br0" = {
	extraConfig = ''
	[Link]
	RequiredForOnline=routable
	'';

	};

  };
  }
