{
  config,
  pkgs,
  lib,
  flakeConfig,
  systemIdentifier,
  ...
}:
let
  cfg = config.auscybernix.vpn;
in
{
  options.auscybernix.vpn = {
    enable = lib.mkEnableOption "Enable Wireguard VPN configuration.";
    ipAddress = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      description = "Wireguard VPN IP address";
    };
    pubkey = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Wireguard public key";
    };
    endpoint = lib.mkOption {
      type = lib.types.str;
      default = "pierlot.com.au:51820";
      description = "Wireguard VPN endpoint";
    };

  };
  config = lib.mkIf cfg.enable {
    age.secrets.wireguard_key = {
      generator.script =
        { pkgs, file, ... }:
        ''
          priv=$(${pkgs.wireguard-tools}/bin/wg genkey)
          ${pkgs.wireguard-tools}/bin/wg pubkey <<< "$priv" > ${
            lib.escapeShellArg (lib.removeSuffix ".age" file + ".pub")
          }
          echo "$priv"
            		  	'';

    };
    auscybernix.vpn.ipAddress =
      flakeConfig.flake.auscybernix.vpn.configMap."${systemIdentifier}".ipAddress;
    auscybernix.vpn.pubkey = # lib.escapeShellArg (
      builtins.path { path = config.age.rekey.generatedSecretsDir; } + ("/wireguard_key.pub")
    #)
    ;
    networking.wg-quick.interfaces.vpn = {
      address = [ cfg.ipAddress ];
      privateKeyFile = config.age.secrets.wireguard_key.path;
      dns = [
        "1.1.1.1"
        "10.100.0.1"
      ];
      peers = [
        {
          publicKey = cfg.pubkey;
          allowedIPs = [
            "10.100.0.0/32"
          ];
          endpoint = cfg.endpoint;
        }
      ];
    };
  };

}
