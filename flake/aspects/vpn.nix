{ den, ... }:
{
  # ── VPN aspect ────────────────────────────────────────────────────────────────
  # NixOS: WireGuard VPN via wg-quick.
  # Include in a host aspect to join the WireGuard mesh.
  den.aspects.vpn = {
    nixos =
      {
        config,
        lib,
        pkgs,
        flakeConfig,
        systemIdentifier,
        ...
      }:
      let
        cfg = config.auscybernix.vpn;
      in
      {
        options.auscybernix.vpn = {
          enable = lib.mkEnableOption "WireGuard VPN configuration";
          ipAddress = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            description = "WireGuard VPN IP address assigned to this host.";
          };
          serverpubkey = lib.mkOption {
            type = lib.types.nullOr lib.types.path;
            default = null;
            description = "Path to the WireGuard server public key file.";
          };
          clientpubkey = lib.mkOption {
            type = lib.types.nullOr lib.types.path;
            default = null;
            description = "Path to this host's WireGuard public key file.";
          };
          endpoint = lib.mkOption {
            type = lib.types.str;
            default = "pierlot.com.au:51820";
            description = "WireGuard endpoint (host:port).";
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
          auscybernix.vpn.serverpubkey =
            ../../systems/x86_64-linux/secondpc/wg_private_key.pub;
          auscybernix.vpn.clientpubkey =
            builtins.path {
              path = config.age.rekey.generatedSecretsDir + "/wireguard_key.pub";
            };
          networking.wg-quick.interfaces.wg0 = {
            address = [ "${cfg.ipAddress}/24" ];
            privateKeyFile = config.age.secrets.wireguard_key.path;
            dns = [ "1.1.1.1" "10.100.0.1" ];
            peers = [
              {
                publicKey = builtins.readFile cfg.serverpubkey;
                allowedIPs = [ "10.100.0.0/24" ];
                endpoint = cfg.endpoint;
                persistentKeepalive = 25;
              }
            ];
          };
        };
      };
  };
}
