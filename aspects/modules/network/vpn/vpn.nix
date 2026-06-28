{ den, lib, ... }:
let
  # 6 hex chars of sha256 → integer in 2..254. Server lives at .1.
  hexToInt = s: (builtins.fromTOML "v=0x${s}").v;
  hostOctet =
    name: 2 + lib.mod (hexToInt (lib.substring 0 6 (builtins.hashString "sha256" name))) 253;
  clientPubKey = name: ../../secrets/generated + "/${name}/wireguard_key.pub";

  vpn-class =
    {
      class,
      aspect-chain,
      ...
    }:
    den.batteries.forward {
      each = [
        { system = "nixos"; }
        { system = "darwin"; }
      ];
      fromClass = _: "vpn";
      intoClass = p: p.system;
      intoPath = _: [ "vpn" ];
      fromAspect = _item: lib.head aspect-chain;
      adapterModule = vpnSubmodule;
      adaptArgs = lib.id;
    };

  vpnSubmodule = {
    options = {
      role = lib.mkOption {
        type = lib.types.enum [
          "client"
          "server"
        ];
        default = "client";
        description = "Whether this host is a wireguard client or the server.";
      };
      interface = lib.mkOption {
        type = lib.types.str;
        default = "wg0";
      };
      ipAddress = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "This host's address inside the tunnel.";
      };
      endpoint = lib.mkOption {
        type = lib.types.str;
        default = "pierlot.com.au:51820";
      };
      serverHost = lib.mkOption {
        type = lib.types.str;
        default = "secondpc";
      };
      allowedIPs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "10.100.0.0/24" ];
      };
    };
  };
in
{
  den.aspects.agenix-rekey.age.generators.wireguard_pair =
    {
      lib,
      file,
      pkgs,
      ...
    }:
    ''
      priv=$(${pkgs.wireguard-tools}/bin/wg genkey)
      ${pkgs.wireguard-tools}/bin/wg pubkey <<< "$priv" \
        > ${lib.escapeShellArg (lib.removeSuffix ".age" file + ".pub")}
      echo "$priv"
    '';

  den.aspects.vpn-secrets = {
    includes = [ den.aspects.agenix-rekey ];
    secrets.wireguard_key.generator.script = "wireguard_pair";
  };

  den.aspects.vpn = {
    includes = [
      den.aspects.vpn-secrets
      vpn-class
    ];

    vpn = { };

    os =
      { config, host, ... }:
      let
        cfg = config.vpn;
        ip =
          if cfg.ipAddress != null then
            cfg.ipAddress
          else if cfg.role == "server" then
            "10.100.0.1"
          else
            "10.100.0.${toString (hostOctet host.name)}";
      in
      {
        options.vpn = lib.mkOption {
          type = lib.types.submodule vpnSubmodule;
          default = { };
        };
        config.networking.wg-quick.interfaces.${cfg.interface} = {
          address = [ "${ip}/24" ];
          privateKeyFile = config.age.secrets.wireguard_key.path;
        };
      };
  };
}
