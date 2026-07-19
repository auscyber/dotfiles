{
  den,
  lib,
  ...
}:
let
  # 6 hex chars of sha256 → integer in 2..254. Server lives at .1.
  hexToInt = s: (builtins.fromTOML "v=0x${s}").v;
  hostOctet =
    name: 2 + lib.mod (hexToInt (lib.substring 0 6 (builtins.hashString "sha256" name))) 253;
  pubKeyFile = name: ../../../../secrets/generated + "/${name}/wireguard_key.pub";
  pubKey = name: lib.removeSuffix "\n" (builtins.readFile (pubKeyFile name));

  # A host peers over wireguard iff the generator has produced a host-level
  # keypair for it. Hosts without the vpn aspect have no such file.
  hasKey = name: builtins.pathExists (pubKeyFile name);
  allHostNames = lib.concatMap lib.attrNames (lib.attrValues (den.hosts or { }));
  clientNames = name: lib.filter (n: n != name && hasKey n) allHostNames;

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

  # Shared by the `os` (wg-quick) and `nixos` (networkd) modules below, which
  # are separate classes and so cannot share a `let`.
  tunnelIp =
    cfg: host:
    if cfg.ipAddress != null then
      cfg.ipAddress
    else if cfg.role == "server" then
      "10.100.0.1"
    else
      "10.100.0.${toString (hostOctet host.name)}";

  # The server routes each client on its hash-derived address. A client that
  # overrides vpn.ipAddress would desync from this; the server has no view into
  # another host's module config, only its name.
  tunnelPeers =
    cfg: host:
    if cfg.role == "server" then
      map (name: {
        publicKey = pubKey name;
        allowedIPs = [ "10.100.0.${toString (hostOctet name)}/32" ];
      }) (clientNames host.name)
    else
      [
        {
          publicKey = pubKey cfg.serverHost;
          endpoint = cfg.endpoint;
          allowedIPs = cfg.allowedIPs;
          persistentKeepalive = 25;
        }
      ];

  # networkd spells peers with capitalised keys and takes Endpoint /
  # PersistentKeepalive only on the client side, where `peers` supplies them.
  toNetworkdPeer =
    p:
    {
      PublicKey = p.publicKey;
      AllowedIPs = p.allowedIPs;
    }
    // lib.optionalAttrs (p ? endpoint) { Endpoint = p.endpoint; }
    // lib.optionalAttrs (p ? persistentKeepalive) { PersistentKeepalive = p.persistentKeepalive; };

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
      backend = lib.mkOption {
        type = lib.types.enum [
          "wg-quick"
          "networkd"
        ];
        default = "wg-quick";
        description = ''
          How to realise the tunnel. `networkd` renders it as a systemd-networkd
          netdev + network and forces `networking.useNetworkd`, which also pulls
          in systemd-resolved; `wg-quick` uses the standalone wg-quick unit on
          top of scripted networking.
        '';
      };
      interface = lib.mkOption {
        type = lib.types.str;
        default = "wg0";
      };
      listenPort = lib.mkOption {
        type = lib.types.nullOr lib.types.port;
        default = null;
        description = "UDP port to listen on. Servers must set this; clients dial out.";
      };
      natExternalInterface = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Uplink to masquerade tunnel traffic out of, for the `wg-quick` backend
          only. The `networkd` backend uses `IPMasquerade` on the tunnel's
          .network instead and ignores this.
        '';
      };
      ipAddress = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "This host's address inside the tunnel.";
      };
      endpoint = lib.mkOption {
        type = lib.types.str;
        default = "121.200.22.213:51820";
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

    # wg-quick exists on both NixOS and nix-darwin (the latter drives it from
    # launchd), so it is safe to declare from the shared `os` class.
    os =
      {
        config,
        host,
        ...
      }:
      let
        cfg = config.vpn;
      in
      {
        options.vpn = lib.mkOption {
          type = lib.types.submodule vpnSubmodule;
          default = { };
        };

        config = lib.mkIf (cfg.backend == "wg-quick") {
          networking.wg-quick.interfaces.${cfg.interface} = {
            address = [ "${tunnelIp cfg host}/24" ];
            privateKeyFile = config.age.secrets.wireguard_key.path;
            peers = tunnelPeers cfg host;
          }
          // lib.optionalAttrs (cfg.listenPort != null) { inherit (cfg) listenPort; };
        };
      };

    # networkd is NixOS-only. This cannot live in `os`: den forwards that class
    # into nix-darwin too, and `mkIf false` does not shield an option the target
    # module system never declares — nix-darwin rejects the mere presence of a
    # `networking.useNetworkd` definition.
    nixos =
      {
        config,
        host,
        ...
      }:
      let
        cfg = config.vpn;
        ip = tunnelIp cfg host;
        peers = tunnelPeers cfg host;
      in
      {
        config = lib.mkIf (cfg.backend == "networkd") {
          networking.useNetworkd = true;

          # networkd opens the key as the systemd-network user; wg-quick runs
          # as root and does not care.
          age.secrets.wireguard_key = {
            owner = "systemd-network";
            group = "systemd-network";
          };

          systemd.network.netdevs."50-${cfg.interface}" = {
            netdevConfig = {
              Kind = "wireguard";
              Name = cfg.interface;
              MTUBytes = "1500";
            };
            wireguardConfig = {
              PrivateKeyFile = config.age.secrets.wireguard_key.path;
              RouteTable = "main";
            }
            // lib.optionalAttrs (cfg.listenPort != null) { ListenPort = cfg.listenPort; };
            wireguardPeers = map toNetworkdPeer peers;
          };

          systemd.network.networks.${cfg.interface} = {
            matchConfig.Name = cfg.interface;
            address = [ "${ip}/24" ];
          }
          // lib.optionalAttrs (cfg.role == "server") {
            networkConfig = {
              IPMasquerade = "ipv4";
              IPv4Forwarding = true;
            };
          };
        };
      };
  };
}
