{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib)
    mkIf
    mkOption
    types
    optionalString
    toLower
    ;

  cfg = config.services.pia-wireguard;

  # Pinned server list (evaluated at build time) for deterministic server selection.
  serverListJSON =
    if cfg.serverSelection.enable then
      builtins.readFile (
        pkgs.fetchurl {
          url = cfg.serverSelection.serverListURL;
          sha256 = cfg.serverSelection.serverListHash;
        }
      )
    else
      null;

  parsedServerList = if cfg.serverSelection.enable then builtins.fromJSON serverListJSON else { };

  # Simple region/server chooser (first region matching location substring).
  chosenServer =
    let
      loc = lib.toLower (cfg.serverSelection.location or "");
      regions = (parsedServerList.regions or [ ]);
      filtered =
        if cfg.serverSelection.location == null || cfg.serverSelection.location == "" then
          regions
        else
          lib.filter (
            r:
            let
              id = toLower (r.id or "");
              name = toLower (r.name or "");
            in
            lib.strings.contains loc id || lib.strings.contains loc name
          ) regions;
      # Optional port-forwarding filter if requested (at selection time — only influences picking, not runtime).
      pfFiltered =
        if cfg.portForwarding.requireAtSelection then
          lib.filter (r: (r.port_forward or false) == true) filtered
        else
          filtered;
      firstRegion = lib.head (pfFiltered);
      wgServers = if firstRegion == null then [ ] else (firstRegion.servers.wireguard or [ ]);
      server = lib.head wgServers;
    in
    if server == null then
      null
    else
      {
        regionId = firstRegion.id;
        regionName = firstRegion.name;
        ip = server.ip;
        cn = server.cn;
      };

  serverIPFinal =
    if cfg.serverIP != null then
      cfg.serverIP
    else if chosenServer != null then
      chosenServer.ip
    else
      null;

  serverHostFinal =
    if cfg.serverHostname != null then
      cfg.serverHostname
    else if chosenServer != null then
      chosenServer.cn
    else
      null;

  # Sanity assertion if selection requested but nothing found.
  assertions = lib.optionals cfg.serverSelection.enable [
    {
      assertion = serverIPFinal != null && serverHostFinal != null;
      message = "PIA wireguard: server selection failed (no matching region/server). Supply serverIP/serverHostname manually or adjust selection criteria.";
    }
  ];

  runtimeDir = "/var/run/pia-wireguard";
  stateDir = "/var/lib/pia-wireguard"; # for persistent key if we auto-generate
  interfaceName = cfg.interfaceName;

  # Build-time static wg config (dummy peer until runtime mutation).
  # We do NOT include PrivateKey content here to avoid leaking secrets into store;
  # instead we read from privateKeyFile during the runtime patch step.
  staticConfig = pkgs.writeText "pia-static-${interfaceName}.conf" ''
    [Interface]
    # PrivateKey will be injected runtime (wg set) from ${
      cfg.privateKeyFile or "${stateDir}/private.key"
    }
    # Temporary dummy address; real /32 set after API call.
    Address = 169.254.254.1/32
    # (Optional) DNS entries inserted directly if you want static DNS
    ${lib.concatMapStrings (d: "DNS = ${d}\n") (
      if cfg.dns.enable && !cfg.dns.useFromApi then cfg.dns.servers else [ ]
    )}

    [Peer]
    # Placeholder peer; will be replaced by wg set with real public key & endpoint.
    PublicKey = 0000000000000000000000000000000000000000000=
    AllowedIPs = ${lib.concatStringsSep "," cfg.allowedIPs}
    Endpoint = 127.0.0.1:1
    PersistentKeepalive = 25
  '';

  serviceScript = pkgs.writeShellScript "pia-wireguard-launch.sh" ''
        set -euo pipefail
        PATH=${
          lib.makeBinPath [
            cfg.wireguardPackage
            pkgs.curl
            pkgs.jq
            pkgs.coreutils
            pkgs.gnugrep
            pkgs.iproute2
          ]
        }

        IFACE="${interfaceName}"
        RUNTIME_DIR="${runtimeDir}"
        STATE_DIR="${stateDir}"
        TOKEN_FILE="${cfg.tokenFile}"
        SERVER_IP="${serverIPFinal}"
        SERVER_HOST="${serverHostFinal}"
        CA_CERT=${
          pkgs.fetchurl {
            url = cfg.caCertURL;
            sha256 = cfg.caCertHash;
          }
        }
        PORT_FWD=${if cfg.portForwarding.enable then "true" else "false"}
        PORT_SCRIPT=${
          if cfg.portForwarding.enable then
            pkgs.fetchurl {
              url = cfg.portForwarding.scriptURL;
              sha256 = cfg.portForwarding.scriptHash;
            }
          else
            "/dev/null"
        }
        USE_API_DNS=${if cfg.dns.enable && cfg.dns.useFromApi then "true" else "false"}
        ALLOWED_IPS="${lib.concatStringsSep "," cfg.allowedIPs}"
        PRIVATE_KEY_FILE="${cfg.privateKeyFile or "${stateDir}/private.key"}"

        mkdir -p "$RUNTIME_DIR" "$STATE_DIR"
        chmod 700 "$RUNTIME_DIR" "$STATE_DIR"

        log() {
          echo "[pia-wireguard] $(date -u +'%Y-%m-%dT%H:%M:%SZ') $*"
        }

        if [ ! -r "$TOKEN_FILE" ]; then
          log "Token file $TOKEN_FILE missing/unreadable"
          exit 1
        fi

        if [ ! -f "$PRIVATE_KEY_FILE" ]; then
          ${lib.optionalString (cfg.privateKeyFile == null) ''
            log "Generating persistent private key at $PRIVATE_KEY_FILE"
            umask 077
            ${cfg.wireguardPackage}/bin/wg genkey > "$PRIVATE_KEY_FILE"
          ''}
        fi
        chmod 600 "$PRIVATE_KEY_FILE"

        PRIVATE_KEY=$(cat "$PRIVATE_KEY_FILE")
        PUB_KEY=$(echo "$PRIVATE_KEY" | wg pubkey)

        if [ -z "$SERVER_IP" ] || [ -z "$SERVER_HOST" ]; then
          log "Server IP/Host not set (build-time selection failed?)."
          exit 1
        fi

        # Bring interface up with static config if not already
        if ! wg show "$IFACE" >/dev/null 2>&1; then
          log "Bringing up interface with static config"
          WG_QUICK_USERSPACE_IMPLEMENTATION= \
            WG_QUICK_CONFIG_FILE="${staticConfig}" \
            wg-quick up ${staticConfig}
        else
          log "Interface already up; proceeding to dynamic patch"
        fi

        TOKEN=$(cat "$TOKEN_FILE")
        log "Calling PIA addKey API via $${SERVER_HOST} ($${SERVER_IP})"
        WG_JSON=$(curl -s -G \
            --connect-to "${SERVER_HOST}::${SERVER_IP}:" \
            --cacert "$CA_CERT" \
            --data-urlencode "pt=$TOKEN" \
            --data-urlencode "pubkey=$PUB_KEY" \
            "https://${SERVER_HOST}:1337/addKey" || true)

        STATUS=$(echo "$WG_JSON" | jq -r '.status // empty')
        if [ "$STATUS" != "OK" ]; then
          log "API status not OK; got: $WG_JSON"
          exit 2
        fi

        PEER_IP=$(echo "$WG_JSON" | jq -r '.peer_ip')
        SERVER_KEY=$(echo "$WG_JSON" | jq -r '.server_key')
        SERVER_PORT=$(echo "$WG_JSON" | jq -r '.server_port')
        API_DNS=$(echo "$WG_JSON" | jq -r '.dns_servers[0] // empty')

        if [ -z "$PEER_IP" ] || [ -z "$SERVER_KEY" ] || [ -z "$SERVER_PORT" ]; then
          log "Incomplete API response."
          exit 3
        fi

        # Apply private key & peer info
        log "Applying live WireGuard parameters"
        wg set "$IFACE" private-key <(echo "$PRIVATE_KEY")
        wg set "$IFACE" peer "$SERVER_KEY" endpoint "${SERVER_IP}:$SERVER_PORT" persistent-keepalive 25 allowed-ips "$ALLOWED_IPS"

        # Adjust interface address (macOS: rely on ifconfig via wg-quick? On Darwin, wg-quick gave us a utun; we can replace address.)
        # Remove existing v4 /32 (best-effort) then add new one
        # Use ifconfig for Darwin
        currentAddr=$(ifconfig "$IFACE" 2>/dev/null | grep 'inet ' | awk '{print $2}' || true)
        if [ -n "$currentAddr" ] && [ "$currentAddr" != "$${PEER_IP%/*}" ]; then
          log "Replacing interface address $currentAddr with $PEER_IP"
          ifconfig "$IFACE" inet "$${PEER_IP%/*}" "$${PEER_IP%/*}" netmask 255.255.255.255
        else
          log "Setting interface address $PEER_IP"
          ifconfig "$IFACE" inet "$${PEER_IP%/*}" "$${PEER_IP%/*}" netmask 255.255.255.255
        fi

        if [ "$USE_API_DNS" = "true" ] && [ -n "$API_DNS" ]; then
          log "Setting resolver to API DNS $API_DNS (scutil)"
          # macOS-specific resolver domain override (basic). Users may prefer a proper DNS profile or networksetup.
          scutil <<EOF
    open
    d.init
    d.add ServerAddresses * $API_DNS
    set State:/Network/Service/${IFACE}/DNS
    quit
    EOF
        fi

        if [ "$PORT_FWD" = "true" ]; then
          if [ -x "$PORT_SCRIPT" ]; then
            log "Running port forwarding script"
            PIA_TOKEN="$TOKEN" PF_GATEWAY="$SERVER_IP" PF_HOSTNAME="$SERVER_HOST" "$PORT_SCRIPT" || log "Port forwarding failed."
          else
            log "Port forwarding enabled but script missing"
          fi
        fi

        log "Interface up with live parameters. (Static build-time config file unchanged.)"
        # Daemon just idles; no periodic renewal since you required immutability.
        # WARNING: This will stop working after the addKey registration expires (~24h).
        while sleep 3600; do :; done
  '';
in
{
  options.services.pia-wireguard = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable PIA WireGuard (static build-time config + runtime patch).";
    };

    interfaceName = mkOption {
      type = types.str;
      default = "pia";
      description = "WireGuard interface name.";
    };

    tokenFile = mkOption {
      type = types.path;
      description = "Path to PIA token file (outside Nix store).";
    };

    privateKeyFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Path to persistent WireGuard private key outside the store. If null, a key is generated once in /var/lib/pia-wireguard/private.key.";
    };

    serverIP = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Explicit server IP (overrides build-time selection).";
    };

    serverHostname = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Explicit server hostname (CN) (overrides selection).";
    };

    allowedIPs = mkOption {
      type = types.listOf types.str;
      default = [ "0.0.0.0/0" ];
      description = "AllowedIPs for peer.";
    };

    dns = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable DNS handling.";
      };
      useFromApi = mkOption {
        type = types.bool;
        default = true;
        description = "If true, set DNS from API response; else use static dns.servers list at build-time.";
      };
      servers = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Static DNS servers when useFromApi=false.";
      };
    };

    portForwarding = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Run upstream port_forwarding.sh after connection.";
      };
      requireAtSelection = mkOption {
        type = types.bool;
        default = true;
        description = "When selecting server at build time, restrict to PF-capable regions.";
      };
      scriptURL = mkOption {
        type = types.str;
        default = "https://raw.githubusercontent.com/pia-foss/manual-connections/e956c57849a38f912e654e0357f5ae456dfd1742/port_forwarding.sh";
        description = "Pinned URL for port_forwarding.sh.";
      };
      scriptHash = mkOption {
        type = types.str;
        default = "sha256-Dm1c0dZ48koxuR6aqX6N2yPxS4poZcxAYP5ifjTp3e0=";
        description = "SHA256 for port_forwarding.sh.";
      };
    };

    serverSelection = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable build-time server selection from pinned server list.";
      };
      location = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Location substring for region (case-insensitive).";
      };
      serverListURL = mkOption {
        type = types.str;
        default = "https://serverlist.piaservers.net/vpninfo/servers/v6";
        description = "Server list URL (must keep hash updated).";
      };
      serverListHash = mkOption {
        type = types.str;
        default = "<REPLACE_WITH_HASH>";
        description = "SHA256 for the server list JSON (must be supplied).";
      };
    };

    caCertURL = mkOption {
      type = types.str;
      default = "https://raw.githubusercontent.com/pia-foss/manual-connections/e956c57849a38f912e654e0357f5ae456dfd1742/ca.rsa.4096.crt";
      description = "PIA CA certificate URL.";
    };
    caCertHash = mkOption {
      type = types.str;
      default = "sha256-E5ljUVr6t6N7GaxzwoMPmxjSPdZRhqUWLWyd4InENnE=";
      description = "SHA256 of CA cert.";
    };

    wireguardPackage = mkOption {
      type = types.package;
      default = pkgs.wireguard-tools;
      description = "wireguard-tools package.";
    };
  };

  config = mkIf cfg.enable {
    assertions = assertions;

    environment.systemPackages = [
      cfg.wireguardPackage
    ];

    # Place the static config in /etc (build-time, immutable until rebuild).
    environment.etc."wireguard/${interfaceName}.conf".source = staticConfig;

    # LaunchDaemon that uses the static config but patches live interface state.
    launchd.daemons."pia-wireguard" = {
      script = serviceScript;
      serviceConfig = {
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Background";
        StandardOutPath = "/var/log/pia-wireguard.log";
        StandardErrorPath = "/var/log/pia-wireguard.log";
      };
    };

    system.activationScripts.piaWireguard = ''
      mkdir -p ${runtimeDir} ${stateDir}
      chmod 700 ${runtimeDir} ${stateDir}
    '';
  };
}
