{
  config,
  lib,
  pkgs,
  options,
  ...
}:
with lib;
let
  cfg = config.services.pia-vpn;
  isDarwin = lib.attrsets.hasAttrByPath [ "environment" "darwinConfig" ] options;

  # Darwin-specific configuration
  darwinConfig = {
    workingDir = "/var/lib/pia-vpn";
    cacheDir = "/var/cache/pia-vpn";
    runtimeDir = "/var/run";
  };

  # NixOS-specific configuration
  linuxConfig = {
    workingDir = "/var/lib/pia-vpn";
    cacheDir = "/var/cache/pia-vpn";
    runtimeDir = "/run";
  };

  # Use appropriate config based on system
  systemConfig = if isDarwin then darwinConfig else linuxConfig;

  # Common script for both platforms
  commonScript = pkgs.writeShellScript "shell.sh" ''
    set -e
    printServerLatency() {
      serverIP="$1"
      regionID="$2"
      regionName="$(echo ''${@:3} |
        sed 's/ false//' | sed 's/true/(geo)/')"
      time=$(LC_NUMERIC=en_US.utf8 curl -o /dev/null -s \
        --connect-timeout ${toString cfg.maxLatency} \
        --write-out "%{time_connect}" \
        http://$serverIP:443)
      if [ $? -eq 0 ]; then
        >&2 echo Got latency ''${time}s for region: $regionName
        echo $time $regionID $serverIP
      fi
    }

    source "${cfg.environmentFile}"
    export -f printServerLatency

    echo Fetching regions...
    serverlist='https://serverlist.piaservers.net/vpninfo/servers/v4'
    allregions=$((curl --no-progress-meter -m 5 "$serverlist" || true) | head -1)

    region="$(echo $allregions |
                jq --arg REGION_ID "${cfg.region}" -r '.regions[] | select(.id==$REGION_ID)')"
    if [ -z "''${region}" ]; then
      echo Determining region...
      filtered="$(echo $allregions | jq -r '.regions[]
                | .servers.meta[0].ip+" "+.id+" "+.name+" "+(.geo|tostring)')"
      best="$(echo "$filtered" | xargs -I{} bash -c 'printServerLatency {}' |
              sort | head -1 | awk '{ print $2 }')"
      if [ -z "$best" ]; then
        >&2 echo "No region found with latency under ${toString cfg.maxLatency} s. Stopping."
        exit 1
      fi
      region="$(echo $allregions |
                jq --arg REGION_ID "$best" -r '.regions[] | select(.id==$REGION_ID)')"
    fi
    echo Using region $(echo $region | jq -r '.id')

    meta_ip="$(echo $region | jq -r '.servers.meta[0].ip')"
    meta_hostname="$(echo $region | jq -r '.servers.meta[0].cn')"
    wg_ip="$(echo $region | jq -r '.servers.wg[0].ip')"
    wg_hostname="$(echo $region | jq -r '.servers.wg[0].cn')"
    echo "$region" > ${systemConfig.workingDir}/region.json

    echo Fetching token from $meta_ip...
    tokenResponse="$(curl --no-progress-meter -m 5 \
      -u "$PIA_USER:$PIA_PASS" \
      --connect-to "$meta_hostname::$meta_ip" \
      --cacert "${cfg.certificateFile}" \
      "https://$meta_hostname/authv3/generateToken" || true)"
    if [ "$(echo "$tokenResponse" | jq -r '.status' || true)" != "OK" ]; then
      >&2 echo "Failed to generate token. Stopping."
      exit 1
    fi
    token="$(echo "$tokenResponse" | jq -r '.token')"

    echo Connecting to the PIA WireGuard API on $wg_ip...
    publicKey="$(wg pubkey < ${cfg.privateKeyFile})"
    json="$(curl --no-progress-meter -m 5 -G \
      --connect-to "$wg_hostname::$wg_ip:" \
      --cacert "${cfg.certificateFile}" \
      --data-urlencode "pt=''${token}" \
      --data-urlencode "pubkey=$publicKey" \
      "https://''${wg_hostname}:1337/addKey" || true)"
    status="$(echo "$json" | jq -r '.status' || true)"
    if [ "$status" != "OK" ]; then
      >&2 echo "Server did not return OK. Stopping."
      >&2 echo "$json"
      exit 1
    fi


    echo Creating network interface ${cfg.interface}.
    echo "$json" > ${systemConfig.workingDir}/wireguard.json

    gateway="$(echo "$json" | jq -r '.server_ip')"
    servervip="$(echo "$json" | jq -r '.server_vip')"
    peerip=$(echo "$json" | jq -r '.peer_ip')

    # Create WireGuard configuration file with secure permissions
    umask 077
    config_file="${systemConfig.runtimeDir}/wireguard-${cfg.interface}.conf"
    cat > $config_file << EOF
    [Interface]
    Address = $peerip/32
    DNS = 8.8.8.8
    PostUp = wg set %i private-key ${cfg.privateKeyFile}

    [Peer]
    PublicKey = $(echo "$json" | jq -r '.server_key')
    AllowedIPs = 0.0.0.0/0
    Endpoint = ''${wg_ip}:$(echo "$json" | jq -r '.server_port')
    PersistentKeepalive = 25

    EOF
    echo $config_file

    chmod 640 $config_file

    ${cfg.preUp}
    networksetup -listallnetworkservices | tail -n +2 | xargs -I{} networksetup -setv6off "{}"

    # Start WireGuard interface
    wg-quick up $config_file


    echo Started
    ${cfg.postUp}
  '';

  # Common stop script
  commonStopScript = ''
    ${cfg.preDown}

    wg-quick down ${systemConfig.runtimeDir}/wireguard-${cfg.interface}.conf
    rm ${systemConfig.runtimeDir}/wireguard-${cfg.interface}.conf

    # networksetup -listallnetworkservices | tail -n +2 | xargs -I{} networksetup -setv6automatic "{}"

    ${cfg.postDown}
  '';

in
{
  options.services.pia-vpn = {
    enable = mkEnableOption "Private Internet Access VPN service.";

    certificateFile = mkOption {
      type = types.path;
      description = ''
        Path to the CA certificate for Private Internet Access servers.

        This is provided as <filename>ca.rsa.4096.crt</filename>.
      '';
    };

    environmentFile = mkOption {
      type = types.path;
      description = ''
        Path to an environment file with the following contents:

        <programlisting>
        PIA_USER=''${username}
        PIA_PASS=''${password}
        </programlisting>
      '';
    };

    privateKeyFile = mkOption {
      type = types.path;
      description = ''
        Path to the file containing the WireGuard private key.
        If the file doesn't exist, it will be generated.
        The file must be readable only by the service user.
      '';
    };

    interface = mkOption {
      type = types.str;
      default = "wg0";
      description = ''
        WireGuard interface to create for the VPN connection.
      '';
    };

    region = mkOption {
      type = types.str;
      default = "";
      description = ''
        Name of the region to connect to.
        See https://serverlist.piaservers.net/vpninfo/servers/v4
      '';
    };

    maxLatency = mkOption {
      type = types.float;
      default = 0.1;
      description = ''
        Maximum latency to allow for auto-selection of VPN server,
        in seconds. Does nothing if region is specified.
      '';
    };

    user = mkOption {
      type = types.str;
      default = "pia-vpn";
      description = ''
        User under which the VPN service will run.
      '';
    };

    group = mkOption {
      type = types.str;
      default = "pia-vpn";
      description = ''
        Group under which the VPN service will run.
      '';
    };

    preUp = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Commands called at the start of the interface setup.
      '';
    };

    postUp = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Commands called at the end of the interface setup.
      '';
    };

    preDown = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Commands called before the interface is taken down.
      '';
    };

    postDown = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Commands called after the interface is taken down.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [

    (optionalAttrs isDarwin {
      # Darwin-specific configuration
      launchd.daemons.pia-vpn = {
        serviceConfig = {
          Label = "com.privateinternetaccess.vpn";
          ProgramArguments = [
            "${pkgs.bash}/bin/bash"
            "${commonScript}"
          ];
          KeepAlive = {
            #            SuccessfulExit = false;
            NetworkState = true;
          };
          RunAtLoad = true;
          StandardOutPath = "/var/log/pia-vpn.log";
          StandardErrorPath = "/var/log/pia-vpn.error.log";
          UserName = cfg.user;
          GroupName = cfg.group;
          EnvironmentVariables = {
            PATH = "/usr/sbin:/bin:/usr/bin:/sbin:${
              lib.makeBinPath (
                with pkgs;
                [
                  gnused
                  findutils
                  coreutils
                  bash
                  curl
                  gawk
                  jq
                  wireguard-tools
                  wireguard-go
                ]
              )
            }";
          };
        };
      };
      environment.systemPackages = with pkgs; [
        wireguard-tools
        wireguard-go
      ];

      # Ensure required directories exist with proper permissions
      system.activationScripts.preActivation.text = ''
        mkdir -p ${systemConfig.workingDir}
        mkdir -p ${systemConfig.cacheDir}
        chown ${cfg.user}:${cfg.group} ${systemConfig.workingDir}
        chown ${cfg.user}:${cfg.group} ${systemConfig.cacheDir}
        chmod 750 ${systemConfig.workingDir}
        chmod 750 ${systemConfig.cacheDir}
      '';
    })

    (optionalAttrs (!isDarwin) {

      # Common configuration for both platforms
      users.users.${cfg.user} = {
        description = "Private Internet Access VPN service user";
        isSystemUser = true;
        group = cfg.group;
        home = systemConfig.workingDir;
        createHome = true;
      };

      users.groups.${cfg.group} = { };

      # NixOS-specific configuration
      boot.kernelModules = builtins.trace isDarwin [ "wireguard" ];

      systemd.services.pia-vpn = {
        description = "Connect to Private Internet Access on ${cfg.interface}";
        path = with pkgs; [
          bash
          curl
          gawk
          jq
          wireguard-tools
        ];
        requires = [ "network-online.target" ];
        after = [
          "network.target"
          "network-online.target"
        ];
        wantedBy = [ "multi-user.target" ];

        unitConfig = {
          ConditionFileNotEmpty = [
            cfg.certificateFile
            cfg.environmentFile
            cfg.privateKeyFile
          ];
        };

        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          Restart = "on-failure";
          User = cfg.user;
          Group = cfg.group;
          EnvironmentFile = cfg.environmentFile;
          CacheDirectory = "pia-vpn";
          StateDirectory = "pia-vpn";
          UMask = "0077";
          ProtectSystem = "strict";
          ProtectHome = true;
          PrivateTmp = true;
          ReadOnlyPaths = [
            cfg.certificateFile
            cfg.environmentFile
            cfg.privateKeyFile
          ];
          ReadWritePaths = [
            systemConfig.workingDir
            systemConfig.cacheDir
            systemConfig.runtimeDir
          ];
        };

        script = commonScript;
        preStop = commonStopScript;
      };
    })
  ]);
}
