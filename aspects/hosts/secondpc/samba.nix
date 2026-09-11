{ den, ... }:
# Network file sharing for secondpc: Samba exposing /mnt/hdd (including the
# Media library plex/jellyfin use) plus a fruit-enabled Time Machine share for
# macOS, discoverable over mDNS on the LAN and reachable off-LAN through the
# wireguard tunnel (aspects/network/vpn.nix) since its subnet is allowlisted
# below. Netatalk/AFP was considered and dropped in favour of this -- one
# protocol, one credential store, reuses the firewall rule Samba already opens
# for itself.
{
  den.aspects.samba = {
    # Pulls in ivy-password (via `scoped.user-pwd.secrets`) for the passdb
    # generator below.
    includes = [ den.aspects.user-pwd ];

    nixos =
      {
        config,
        pkgs,
        scoped,
        ...
      }:
      {
        # Zeroconf: mDNS resolution plus service advertisement so LAN clients
        # (Finder, Time Machine's disk picker) discover the Samba shares
        # without manual smb:// entry. VPN clients (10.100.0.0/24, see the
        # "hosts allow" below) don't get this -- mDNS is multicast/LAN-only --
        # they connect with an explicit smb://10.100.0.1/... URL instead.
        services.avahi = {
          enable = true;
          nssmdns = true;
          openFirewall = true;
          publish = {
            enable = true;
            userServices = true;
          };
          extraServiceFiles.smb = ''
            <?xml version="1.0" standalone='no'?>
            <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
            <service-group>
              <name replace-wildcards="yes">%h</name>
              <service>
                <type>_smb._tcp</type>
                <port>445</port>
              </service>
              <service>
                <type>_device-info._tcp</type>
                <port>0</port>
                <txt-record>model=RackMac</txt-record>
              </service>
              <service>
                <type>_adisk._tcp</type>
                <port>9</port>
                <txt-record>dk0=adVN=timemachine,adVF=0x82</txt-record>
                <txt-record>sys=adVF=0x100</txt-record>
              </service>
            </service-group>
          '';
        };

        # Samba password database for `auscyber`, seeded from the same
        # ivy-password intermediary secret as ivy-pwd-hash/htpasswd (see
        # user-pwd.nix / media.nix) -- built at generation time with pdbedit
        # against a scratch smb.conf, so the plaintext password never lands
        # on this host, only the resulting tdbsam hash database.
        age.secrets."samba-passdb.tdb" = {
          owner = "root";
          generator = {
            dependencies = { inherit (scoped.user-pwd.secrets) ivy-password; };
            script =
              {
                pkgs,
                lib,
                decrypt,
                deps,
                ...
              }:
              ''
                workdir=$(mktemp -d)
                trap 'rm -rf "$workdir"' EXIT
                mkdir -p "$workdir/private"
                cat > "$workdir/smb.conf" <<EOF
                [global]
                  private dir = $workdir/private
                  passdb backend = tdbsam:$workdir/private/passdb.tdb
                EOF
                pw="$(${decrypt} ${lib.escapeShellArg deps.ivy-password.file})"
                printf '%s\n%s\n' "$pw" "$pw" | \
                  ${pkgs.samba}/bin/pdbedit -a -u auscyber -t -s "$workdir/smb.conf"
                cat "$workdir/private/passdb.tdb"
              '';
          };
        };

        services.samba = {
          enable = true;
          securityType = "user";
          openFirewall = true;
          settings = {
            global = {
              "workgroup" = "WORKGROUP";
              "server string" = "smbnix";
              "netbios name" = "smbnix";
              "security" = "user";
              "server min protocol" = "SMB2";
              "passdb backend" = "tdbsam:${config.age.secrets."samba-passdb.tdb".path}";
              # 192.168.0.0/24 (LAN), 100.64.0.0/10 (unused/reserved), and
              # 10.100.0.0/24 (the wireguard tunnel -- see aspects/network/vpn.nix
              # -- lets the Mac reach this share from a different network).
              "hosts allow" = "192.168.0. 127.0.0.1 localhost 100.64.0. 10.100.0.";
              "hosts deny" = "0.0.0.0/0";
              "guest account" = "nobody";
              "map to guest" = "bad user";
            };
            hdd = {
              "path" = "/mnt/hdd";
              "browseable" = "yes";
              "read only" = "no";
              "guest ok" = "no";
              "create mask" = "0644";
              "directory mask" = "0755";
              "force group" = "music";
            };
            timemachine = {
              "path" = "/mnt/hdd/timemachine";
              "valid users" = "auscyber";
              "read only" = "no";
              "browseable" = "yes";
              "guest ok" = "no";
              "vfs objects" = "catia fruit streams_xattr";
              "fruit:time machine" = "yes";
              "durable handles" = "yes";
              "kernel oplocks" = "no";
              "kernel share modes" = "no";
              "posix locking" = "no";
            };
          };
        };

        systemd.tmpfiles.settings.timemachine."/mnt/hdd/timemachine"."d" = {
          user = "auscyber";
          group = "users";
          mode = "0700";
        };
      };
  };
}
