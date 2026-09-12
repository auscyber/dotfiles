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

        # Samba's NT hash for `auscyber`, derived from the same ivy-password
        # intermediary secret as ivy-pwd-hash/htpasswd (see user-pwd.nix /
        # media.nix) -- just MD4(UTF-16LE(password)), a one-way value like
        # ivy-pwd-hash's sha512-crypt, safe to deploy normally.
        #
        # This intentionally does NOT try to build the tdbsam database itself
        # at generation time: `pdbedit -a` requires resolving `auscyber` via
        # getpwnam(), which doesn't exist in the generator's build sandbox.
        # Faking it there (e.g. via nss_wrapper) is possible but produces a
        # static blob that has to be redeployed whole and re-diverges from
        # the live system; instead `samba-passdb-sync` below injects this
        # hash into the real, persistent passdb on the real host, where
        # `auscyber` is an actual account and no faking is needed.
        age.secrets."ivy-nt-hash" = {
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
                pw="$(${decrypt} ${lib.escapeShellArg deps.ivy-password.file})"
                printf '%s' "$pw" | ${pkgs.libiconv}/bin/iconv -f UTF-8 -t UTF-16LE | \
                  ${pkgs.openssl}/bin/openssl dgst -md4 -provider legacy -provider default | \
                  awk '{print toupper($NF)}'
              '';
          };
        };

        # Applies ivy-nt-hash to the real passdb (creating the `auscyber`
        # Samba account first if this is the first run) every activation, so
        # it can never drift from ivy-password and never needs to be
        # re-applied by hand after a switch/reboot the way `smbpasswd -a`
        # did. `-a` alone would prompt for -- and briefly hold -- a real
        # password, so it seeds a throwaway one that's immediately
        # overwritten by `--set-nt-hash`, which never sees the plaintext.
        systemd.services.samba-passdb-sync = {
          description = "Sync the auscyber Samba account to ivy-nt-hash";
          before = [ "samba-smbd.service" ];
          requiredBy = [ "samba-smbd.service" ];
          serviceConfig.Type = "oneshot";
          path = [ pkgs.samba ];
          script = ''
            if ! pdbedit -L -u auscyber >/dev/null 2>&1; then
              printf 'placeholder\nplaceholder\n' | pdbedit -a -u auscyber -t
            fi
            pdbedit -r -u auscyber --set-nt-hash="$(cat ${config.age.secrets."ivy-nt-hash".path})"
          '';
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
              # Default log level is effectively silent -- it logs smbd
              # startup and nothing else, not even auth failures. auth_audit
              # puts a one-line NOTICE-level pass/fail per login attempt in
              # log.smbd without the noise of a blanket higher log level.
              "log level" = "1 auth_audit:3";
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
              "force group" = "media";
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
