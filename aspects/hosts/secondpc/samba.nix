{ den, ... }:
# Network file sharing for secondpc: Samba exposing /mnt/hdd (including the
# Media library plex/jellyfin use) plus a fruit-enabled Time Machine share for
# macOS, discoverable over mDNS on the LAN and reachable off-LAN through the
# wireguard tunnel (aspects/network/vpn.nix) since its subnet is allowlisted
# below. Netatalk/AFP was considered and dropped in favour of this -- one
# protocol, one credential store, reuses the firewall rule Samba already opens
# for itself.
#
# Identity comes from kanidm: sso.nix turns on `enablePam`, so NSS resolves
# kanidm users and groups and no local account is declared for the shares.
#
# The NT hash below stays, and it is worth saying why rather than deleting it
# on the assumption kanidm replaced it: SMB authentication needs an NT hash,
# and kanidm's LDAP gateway is read-only with none of the Samba schema, so
# `ldapsam` against it is not a backend at any version. What kanidm changes is
# WHOSE hash it is -- `ivy`, the kanidm person, rather than a separate local
# account -- not whether one is needed.
#
# kanidm/kanidm#2627 is the epic for real kanidm<->samba auth (SID mapping plus
# kerberos, not NTLM). It is research-stage, so when it lands the NT hash and
# `samba-passdb-sync` below are what to delete.
#
# kanidm/kanidm#2627 is the epic for real kanidm<->samba auth (SID mapping plus
# kerberos, not NTLM). It is research-stage, so when it lands the NT hash and
# `samba-passdb-sync` below are what to delete.
{
  den.aspects.samba = {
    # Pulls in ivy-password (via `scoped.user-pwd.secrets`) for the passdb
    # generator below.
    includes = [
      den.aspects.user-pwd
      # NSS/PAM against kanidm, so the groups named below resolve.
      den.aspects.sso
    ];

    # Access to the backup share, declared here because this is the only thing
    # that uses it. `ivy` is the person sso.nix provisions.
    provision.groups.timemachine-users.members = [ "ivy" ];

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
          nssmdns4 = true;
          openFirewall = true;
          publish = {
            enable = true;
            userServices = true;
          };
        };

        # Windows' own discovery protocol; avahi reaches macOS and Linux only.
        services.samba-wsdd = {
          enable = true;
          openFirewall = true;
          hostname = "smbnix";
        };

        # Samba's NT hash for `ivy` -- the same name as the kanidm person, so
        # the SMB login and the SSO login are one identity even though the hash
        # has to live in its own store. Derived from the same ivy-password
        # intermediary secret as ivy-pwd-hash/htpasswd (see user-pwd.nix /
        # media.nix) -- just MD4(UTF-16LE(password)), a one-way value like
        # ivy-pwd-hash's sha512-crypt, safe to deploy normally.
        #
        # This intentionally does NOT try to build the tdbsam database itself
        # at generation time: `pdbedit -a` requires resolving `ivy` via
        # getpwnam(), which doesn't exist in the generator's build sandbox.
        # Faking it there (e.g. via nss_wrapper) is possible but produces a
        # static blob that has to be redeployed whole and re-diverges from
        # the live system; instead `samba-passdb-sync` below injects this
        # hash into the real, persistent passdb on the real host, where `ivy`
        # resolves through kanidm's NSS and no faking is needed.
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
          description = "Sync the ivy Samba account to ivy-nt-hash";
          before = [ "samba-smbd.service" ];
          requiredBy = [ "samba-smbd.service" ];
          # `pdbedit -a` resolves the account through getpwnam, and `ivy` comes
          # from kanidm -- so this cannot run before kanidm-unixd answers.
          after = [ "kanidm-unixd.service" ];
          wants = [ "kanidm-unixd.service" ];
          serviceConfig.Type = "oneshot";
          path = [ pkgs.samba ];
          script = ''
            if ! pdbedit -L -u ivy >/dev/null 2>&1; then
              printf 'placeholder\nplaceholder\n' | pdbedit -a -u ivy -t
            fi
            pdbedit -r -u ivy --set-nt-hash="$(cat ${config.age.secrets."ivy-nt-hash".path})"
          '';
        };

        services.samba = {
          enable = true;
          package = pkgs.samba4Full; # built with avahi, so smbd does its own mDNS
          openFirewall = true;
          settings = {
            global = {
              "workgroup" = "WORKGROUP";
              "server string" = "smbnix";
              "netbios name" = "smbnix";
              "security" = "user";
              # SMB3 throughout, and encrypted. Note this breaks anonymous
              # enumeration (`smbclient -L <ip> -U%`).
              "server min protocol" = "SMB3_00";
              "server smb encrypt" = "required";
              # Let the filesystem decide ownership and mode rather than samba
              # imposing its own: the library is group-owned and setgid, and
              # create/force masks would fight that.
              "inherit owner" = "unix only";
              "inherit permissions" = "yes";
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
              # A kanidm group, resolved through NSS.
              "valid users" = "@media-users";
              "force group" = "media";
            };
            timemachine = {
              "path" = "/mnt/hdd/timemachine";
              # kanidm group; `timemachine` below is the local group that owns
              # the directory. Two names because only one of them has to resolve
              # before kanidm-unixd is up.
              "valid users" = "@timemachine-users";
              "force group" = "timemachine";
              # The global `inherit owner` would hand every backup file to the
              # directory's owner; a backup has to belong to whoever made it.
              "inherit owner" = "no";
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

        # A local group on purpose: systemd-tmpfiles runs long before
        # kanidm-unixd, so a kanidm group name here would not resolve on a cold
        # boot. setgid keeps the sparsebundle's contents in it.
        users.groups.timemachine = { };
        systemd.tmpfiles.settings.timemachine."/mnt/hdd/timemachine"."d" = {
          user = "root";
          group = "timemachine";
          mode = "2770";
        };
      };
  };
}
