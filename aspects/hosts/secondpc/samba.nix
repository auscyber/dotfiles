{
  den,
  sharesFor,
  ...
}:
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
# THE AAPL HAZARD, NOW FIXED, and the comment stays because the fix looks like
# an unmotivated global. vfs_fruit used to be loaded by `timemachine` alone.
# vfs_fruit(8): "OS X clients negotiate SMB2 AAPL protocol extensions on the
# first tcon, so mixing shares with and without fruit will globally disable AAPL
# if the first tcon is without fruit." So a Mac that mounted the media share in
# Finder BEFORE Time Machine connected lost AAPL for that whole SMB session and
# the backup share ran degraded -- a failure that correlates with which share you
# opened first and with nothing else. `vfs objects` is now set once in [global]
# (it is an (S) parameter, so a value there is the default for every share) and
# every share therefore loads fruit.
#
# What that costs, which is why it was deferred before: with fruit's defaults
# (`fruit:metadata = netatalk`, `fruit:resource = file`) Finder metadata and
# resource forks move into `.AppleDouble/` sidecars instead of the `._name` files
# macOS writes when the server offers no stream support. So the Nextcloud trees
# ../../services/nextcloud.nix also writes gain `.AppleDouble/` directories, any
# `._name` file already there stops being read as metadata and stays behind as an
# ordinary file, and both are visible to Nextcloud. That is the whole price; it
# is smaller than a degraded backup.
#
# The NT hashes below stay, and it is worth saying why rather than deleting them
# on the assumption kanidm replaced them: SMB authentication needs an NT hash,
# and kanidm's LDAP gateway is read-only with none of the Samba schema, so
# `ldapsam` against it is not a backend at any version. What kanidm changes is
# WHOSE identity a hash belongs to -- `auscyber` is one account now, the unix
# user and the kanidm person both -- not whether a hash is needed.
#
# WHO GETS ONE is not decided here: a person opts in by including
# `den.aspects.samba-user`, and ../../services/samba-users.nix resolves that into
# `config.sambaUsers`. This file only mints each one's hash and keeps the passdb
# in step with it. Note that a samba user needs no `users.users.<n>` entry --
# kanidm supplies the identity through NSS and the hash supplies only the
# authentication -- which is what makes SMB access separable from unix accounts.
#
# WHAT IT SERVES is not decided here either: a share is an aspect
# (../../services/shares.nix), membership is whoever includes it, and the
# resolved set is `sharesFor host` -- the `share` capability's takers on this
# box, folded by ../../base/capability.nix. The collector below turns each entry
# into a samba section. ../../services/nextcloud.nix reads the same function.
#
# kanidm/kanidm#2627 is the epic for real kanidm<->samba auth (SID mapping plus
# kerberos, not NTLM). It is research-stage, so when it lands the NT hashes and
# `samba-passdb-sync` below are what to delete.
{
  den.aspects.samba = {
    # Pulls in ivy-password (via `scoped.user-pwd.secrets`) for the passdb
    # generator below.
    includes = [
      den.aspects.user-pwd
      # NSS/PAM against kanidm, so the groups named below resolve.
      den.aspects.sso
      # The `share-<name>` kanidm group each declared share is gated by.
      # Included here rather than relied on through
      # ../../services/nextcloud.nix, which also includes it: the groups the
      # sections below name have to be provisioned by this file's own closure.
      den.aspects.shares
      # `config.sambaUsers` -- who has SMB authentication.
      den.aspects.samba-users
    ];

    # Access to the backup share, declared here because this is the only thing
    # that uses it. `auscyber` is the person sso.nix provisions.
    provision.groups.timemachine-users.members = [ "auscyber" ];

    # ── WHAT DISCOVERY LISTENS ON ──
    #
    # WS-Discovery only. smbd's own 139/445 and nmbd's 137/138 are NOT here
    # and cannot be until there is a wireguard carrier: the "hosts allow"
    # below admits 10.100.0.0/24 on purpose, so narrowing them to `lan-home`
    # would revoke SMB over the tunnel. They stay on `services.samba.
    # openFirewall` below, wrongly, until that carrier exists.
    #
    # WS-Discovery has no such problem -- it is multicast, so the lan is the
    # only wire it means anything on.
    # A house service: what lets a member's machine resolve the share.
    reach = "household";

    gives =
      { self, ... }:
      {
        tcp = _: {
          name = self.name;
          # smbd itself, so a client can resolve where the share is. Exposure
          # still comes from `services.samba.openFirewall` (see above); this
          # names the wires without narrowing them.
          ports.smb = {
            port = 445;
            meta.description = "smbd: the shares, Time Machine's among them, for the house's own machines.";
            expose = [
              "lan-home"
              "tailscale"
            ];
          };
          ports.wsd = {
            port = 5357;
            meta.description = "WS-Discovery's HTTP leg: Windows Explorer fetching this host's device metadata after finding it on 3702.";
            expose = [ "lan-home" ];
          };
          carrier = self.name;
        };

        udp = _: {
          name = self.name;
          ports.wsd-discovery = {
            port = 3702;
            meta.description = "WS-Discovery multicast: how Windows machines on the lan see this host at all. avahi reaches macOS and Linux only.";
            expose = [ "lan-home" ];
          };
          carrier = self.name;
        };
      };

    nixos =
      {
        config,
        pkgs,
        lib,
        scoped,
        host ? { },
        ...
      }:
      let
        # ── THE SMB SERVER IS THIS HOST, SO IT IS NAMED AFTER IT ──
        #
        # `netbios name`, `server string` and the WSD daemon's `hostname` were
        # the literal `smbnix` in three places, which is one identity written
        # three times with nothing keeping them equal -- and none of the three
        # was the name of the machine actually serving the shares. A Windows
        # client browsing the lan, a Mac in Finder and `smbclient -L` each read
        # a different one of these, so they have to agree.
        #
        # The host's own name is that identity. NetBIOS takes 15 characters
        # upper-cased, which the entity name satisfies; samba itself is
        # case-insensitive here.
        smbName = host.name or config.networking.hostName;

        # `settings.datadirectory` rather than `datadir`: the latter is the
        # instance root (config/ + data/ + store-apps/), and only the former is
        # where account files actually live.
        ncData = config.services.nextcloud.settings.datadirectory;
        # The account nextcloud's own `webdav` give names.
        ncUser =
          (den.lib.capability.linkFor den.aspects.nextcloud "webdav" {
            inherit host;
            peer = host;
          }).username;
        ncShared = config.nextcloud.sharedDir;

        # --- the declared shares, as samba sections ---

        # The resolved set, keyed by share name (../../services/shares.nix).
        # Applied here because `host` -- whose accounts have which share -- is
        # all it needs, and nothing in it depends on this host's fixpoint.
        shares = sharesFor host;

        # EVERYTHING ABOUT A SHARE'S IDENTITY GOES THROUGH NSS, which is to say
        # through kanidm: `valid users = @share-<name>` is a getgrnam() and the
        # authenticated user is a getpwnam(), so the share group and the people in
        # it are the real POSIX identities and no local account or local group is
        # declared for any of it. `force group` is then the share's own kanidm
        # group rather than one shared local group, which is what keeps a share's
        # bytes readable by its members and by nobody else, and leaves the file
        # owner as the person who actually wrote it.
        #
        # WHAT THAT COSTS, and it is one command per share: an identity is only
        # served by NSS once it is POSIX-extended in kanidm, and
        # kanidm-provision's schema has no posix entity (see ./sso.nix, which
        # records the same one-off for `media-users`) -- this repo does not build
        # the `enableUnix`/`gidNumber` patch that would let it be declared, so
        # patches/kanidm-provision/ has the service-accounts patch and nothing
        # else. For each share:
        #
        #   kanidm group posix set share-<name>
        #
        # and once per person: `kanidm person posix set <p>`. Nothing here can
        # check that was done, so both the directory unit and `samba-passdb-sync`
        # below say so in the journal and carry on rather than failing the host.
        #
        # The alternative that was rejected: one local `shares` user plus
        # `force user`, which needs no kanidm posix extension for the OWNERSHIP
        # -- but `valid users = @share-<name>` needs the group extended anyway,
        # so it bought nothing and cost per-file attribution.

        # `<root>/<name>`, with the root declared by the host (below) rather than
        # written here: /mnt/hdd is this box's disk layout, not a fact about
        # shares. A share that wants to live somewhere else sets its own `path`
        # (../../services/shares.nix), which is why this is a fallback and not a
        # rewrite.
        pathOf = s: if s.path != null then s.path else "${config.samba.shareRoot}/${s.name}";

        # The share model's `permissions` are groupfolders' words
        # (read/write/share/delete). Only `write` means anything to samba;
        # `share` and `delete` are Nextcloud-side concepts with no smb.conf
        # equivalent.
        #
        # `quota` is NOT applied, and there is no way to apply it: smb.conf's only
        # size knob is `max disk size`, which is (G)lobal, in megabytes, and caps
        # only the figure REPORTED to clients rather than what may be written
        # (smb.conf(5)). A real cap on a share is a ZFS dataset with a `quota`,
        # i.e. a disko change in ./default.nix, and until then the number only
        # binds the groupfolder ../../services/nextcloud.nix makes.
        writable = s: lib.elem "write" s.permissions;

        # A declared share that is named like one of the hand-written sections
        # would silently replace it, which for `timemachine` means losing a
        # backup destination to a rename somewhere else in the tree.
        reserved = [
          "global"
          "hdd"
          "nc-shared"
          "nc-data"
          "timemachine"
          "homes"
          "printers"
          "print$"
        ];
        collisions = lib.intersectLists reserved (lib.attrNames shares);

        shareSettings =
          if collisions != [ ] then
            throw "aspects/hosts/secondpc/samba.nix: the share(s) ${lib.concatStringsSep ", " collisions} are named like a section this file already declares, and would replace it. Rename the share aspect."
          else
            lib.mapAttrs (_: s: {
              "path" = pathOf s;
              "comment" = s.displayName;
              "browseable" = "yes";
              "guest ok" = "no";
              "read only" = if writable s then "no" else "yes";
              # The kanidm group ../../services/shares.nix provisions for this
              # share, resolved through NSS. smb.conf(5) on the `@` prefix
              # (under `invalid users`, which `valid users` defers to): "a name
              # starting with '@' is interpreted as an NIS netgroup first (if
              # your system supports NIS), and then as a UNIX group if the name
              # was not found in the NIS netgroup database" -- so this is a
              # getgrnam() against nss_kanidm, the same lookup `@media-users`
              # already depends on.
              #
              # Until the group is POSIX-extended it resolves to nothing and
              # the share admits nobody -- it fails shut, which is the right
              # direction, but it does fail. See the note above `pathOf`.
              "valid users" = "@${s.group}";
              # The same group, so every member can write what another member
              # wrote; the directory is setgid as well, so this holds even for
              # a tree created outside samba.
              "force group" = s.group;
              # The global `inherit owner = unix only` would hand new files to
              # the DIRECTORY's owner, which erases who wrote them. The whole
              # reason identity comes from kanidm is that the uid on the file
              # means something.
              "inherit owner" = "no";
            }) shares;

        # --- the people with SMB authentication ---

        # The plaintext this person's NT hash is minted from. A pinned scope
        # (`user-pwd`, for auscyber) reuses the human's real password so the SMB
        # login and the SSO login stay one credential; an unpinned person gets a
        # random one minted below, which is its own credential and is never a
        # unix or SSO password.
        passwordOf =
          u:
          if u.passwordScope == null then
            config.age.secrets.${u.passwordSecret}
          else if !(scoped ? ${u.passwordScope}) then
            throw "samba: '${u.person}' pins their samba password to the '${u.passwordScope}' secret scope, which this host has no aspect for."
          else if !(scoped.${u.passwordScope}.secrets ? ${u.passwordSecret}) then
            throw "samba: '${u.person}' pins their samba password to '${u.passwordScope}/${u.passwordSecret}', which that scope does not declare."
          else
            scoped.${u.passwordScope}.secrets.${u.passwordSecret};
      in
      {
        # Declared through `imports` because the rest of this block is bare
        # config, and one module may not carry `options` beside bare config keys
        # -- ../../services/nextcloud.nix's `nextcloud.sharedDir` does the same.
        imports = [
          {
            options.samba.shareRoot = lib.mkOption {
              type = lib.types.str;
              example = "/mnt/hdd/shares";
              description = ''
                Directory the declared shares (../../services/shares.nix) are served out
                of: each one lands in `<shareRoot>/<name>` unless it sets its
                own `path`.

                No default, deliberately. Where the bytes live is a fact about
                this machine's disks and nothing a share can know, so the HOST
                says it -- see ./default.nix.
              '';
            };
          }
        ];

        # Zeroconf: mDNS resolution plus service advertisement so LAN clients
        # (Finder, Time Machine's disk picker) discover the Samba shares
        # without manual smb:// entry. VPN clients (10.100.0.0/24, see the
        # "hosts allow" below) don't get this -- mDNS is multicast/LAN-only --
        # they connect with an explicit smb://10.100.0.1/... URL instead.
        #
        # `_adisk._tcp` -- the record Time Machine's disk picker browses for --
        # is NOT written as an avahi service file here, and should not be: with
        # `fruit:time machine = yes` smbd "registers the share with mDNS in
        # case Samba is built with mDNS support" (vfs_fruit(8)), and
        # `pkgs.samba4Full` below is built with avahi. So smbd registers
        # through this daemon as an avahi CLIENT, which is what
        # `publish.userServices` permits -- it maps to
        # `disable-user-service-publishing = no`, and without it a
        # client-registered service is refused. A hand-written
        # `extraServiceFiles` entry would be a second, stale copy of a record
        # smbd already keeps in step with the share list.
        #
        # ../../darwin/time-machine.nix does not depend on any of this: it
        # registers the destination by URL. Discovery is only what makes the
        # share appear in Finder and in the picker.
        services.avahi = {
          enable = true;
          nssmdns4 = true;
          # OFF EXPLICITLY, because this one DEFAULTS TO TRUE -- measured:
          # deleting the line left 5353 in the global list. It writes
          # `allowedUDPPorts = [ 5353 ]`, the pseudo-interface `default`.
          #
          # 5353 is already declared on `lan-home` by the home automation
          # aspect that also speaks mDNS, and a socket is exclusive per host
          # (../../base/capability-types.nix) -- so a second giver here would
          # trip `socket-exclusions` rather than add a rule. The hole exists on
          # `br0`; what cannot be said is that avahi is a listener on it too.
          openFirewall = false;
          publish = {
            enable = true;
            userServices = true;
          };
        };

        # Windows' own discovery protocol; avahi reaches macOS and Linux only.
        services.samba-wsdd = {
          enable = true;
          # 5357/tcp and 3702/udp are declared in `gives` above and land on
          # `br0` alone. Off explicitly, not left at its default.
          openFirewall = false;
          # THE HOST SERVING SMB, not a second name for it. See `smbName`.
          hostname = smbName;
        };

        # One NT hash per samba user, plus a minted password for anyone who has
        # no existing one. For `auscyber` the password is the `ivy-password`
        # intermediary (../../security/user-pwd.nix), the same source as
        # ivy-pwd-hash/htpasswd, so the SMB login and the SSO login are one
        # credential; the hash itself is just MD4(UTF-16LE(password)), a one-way
        # value like ivy-pwd-hash's sha512-crypt and safe to deploy normally.
        #
        # The names come from `config.sambaUsers`, not from here, and for
        # auscyber they are pinned to what is already generated --
        # ../../services/samba-users.nix says why.
        #
        # This intentionally does NOT try to build the tdbsam database itself
        # at generation time: `pdbedit -a` requires resolving the account via
        # getpwnam(), which doesn't exist in the generator's build sandbox.
        # Faking it there (e.g. via nss_wrapper) is possible but produces a
        # static blob that has to be redeployed whole and re-diverges from
        # the live system; instead `samba-passdb-sync` below injects each
        # hash into the real, persistent passdb on the real host, where the
        # account actually resolves and no faking is needed.
        age.secrets =
          # The SMB credential of a person who has no password secret anywhere:
          # random, and never deployed (`intermediary`) because the host needs
          # only the hash. They read it once with `nix run .#secret-view`.
          lib.mapAttrs' (
            _: u:
            lib.nameValuePair u.passwordSecret {
              intermediary = true;
              generator.script = "random-base64-32";
            }
          ) (lib.filterAttrs (_: u: u.passwordScope == null) config.sambaUsers)
          // lib.mapAttrs' (
            _: u:
            lib.nameValuePair u.ntHashSecret {
              generator = {
                dependencies.password = passwordOf u;
                script =
                  {
                    pkgs,
                    lib,
                    decrypt,
                    deps,
                    ...
                  }:
                  ''
                    pw="$(${decrypt} ${lib.escapeShellArg deps.password.file})"
                    # UTF-16LE then MD4 -- that is what an NT hash is.
                    #
                    # perl, not iconv, because a generator's `pkgs` is the ADMIN
                    # host's and not the target's: `agenix-generate` is built for
                    # the machine you run `nix run .#gen-secrets` on. Naming an
                    # iconv here means naming one that exists on every machine that
                    # might run it, and there isn't one -- on Linux the binary is
                    # in `glibc.bin` (glibc provides iconv natively, so pkgs.libiconv
                    # is headers only and has no bin/iconv), while on darwin
                    # `pkgs.glibc` refuses to evaluate at all. Encode is core perl,
                    # so this needs no platform branch and no second closure.
                    printf '%s' "$pw" \
                      | ${pkgs.perl}/bin/perl -MEncode -0777 -ne \
                          'print encode("UTF-16LE", decode("UTF-8", $_))' \
                      | ${pkgs.openssl}/bin/openssl dgst -md4 -provider legacy -provider default | \
                      awk '{print toupper($NF)}'
                  '';
              };
            }
          ) config.sambaUsers;

        # Applies each hash to the real passdb (creating the Samba account first
        # if this is its first run) every activation, so it can never drift from
        # the password it came from and never needs to be re-applied by hand
        # after a switch/reboot the way `smbpasswd -a` did. `-a` alone would
        # prompt for -- and briefly hold -- a real password, so it seeds a
        # throwaway one that's immediately overwritten by `--set-nt-hash`, which
        # never sees the plaintext.
        systemd.services.samba-passdb-sync = {
          description = "Sync the declared Samba accounts to their NT-hash secrets";
          before = [ "samba-smbd.service" ];
          requiredBy = [ "samba-smbd.service" ];
          # `pdbedit -a` resolves the account through getpwnam. `auscyber` is
          # also a local account so that one would work regardless, but a samba
          # user does NOT have to be -- kanidm serves the identity through NSS
          # -- so order after unixd, which for a kanidm-only person is the only
          # thing that makes the name resolve at all.
          after = [ "kanidm-unixd.service" ];
          wants = [ "kanidm-unixd.service" ];
          serviceConfig.Type = "oneshot";
          path = [ pkgs.samba ];
          script = ''
            # One person's failure must not take the file server down with it:
            # this unit is `requiredBy` smbd, so a non-zero exit here stops
            # samba for everybody. The likely failure is a kanidm-only person
            # who has not been POSIX-extended yet (`kanidm person posix set
            # <p>`, a one-off -- kanidm-provision's schema cannot express it,
            # see ./sso.nix), which getpwnam cannot see and which is not a
            # reason to refuse every other share.
            sync_user() {
              name="$1"
              hash_file="$2"

              if ! ${pkgs.getent}/bin/getent passwd "$name" >/dev/null 2>&1; then
                echo "samba-passdb-sync: '$name' does not resolve through NSS -- skipping. If this is a kanidm-only person, run: kanidm person posix set $name" >&2
                return 0
              fi

              if ! pdbedit -L -u "$name" >/dev/null 2>&1; then
                if ! printf 'placeholder\nplaceholder\n' | pdbedit -a -u "$name" -t; then
                  echo "samba-passdb-sync: could not create a passdb entry for '$name' -- skipping." >&2
                  return 0
                fi
              fi

              if ! pdbedit -r -u "$name" --set-nt-hash="$(cat "$hash_file")"; then
                echo "samba-passdb-sync: could not set the NT hash for '$name' -- skipping." >&2
                return 0
              fi
            }

            ${lib.concatStringsSep "\n" (
              lib.mapAttrsToList (
                _: u:
                lib.escapeShellArgs [
                  "sync_user"
                  u.person
                  config.age.secrets.${u.ntHashSecret}.path
                ]
              ) config.sambaUsers
            )}
          '';
        };

        services.samba = {
          enable = true;
          package = pkgs.samba4Full; # built with avahi, so smbd does its own mDNS
          # ⚠ STILL THE GLOBAL LISTS (139/445 tcp, 137/138 udp, no `-i`), and
          # deliberately so: see the `gives` note above -- there is no carrier
          # for the wireguard tunnel the "hosts allow" below admits, so these
          # cannot be declared without revoking SMB over it.
          openFirewall = true;
          settings = {
            global = {
              "workgroup" = "WORKGROUP";
              "server string" = smbName;
              "netbios name" = smbName;
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
              # EVERY share loads fruit, which is the point of putting it here:
              # macOS negotiates the SMB2 AAPL extensions on the FIRST tcon of a
              # session, so one share without fruit disables AAPL for the whole
              # session and silently degrades the Time Machine share. See the
              # header for what that costs.
              #
              # The order is load-bearing and this is the right one: vfs_fruit(8)
              # says fruit "must be stacked with vfs_streams_xattr" and that
              # catia belongs with it for character conversion, and it defers
              # every stream other than AFP_AfpInfo/AFP_Resource down to
              # streams_xattr -- which has to be below it to receive them.
              #
              # `fruit:metadata` / `fruit:resource` are left at their defaults
              # (`netatalk` / `file`) on purpose. `stream` would avoid the
              # AppleDouble sidecars, but it RELOCATES where metadata lives, so
              # flipping it makes what is already stored invisible -- and the
              # timemachine share below already holds a live backup.
              "vfs objects" = "catia fruit streams_xattr";
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
            # WRITABLE, and pointed at the external-storage directory rather
            # than at `datadirectory`. ../../services/nextcloud.nix registers
            # the same path with Nextcloud as a `local` external storage and
            # explains why that distinction is what makes writing safe: primary
            # storage is indexed in `oc_filecache` and that index is
            # authoritative, so a file written in through the filesystem is
            # invisible until `occ files:scan` and may later be reconciled as a
            # deletion. An external mount is rescanned on access instead, so
            # both sides can write.
            #
            # It appears in Nextcloud as the "SMB" folder.
            #
            # `force user` is not cosmetic: it makes every file arrive owned by
            # `nextcloud` whoever wrote it, which is what keeps Nextcloud able
            # to read back what Samba put there. Combined with the directory's
            # setgid bit, group stays put too.
            nc-shared = {
              "path" = ncShared;
              "read only" = "no";
              "browseable" = "yes";
              "guest ok" = "no";
              "valid users" = "@media-users";
              "force user" = "nextcloud";
              "force group" = "nextcloud";
            };

            # The account's own Nextcloud files, READ-WRITE.
            #
            # This is primary storage, which Nextcloud indexes in
            # `oc_filecache` and treats that index as authoritative -- so a
            # write arriving through the filesystem is NOT seen until a scan
            # runs. ../../services/nextcloud.nix therefore runs
            # `occ files:scan` on a timer against exactly this path, and the
            # consequences that timer does not fix are listed there. Writing
            # here is a deliberate choice, not an oversight; `nc-shared` above
            # is the arrangement with no such caveats.
            #
            # `force user` makes every write land owned by `nextcloud`, which
            # is both what lets Samba into a 0750 nextcloud:nextcloud tree and
            # what keeps Nextcloud able to read back what Samba wrote.
            #
            # Not `%u`: samba's user here is `auscyber` (the unix account and
            # the kanidm person both -- see the header) while Nextcloud's
            # account is `ivy`, so `%u` would resolve to a directory that does
            # not exist. Once those two names agree, `%u` is the better form
            # and makes this share serve every account instead of one.
            nc-data = {
              "path" = "${ncData}/${ncUser}/files";
              "read only" = "no";
              "browseable" = "yes";
              "guest ok" = "no";
              "valid users" = "@media-users";
              "force user" = "nextcloud";
              "force group" = "nextcloud";
              # The global `inherit owner = unix only` would hand new files to
              # the directory's owner rather than to `force user`.
              "inherit owner" = "no";
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
              # `vfs objects` is NOT repeated here. It is in [global] now -- an
              # (S) parameter set there is the default for every share, and a
              # per-share definition "will overwrite a possible previous
              # definition" (smb.conf(5)), so restating it would be a second
              # copy of the list that can silently diverge from the one every
              # other share gets. The order rationale moved with it.
              #
              # NOT SET, deliberately, having been checked against vfs_fruit(8)
              # for 4.23:
              #
              #   fruit:advertise_fullsync   does not exist. The FULLSYNC volume
              #     capability Time Machine looks for is exactly what
              #     `fruit:time machine = yes` advertises -- that is the whole
              #     text of its documentation. Nothing else to turn on.
              #   fruit:aapl                 global-only and already `yes` by
              #     default; setting it per-share would be ignored.
              #   fruit:metadata / fruit:resource   left at `netatalk` / `file`,
              #     in [global]. `stream` would avoid AppleDouble sidecars next
              #     to the sparsebundle, but it RELOCATES where metadata lives,
              #     so flipping it on a share that already holds a live backup
              #     makes what is stored there invisible. No netatalk server
              #     reads this tree, so there is nothing to gain either.
              "fruit:time machine" = "yes";
              # Caps what Time Machine believes the volume holds, so it stops
              # instead of growing until /mnt/hdd is full -- which matters here
              # because /mnt/hdd is also the media library (./media.nix,
              # ./plex.nix) and the Nextcloud external mount above.
              #
              # 1T, from: the client's own last-known figures for this share
              # (3.22 TB available, measured 2026-10-05) against ~291 GiB of
              # backed-up data on the Mac -- /nix is a separate APFS volume and
              # ../../darwin/time-machine.nix excludes it. So 1T is ~3.5x the
              # source, above Apple's 2-3x rule of thumb for useful history,
              # and still leaves ~2.2 TB of today's free space to the media
              # libraries. Raising it later is safe; lowering it below what the
              # sparsebundle already occupies is not.
              #
              # READ THE CAVEAT IN vfs_fruit(8) BEFORE TRUSTING THIS: the
              # reported size is band size x band count of the sparsebundles in
              # the share and NOTHING ELSE, so the media sitting on the same
              # ZFS dataset is not subtracted. This is therefore a ceiling on
              # Time Machine, not a promise that /mnt/hdd has room -- if the
              # library fills the pool, Time Machine still believes it has 1 TB
              # free and fails at write time. The real guarantee would be a
              # dedicated ZFS dataset with a `quota` for this path, which is a
              # disko change in ./default.nix.
              "fruit:time machine max size" = "1T";
              "durable handles" = "yes";
              "kernel oplocks" = "no";
              "kernel share modes" = "no";
              "posix locking" = "no";
            };
          }
          # The declared shares (../../services/shares.nix), one section each.
          # `//` rather than a merge: the sections above are hand-written and a
          # declared share must not reach them -- `collisions` throws first.
          // shareSettings;
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

        # The root the shares sit in. Only the root: root:root 0755 needs no name
        # that kanidm has to resolve, so systemd-tmpfiles can make it at boot,
        # while each share's own directory is group-owned by a kanidm group and
        # therefore has to wait for `samba-share-dirs` below.
        systemd.tmpfiles.settings.samba-shares.${config.samba.shareRoot}."d" = {
          user = "root";
          group = "root";
          mode = "0755";
        };

        # One directory per declared share, group-owned by that share's kanidm
        # group and setgid so everything written inside stays in it.
        #
        # A UNIT RATHER THAN A TMPFILES RULE, and that is the whole reason this
        # exists: systemd-tmpfiles runs long before kanidm-unixd, so a kanidm
        # group name in a tmpfiles rule does not resolve on a cold boot (the
        # `timemachine` rule above uses a local group precisely to dodge that).
        # Ordering after unixd is what makes `chgrp share-<name>` possible at
        # all, and it is the same argument `samba-passdb-sync` makes for
        # `pdbedit`: the work has to happen on the real host, after NSS is up.
        systemd.services.samba-share-dirs = {
          description = "Create the declared SMB share directories";
          before = [ "samba-smbd.service" ];
          requiredBy = [ "samba-smbd.service" ];
          after = [ "kanidm-unixd.service" ];
          wants = [ "kanidm-unixd.service" ];
          serviceConfig.Type = "oneshot";
          script = ''
            # Soft failure for the same reason as samba-passdb-sync: this unit is
            # `requiredBy` smbd, so one unresolvable group must not stop the file
            # server for every other share.
            share_dir() {
              path="$1"
              group="$2"

              if ! mkdir -p "$path"; then
                echo "samba-share-dirs: could not create '$path' -- skipping." >&2
                return 0
              fi
              # setgid, so a tree created outside samba also stays in the group.
              chmod 2770 "$path" || echo "samba-share-dirs: could not chmod '$path'." >&2

              if ! ${pkgs.getent}/bin/getent group "$group" >/dev/null 2>&1; then
                echo "samba-share-dirs: '$group' does not resolve through NSS, so '$path' stays root-owned and admits nobody. Run: kanidm group posix set $group" >&2
                return 0
              fi
              chgrp "$group" "$path" || echo "samba-share-dirs: could not give '$path' to '$group'." >&2
            }

            ${lib.concatStringsSep "\n" (
              lib.mapAttrsToList (
                _: s:
                lib.escapeShellArgs [
                  "share_dir"
                  (pathOf s)
                  s.group
                ]
              ) shares
            )}
          '';
        };
      };
  };
}
