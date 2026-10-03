{
  den,
  lib,
  ...
}:
# Nextcloud on secondpc, per https://wiki.nixos.org/wiki/Nextcloud: nginx +
# php-fpm from the nixpkgs module, postgres over the unix socket, files on the
# `zpool` data pool.
#
# Secondpc-specific by construction (the domain, /mnt/hdd, the kanidm issuer),
# so the few host facts are written here rather than threaded through options.
let
  domain = "ivymect.in";
  hostName = "cloud.${domain}";

  # Files on the ZFS data pool, not /var/lib: the root pool is a small SSD.
  # `home` carries config/ + data/ + store-apps/, and `datadir` defaults to it,
  # so this one path is the whole instance minus the database.
  #
  # A dedicated `zpool/nextcloud` dataset in the host's disko block was the
  # alternative and is deliberately not taken: disko generates a `fileSystems`
  # entry per dataset, so declaring one that does not exist on the live pool
  # would fail that mount, and the boot, until somebody ran `zfs create`. A
  # directory on the existing `zpool/root` dataset needs nothing.
  home = "/mnt/hdd/nextcloud";

  # Mount roots for the rclone remotes that stand in for the cloud backends
  # Nextcloud does not have (see the external-storage block below).
  externalRoot = "/mnt/hdd/nextcloud-external";

  # A directory both Nextcloud and Samba write to, registered below as a
  # `local` EXTERNAL storage rather than living inside `datadirectory`.
  #
  # This is the whole reason it is a separate directory: Nextcloud's primary
  # storage is indexed in `oc_filecache` and that index is authoritative, so a
  # file written straight into `<datadirectory>/<user>/files` is invisible to
  # the web UI and every sync client until `occ files:scan` runs -- and
  # Nextcloud may reconcile the difference later by treating it as deleted.
  # An external `local` mount carries the opposite assumption: the backend is
  # expected to change underneath Nextcloud, so it is rescanned on access.
  # That makes a read-WRITE share correct here and unsafe on the data dir.
  sharedDir = "/mnt/hdd/nextcloud-shared";

  # --- out-of-store host state ---
  #
  # None of these are agenix secrets. Each is typed in once per host, by hand,
  # for the same reason as the mosquitto hashes in ../homeassistant/default.nix:
  # a generated secret has to be minted with the gpg-yubikey master identity
  # before the host will even evaluate, and these are values that only ever need
  # to exist on this one box.
  #
  #   umask 077
  #   install -d -m 0700 /var/lib/nextcloud-state
  #   openssl rand -base64 32 | tr -d '\n' > /var/lib/nextcloud-state/adminpass
  #
  #   # Backblaze B2 credentials for the backup below: an application key
  #   # scoped to the one bucket (Backblaze calls the two halves keyID and
  #   # applicationKey). These are rclone's own variable names for the `b2`
  #   # backend, not the AWS ones -- the native backend is used rather than
  #   # B2's S3 compatibility layer, so there is no endpoint to configure.
  #   printf 'RCLONE_CONFIG_B2_ACCOUNT=%s\nRCLONE_CONFIG_B2_KEY=%s\n' \
  #     "$keyId" "$appKey" > /var/lib/nextcloud-state/b2.env
  #
  #   # The kanidm OAuth2 client secret, read by BOTH ends so they cannot
  #   # drift. It MUST exist before the first switch that deploys this aspect:
  #   # kanidm bind-mounts the path and refuses to start without it, which would
  #   # take every SSO-gated vhost down with it.
  #   install -d -m 0750 -g nextcloud-oidc /var/lib/nextcloud-oidc
  #   openssl rand -hex 32 | tr -d '\n' > /var/lib/nextcloud-oidc/client-secret
  #   chgrp nextcloud-oidc /var/lib/nextcloud-oidc/client-secret
  #   chmod 0640 /var/lib/nextcloud-oidc/client-secret
  #
  #   # rclone remotes for the external storages, owned by the nextcloud user
  #   # because rclone rewrites this file whenever it refreshes a token.
  #   install -d -m 0700 -o nextcloud -g nextcloud /var/lib/nextcloud-rclone
  stateDir = "/var/lib/nextcloud-state";
  adminpassFile = "${stateDir}/adminpass";
  b2EnvFile = "${stateDir}/b2.env";
  oidcSecretFile = "/var/lib/nextcloud-oidc/client-secret";
  rcloneConfig = "/var/lib/nextcloud-rclone/rclone.conf";

  # Nextcloud's `serverinfo` app (ships in core) serves the metrics document
  # the exporter reads, gated by a token rather than an admin login. Both sides
  # read this one file -- occ puts the value INTO Nextcloud, the exporter
  # presents it -- so they cannot drift.
  serverinfoTokenFile = "${stateDir}/serverinfo-token";

  # Created by hand in the Backblaze dashboard, along with the application key
  # whose two halves go into b2.env.
  #
  # Unlike the Cloudflare R2 arrangement this replaces, nothing here is known
  # only by asking an API: rclone's native `b2` backend derives its own API
  # endpoint from the credentials, so there is no account id to look up and no
  # generated file to read at evaluation time. The backup is therefore
  # unconditional in the config and merely SKIPPED at runtime until the
  # credentials exist -- no `pathExists` guard, and no way for a missing
  # generated file to make this host stop evaluating.
  b2Bucket = "secondpc-nextcloud";

  # rclone remotes mounted on the host and handed to Nextcloud as `local`
  # external storages. The gdrive/onedrive-personal remote NAMES and settings
  # are ./rclone.nix's; that aspect is home-manager only (it mounts into a
  # user's ~/mnts, with the OAuth tokens as agenix secrets in a user scope), so
  # this adds a system-level mount alongside it rather than refactoring it.
  externals = {
    gdrive = {
      remote = "gdrive";
      label = "Google Drive";
    };
    onedrive = {
      remote = "onedrive-personal";
      label = "OneDrive";
    };
    icloud = {
      remote = "icloud";
      label = "iCloud Drive";
    };
  };
in
{
  den.aspects.nextcloud = {
    includes = [
      # nginx + the wildcard ivymect.in ACME cert + the cloudflare tunnel that
      # already fronts every other *.ivymect.in vhost on this box.
      den.aspects.secondpc-web
    ];

    # A plain vhost, NOT behind the shared oauth2-proxy SSO
    # (../hosts/secondpc/sso.nix) -- same reasoning as Home Assistant in
    # ../homeassistant/default.nix. Nextcloud has its own login (kanidm-backed
    # via user_oidc below), and three classes of client here cannot follow an
    # interactive SSO redirect: the desktop/mobile sync clients, the
    # CalDAV/CardDAV clients, and Zotero, which talks WebDAV at
    # /remote.php/dav/files/<user>/ with an app password. Gating this vhost
    # breaks all three.
    #
    # No locations here: the nixpkgs module declares this same virtualHost
    # (php-fpm, the .well-known caldav/carddav redirects, client_max_body_size
    # from `maxUploadSize`) and these two attributes merge into it.
    vhosts.${hostName} = {
      useACMEHost = domain;
      forceSSL = true;
    };

    nixos =
      {
        config,
        pkgs,
        ...
      }:
      let
        occ = lib.getExe config.services.nextcloud.occ;
        # Derived from the options rather than the literals above, so the backup
        # and the unit ordering follow the module wherever it puts things.
        ncHome = config.services.nextcloud.home;
        dumpDir = "${config.services.nextcloud.datadir}/db-dump";
        issuer = "https://auth.${domain}/oauth2/openid/nextcloud";

        rcloneMounts = lib.mapAttrs' (name: e: {
          name = "rclone-mount-${name}";
          value = {
            description = "rclone mount of ${e.label} for Nextcloud";
            after = [ "network-online.target" ];
            wants = [ "network-online.target" ];
            wantedBy = [ "multi-user.target" ];
            before = [ "phpfpm-nextcloud.service" ];
            unitConfig = {
              # Skipped rather than failed until the remotes are configured.
              ConditionPathExists = rcloneConfig;
              RequiresMountsFor = [ externalRoot ];
            };
            # fusermount has to be the setuid wrapper: the mount runs as
            # nextcloud so the files land owned by the user php-fpm reads them
            # as, and an unprivileged mount/unmount goes through that wrapper.
            path = [
              pkgs.fuse
              "/run/wrappers"
            ];
            serviceConfig = {
              # rclone implements sd_notify, so php-fpm cannot be started
              # against a half-mounted directory.
              Type = "notify";
              User = "nextcloud";
              Group = "nextcloud";
              Restart = "on-failure";
              RestartSec = "30s";
              ExecStart = lib.concatStringsSep " " [
                (lib.getExe pkgs.rclone)
                "mount"
                "${e.remote}:"
                "${externalRoot}/${name}"
                "--config ${rcloneConfig}"
                # A metadata-only listing is a round trip per directory on all
                # three of these; without a cache Nextcloud's scanner is
                # unusable.
                "--vfs-cache-mode full"
                "--vfs-cache-max-age 24h"
                "--dir-cache-time 1h"
              ];
              ExecStop = "/run/wrappers/bin/fusermount -uz ${externalRoot}/${name}";
            };
          };
        }) externals;
      in
      {
        # A CONFIDENTIAL client -- user_oidc has a server side and takes a
        # client secret, unlike Home Assistant's public PKCE client.
        #
        # Written through `nixos` rather than the `provision` class
        # (./gateway.nix), for the reason spelled out at the top of
        # ../homeassistant/default.nix's `nixos` block: class content merges one
        # level deep, so a second aspect writing `systems` REPLACES the first
        # aspect's whole `systems` tree -- which would silently delete sso.nix's
        # `oauth2-proxy` client and take every gated vhost with it. The NixOS
        # option system merges `systems.oauth2` by attribute.
        services.kanidm.provision.systems.oauth2.nextcloud = {
          displayName = "Nextcloud";
          # user_oidc's `login#code` route (appinfo/routes.php), i.e.
          # /apps/<appid>/code.
          originUrl = "https://${hostName}/apps/user_oidc/code";
          originLanding = "https://${hostName}/";
          basicSecretFile = oidcSecretFile;
          preferShortUsername = true;
          # `groups_name`, not `groups`: only that form yields bare names like
          # `media-users`; the SPN forms arrive as `media-users@auth.ivymect.in`.
          scopeMaps.media-users = [
            "openid"
            "profile"
            "email"
            "groups_name"
          ];
        };

        # kanidm reads the client secret as its own user and nextcloud's occ
        # reads the same file, so one group holds both rather than either side
        # getting a copy that can drift.
        users.groups.nextcloud-oidc.members = [
          "kanidm"
          "nextcloud"
        ];

        # Same shape, for the serverinfo token: occ writes the value into
        # Nextcloud as `nextcloud`, the exporter reads the file as its own
        # static user (that exporter sets `DynamicUser = false`).
        users.groups.nextcloud-metrics.members = [
          "nextcloud"
          "nextcloud-exporter"
        ];

        # Read by ../hosts/secondpc/samba.nix, so the share and the external
        # mount cannot drift apart. Declared through `imports` because the rest
        # of this block is bare config, and one module may not carry `options`
        # beside bare config keys.
        imports = [
          {
            options.nextcloud.sharedDir = lib.mkOption {
              type = lib.types.str;
              readOnly = true;
              default = sharedDir;
              description = "Directory shared read-write with Samba and registered with Nextcloud as a `local` external storage.";
            };
          }
        ];

        # --- metrics ---
        #
        # Into the prometheus already running on this host
        # (../hosts/secondpc/observability.nix). `scrapeConfigs` is list-typed,
        # so the job is declared HERE, next to the thing it scrapes, rather
        # than in that file's list -- the same reason the exportarr jobs could
        # have been but are not.
        #
        # Scraped over loopback, not the public vhost: going through nginx and
        # the cloudflare tunnel to reach a service on the same box would make
        # metrics collection depend on DNS, TLS and the tunnel being up.
        services.prometheus.exporters.nextcloud = {
          enable = true;
          url = "http://127.0.0.1";
          tokenFile = serverinfoTokenFile;
          # `Host:` has to be the configured trusted domain or Nextcloud
          # answers with a redirect to the canonical name instead of the
          # serverinfo document.
          extraFlags = [ "--server-host ${hostName}" ];
        };

        services.prometheus.scrapeConfigs = [
          {
            job_name = "nextcloud";
            static_configs = [
              {
                targets = [
                  "localhost:${toString config.services.prometheus.exporters.nextcloud.port}"
                ];
              }
            ];
          }
          # php-fpm's own pool status, which is what actually shows Nextcloud
          # saturating its workers -- the serverinfo document above reports
          # Nextcloud's view of itself (users, shares, app versions) and says
          # nothing about the pool.
          {
            job_name = "nextcloud-phpfpm";
            static_configs = [
              {
                targets = [
                  "localhost:${toString config.services.prometheus.exporters.php-fpm.port}"
                ];
              }
            ];
          }
        ];

        # The pool has to publish a status path for that exporter to read.
        services.phpfpm.pools.nextcloud.settings."pm.status_path" = "/status";
        services.prometheus.exporters.php-fpm = {
          enable = true;
          environmentFile = null;
          # Reads the pool over its unix socket rather than a TCP listener, so
          # nothing new is exposed.
          extraFlags = [
            "--phpfpm.scrape-uri"
            "unix://${config.services.phpfpm.pools.nextcloud.socket};/status"
          ];
        };

        # `rclone mount` is an unprivileged FUSE mount, which needs the setuid
        # fusermount wrapper this turns on.
        programs.fuse.enable = true;

        systemd.tmpfiles.settings = {
          "10-nextcloud-shared".${sharedDir}.d = {
            user = "nextcloud";
            group = "nextcloud";
            # 2770: setgid so anything Samba creates here stays in the group,
            # whichever account wrote it.
            mode = "2770";
          };
          "10-nextcloud-state".${stateDir}.d = {
            user = "root";
            group = "root";
            mode = "0700";
          };
          "10-nextcloud-oidc"."/var/lib/nextcloud-oidc".d = {
            user = "root";
            group = "nextcloud-oidc";
            mode = "0750";
          };
          # rclone rewrites its config on token refresh, so this one belongs to
          # the user the mounts run as.
          "10-nextcloud-rclone"."/var/lib/nextcloud-rclone".d = {
            user = "nextcloud";
            group = "nextcloud";
            mode = "0700";
          };
          "10-nextcloud-hdd" = {
            ${dumpDir}.d = {
              user = "nextcloud";
              group = "nextcloud";
              mode = "0700";
            };
          }
          // lib.mapAttrs' (name: _: {
            name = "${externalRoot}/${name}";
            value.d = {
              user = "nextcloud";
              group = "nextcloud";
              mode = "0750";
            };
          }) externals;
        };

        services.nextcloud = {
          enable = true;
          inherit hostName home;

          # 34, not the module's stateVersion-driven default. Nextcloud upgrades
          # one major at a time, so the version is a commitment rather than a
          # preference and belongs in the config: 34.0.4 is the newest series
          # with patch releases behind it (35 is still at .0), and it leaves 35
          # as the single next step.
          package = pkgs.nextcloud34;

          https = true;
          # nginx's client_max_body_size comes from this, which is what Zotero
          # attachment uploads and the sync clients run into. NOTE the tunnel in
          # front of nginx caps a PROXIED request body at 100MB, so anything
          # bigger only works over the LAN.
          maxUploadSize = "10G";

          # Redis for the distributed and file-locking caches; with no locking
          # cache Nextcloud's own admin overview flags the instance.
          configureRedis = true;

          # Postgres over the unix socket. `createLocally` writes the database
          # and role into the same list-typed `services.postgresql.ensureDatabases`
          # / `ensureUsers` that ../homeassistant/default.nix already declares
          # `hass` in -- list definitions concatenate, so the two coexist and
          # that file stays untouched. Socket peer auth means there is no
          # database password anywhere, which is also the only form
          # `createLocally` still supports.
          database.createLocally = true;
          config = {
            dbtype = "pgsql";
            dbname = "nextcloud";
            dbuser = "nextcloud";
            dbhost = "/run/postgresql";
            # `ivy`, not the repo's usual `auscyber`: Zotero's WebDAV URL
            # embeds the username (/remote.php/dav/files/ivy/), so this name is
            # load-bearing outside Nextcloud.
            adminuser = "ivy";
            inherit adminpassFile;
          };

          # The official OIDC app. `extraApps` is what pins it to the store
          # instead of the appstore, and `extraAppsEnable` (default true) is what
          # has nextcloud-setup run `occ app:enable` for it. Taken off the
          # pinned package so the app and the server cannot drift apart.
          #
          # A non-empty `extraApps` switches the appstore off, which is left
          # that way on purpose: an app installed through the web UI would be
          # state this file does not describe.
          extraApps = { inherit (pkgs.nextcloud34.packages.apps) user_oidc; };

          settings = {
            overwriteprotocol = "https";
            trusted_proxies = [
              "127.0.0.1"
              "::1"
            ];
            default_phone_region = "AU";
            # `hide_login_form` is deliberately NOT set: the local form is the
            # break-glass path when kanidm is down, and the `ivy` admin account
            # above is what it logs into.
            #
            # Server-side encryption is likewise left off. A password-less app
            # token -- how Zotero authenticates, minted with `occ
            # user:auth-tokens:add` -- cannot perform operations that need the
            # login password, and encryption is one of them.
          };
        };

        # `postgresql-setup` is a separate oneshot from `postgresql` itself and
        # is the unit that runs ensureDatabases/ensureUsers; the module only
        # orders against `postgresql.target`, which lets setup reach a live
        # server with no `nextcloud` database yet.
        #
        # /mnt/hdd is a legacy-mountpoint ZFS dataset, so nothing derives a
        # dependency on it from the paths in this config -- every unit that
        # touches the instance has to be told.
        systemd.services = rcloneMounts // {
          nextcloud-setup = {
            after = [ "postgresql-setup.service" ];
            requires = [ "postgresql-setup.service" ];
            unitConfig.RequiresMountsFor = [ ncHome ];
          };
          nextcloud-cron.unitConfig.RequiresMountsFor = [ ncHome ];
          nextcloud-update-db.unitConfig.RequiresMountsFor = [ ncHome ];
          phpfpm-nextcloud.unitConfig.RequiresMountsFor = [ ncHome ];

          # --- kanidm as a login provider ---
          #
          # The provider row is a table in Nextcloud's database with no
          # config.php equivalent, so it gets reconciled with occ
          # (`user_oidc:provider` is an upsert keyed on the identifier).
          # `--clientsecret-file`, not `--clientsecret`: the latter would put
          # the secret in `ps`.
          #
          # `--unique-uid 0` with `--mapping-uid preferred_username` is what
          # makes the Nextcloud account id the bare kanidm username; at the
          # default the id is a hash of issuer+subject, which would not be the
          # `ivy` that Zotero's WebDAV path names.
          nextcloud-oidc = {
            description = "Register kanidm as a Nextcloud login provider";
            after = [ "nextcloud-setup.service" ];
            requires = [ "nextcloud-setup.service" ];
            wantedBy = [ "multi-user.target" ];
            unitConfig.ConditionPathExists = oidcSecretFile;
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "nextcloud";
            };
            script = ''
              ${occ} user_oidc:provider kanidm \
                --clientid nextcloud \
                --clientsecret-file ${oidcSecretFile} \
                --discoveryuri ${issuer}/.well-known/openid-configuration \
                --scope "openid profile email groups_name" \
                --unique-uid 0 \
                --mapping-uid preferred_username \
                --mapping-display-name name \
                --mapping-email email
            '';
          };

          # Hands Nextcloud the same serverinfo token the exporter presents.
          # `config:app:set` is an upsert, so this is idempotent, and the value
          # arrives via a file rather than an argument so it never shows in ps.
          nextcloud-metrics = {
            description = "Enable Nextcloud's serverinfo endpoint for prometheus";
            after = [ "nextcloud-setup.service" ];
            requires = [ "nextcloud-setup.service" ];
            wantedBy = [ "multi-user.target" ];
            before = [ "prometheus-nextcloud-exporter.service" ];
            unitConfig.ConditionPathExists = serverinfoTokenFile;
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "nextcloud";
            };
            script = ''
              ${occ} app:enable serverinfo
              ${occ} config:app:set serverinfo token \
                --value "$(cat ${serverinfoTokenFile})"
            '';
          };

          # Reconciles `oc_filecache` with what is actually on disk under the
          # account's own files, which is what makes the WRITABLE `nc-data`
          # Samba share in ../hosts/secondpc/samba.nix usable.
          #
          # Primary storage is the one place Nextcloud assumes it is the only
          # writer: it serves the web UI and every sync client from that index,
          # not from a directory listing. A scan is the supported way to tell
          # it otherwise, but it is reconciliation after the fact, so know what
          # it does NOT fix:
          #
          #   - a file written over SMB is invisible until the next scan, so
          #     there is a window of up to the timer interval;
          #   - no version is recorded for an SMB-side overwrite, and an
          #     SMB-side delete does not land in the trashbin -- both of those
          #     are Nextcloud-side features, and the file never went through
          #     Nextcloud;
          #   - a file changed on BOTH sides between scans resolves to whatever
          #     is on disk, with no conflict copy.
          #
          # `--path` rather than a bare user id: that form also walks every
          # external storage mounted for the account, and the rclone remotes
          # are slow, network-backed, and already rescanned on access.
          nextcloud-files-scan = {
            description = "Reconcile Nextcloud's file index with the filesystem";
            after = [ "nextcloud-setup.service" ];
            requires = [ "nextcloud-setup.service" ];
            serviceConfig = {
              Type = "oneshot";
              User = "nextcloud";
            };
            script = ''
              ${occ} files:scan --path=${lib.escapeShellArg "/${config.services.nextcloud.config.adminuser}/files"}
            '';
          };

          # The rclone mounts, as Nextcloud sees them. `files_external:create`
          # is not idempotent, so this checks first; `local` and `null::null`
          # are the identifiers those two classes actually register
          # (files_external's Lib/Backend/Local.php, Lib/Auth/NullMechanism.php),
          # and a mount point comes back from occ with a leading slash.
          #
          # Applicable to the `ivy` user rather than the `media-users` group:
          # occ only WARNS about an unknown group and creates the mount anyway,
          # and nothing here provisions Nextcloud groups, so a group would leave
          # a mount nobody can see.
          nextcloud-external-storage = {
            description = "Reconcile Nextcloud's external storage mounts";
            after = [ "nextcloud-setup.service" ];
            requires = [ "nextcloud-setup.service" ];
            wantedBy = [ "multi-user.target" ];
            # Deliberately NOT gated on `rcloneConfig` existing any more: the
            # SMB mount below needs no rclone remote, so gating the unit would
            # keep it unregistered until the cloud tokens were typed in. A
            # missing rclone mount point registers as an empty folder; occ does
            # not fail on it.
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "nextcloud";
            };
            path = [ pkgs.jq ];
            script = ''
              existing="$(${occ} files_external:list --output=json --all)"
              add() {
                if jq -e --arg m "/$1" 'any(.[]; .mount_point == $m)' <<<"$existing" >/dev/null; then
                  return 0
                fi
                ${occ} files_external:create "$1" local null::null \
                  -c datadir="$2" --applicable-user ivy
              }
              ${lib.concatStringsSep "\n" (
                lib.mapAttrsToList (
                  name: e: "add ${lib.escapeShellArg e.label} ${lib.escapeShellArg "${externalRoot}/${name}"}"
                ) externals
              )}
              add 'SMB' ${lib.escapeShellArg sharedDir}
            '';
          };

          # --- incremental backup to Backblaze B2 ---
          #
          # rclone on a timer, and a sync with `--backup-dir` rather than a bare
          # mirror: every file the sync would overwrite or delete goes into a
          # dated prefix instead, so a local deletion -- or a run of something
          # encrypting the files -- leaves the previous copy recoverable. A
          # plain `rclone sync` propagates the damage and destroys the only
          # other copy.
          #
          # The whole of `home` is the source, not just `data`: config.php holds
          # `instanceid`, `secret` and `passwordsalt`, without which the files
          # are not restorable into a new instance.
          nextcloud-backup = {
            description = "Back up Nextcloud to Backblaze B2";
            after = [ "nextcloud-setup.service" ];
            unitConfig = {
              # Skipped rather than failed while the B2 credentials have not
              # been written.
              ConditionPathExists = b2EnvFile;
              RequiresMountsFor = [ ncHome ];
            };
            path = [
              pkgs.rclone
              pkgs.zstd
              pkgs.coreutils
              config.services.postgresql.package
            ];
            serviceConfig = {
              Type = "oneshot";
              # Reads ${ncHome} and writes the dump, both nextcloud-owned, and
              # runs occ and pg_dump -- the latter is peer auth over the socket,
              # so it has to be this unix user. EnvironmentFile is read by the
              # service manager as root, so 0700 root state is still fine.
              User = "nextcloud";
              Group = "nextcloud";
              EnvironmentFile = b2EnvFile;
              RuntimeDirectory = "nextcloud-backup";
              Environment = [
                "HOME=%t/nextcloud-backup"
                "PGHOST=/run/postgresql"
                # The remote, defined entirely in the environment so there is
                # no second rclone config to keep in sync. The two credential
                # halves arrive the same way, from the EnvironmentFile above.
                #
                # `b2`, not `s3` against B2's compatibility endpoint: the
                # native backend costs fewer class-B/C transactions, and it
                # hands rclone B2's own SHA1 for every object, which is what
                # `--track-renames` below needs to recognise a moved file
                # instead of re-uploading it.
                "RCLONE_CONFIG_B2_TYPE=b2"
                # Deletes become hides rather than immediate removals, so a
                # `--backup-dir` move can still be undone server side. Bound
                # the growth with a bucket lifecycle rule in the dashboard.
                "RCLONE_CONFIG_B2_HARD_DELETE=false"
              ];
              # Maintenance mode comes back off even if the sync fails.
              ExecStopPost = "-${occ} maintenance:mode --off";
            };
            script = ''
              stamp="$(date -u +%Y-%m-%dT%H%M%SZ)"

              # The files and the database rows have to agree with each other,
              # so the instance is closed for the whole run rather than just for
              # the dump: a dump taken while uploads keep landing describes a
              # tree that never existed.
              ${occ} maintenance:mode --on
              pg_dump --no-owner --clean --if-exists nextcloud \
                | zstd -q -19 -f -o ${dumpDir}/nextcloud.sql.zst

              rclone sync ${ncHome} b2:${b2Bucket}/current \
                --backup-dir b2:${b2Bucket}/versions/"$stamp" \
                --track-renames \
                --fast-list \
                --transfers 8 \
                --stats-one-line

              ${occ} maintenance:mode --off
            '';
          };
        };

        # Every 5 minutes, and that interval IS the staleness window for a
        # file written over SMB. `Persistent` is deliberately off: a missed
        # scan is made good by the next one, and catching up on boot would
        # just add a full walk to every startup.
        systemd.timers.nextcloud-files-scan = {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnBootSec = "5m";
            OnUnitActiveSec = "5m";
            RandomizedDelaySec = "30s";
          };
        };

        systemd.timers.nextcloud-backup = {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "03:30";
            RandomizedDelaySec = "20m";
            Persistent = true;
          };
        };
      };
  };
}
