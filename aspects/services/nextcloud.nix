{
  den,
  lib,
  sharesFor,
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

  # ── THE RELAY, ASKED FOR ──
  #
  # Nextcloud had NO mailer configuration at all, which is not the same as
  # having none to do: `mail_smtpmode` defaults to `smtp` against `localhost`
  # with no From address, so a share notification or a password reset was a
  # mail with an empty sender handed to whatever was listening. This states the
  # one thing that is nextcloud's -- that it sends -- and reads the rest off
  # ./mail.nix.
  #
  # NO `localPart` IN THE TAKE, deliberately: the default is the aspect's own
  # name, so this is `nextcloud@ivymect.in` and the only service whose brand
  # needs saying is the one that says it (../hosts/secondpc/vaultwarden.nix).
  #
  # `linkFor` with both ends named, and `host = { }`, for the reasons
  # ./tandoor-recipes.nix's `oidc` block records: the From address is composed
  # per consumer, so `dial` -- which binds the giver's `peer` to the asking
  # HOST -- cannot carry it.
  mail =
    let
      l = den.lib.capability.linkFor den.aspects.mail "smtp" {
        host = { };
        peer = den.aspects.nextcloud;
      };
    in
    if l != null then
      l
    else
      throw "den.aspects.nextcloud: nothing furnishes an `smtp` endpoint, so there is no relay for share notifications and password resets. `den.capabilities.smtp` is declared in ./mail.nix beside the `gives` that answers it.";

  # NEXTCLOUD'S OWN SPELLING. `mail_smtpsecure` is `""`, `"tls"` (meaning
  # STARTTLS) or `"ssl"` (meaning implicit TLS) -- which is the same three
  # states under two confusingly-swapped names, so the mapping is written out
  # rather than guessed at the use site.
  # ── ONE PLACE FOR THE THREE DAV FACTS ──
  # `subdomain` was stated in `takes.proxy`, the account in `adminuser`, and
  # the path in a client's URL literal. The DAV path EMBEDS the username, which
  # is what made that name load-bearing outside Nextcloud in the first place --
  # so it is composed here rather than written out anywhere.
  # A RAW take, so `or`: the clause is absent unless stated.
  serviceAccountFor =
    taker: takerHost:
    "${(taker.takes.webdav-provisioned or { }).account or taker.name}-${lib.toLower takerHost.name}";

  dav = rec {
    subdomain = "cloud";
    username = "ivy";
    davPath = "/remote.php/dav/files/${username}";
  };

  mailSecure =
    {
      none = "";
      starttls = "tls";
      tls = "ssl";
    }
    .${mail.security};

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
  # Only the Backblaze credentials are still typed in by hand, because they are
  # ISSUED by Backblaze rather than chosen -- no generator can produce them.
  #
  # The three values that *are* just random strings (the admin password, the
  # kanidm client secret and the serverinfo token) are agenix generated secrets
  # in this aspect's own scope instead. They used to be hand-created here for
  # the reason ../homeassistant/default.nix still gives for its mosquitto
  # hashes -- a generated secret has to be minted before it exists on the box --
  # but that cost is one `nix run .#rekey && nix run .#gen-secrets` with the
  # yubikey present, against a standing requirement to remember four `openssl`
  # invocations and their exact ownership. The rest of this host's secrets
  # already work that way.
  #
  #   umask 077
  #   install -d -m 0700 /var/lib/nextcloud-state
  #
  #   # Backblaze B2 credentials for the backup below: an application key
  #   # scoped to the one bucket (Backblaze calls the two halves keyID and
  #   # applicationKey). These are rclone's own variable names for the `b2`
  #   # backend, not the AWS ones -- the native backend is used rather than
  #   # B2's S3 compatibility layer, so there is no endpoint to configure.
  #   printf 'RCLONE_CONFIG_B2_ACCOUNT=%s\nRCLONE_CONFIG_B2_KEY=%s\n' \
  #     "$keyId" "$appKey" > /var/lib/nextcloud-state/b2.env
  #
  #   # rclone remotes for the external storages, owned by the nextcloud user
  #   # because rclone rewrites this file whenever it refreshes a token.
  #   install -d -m 0700 -o nextcloud -g nextcloud /var/lib/nextcloud-rclone
  stateDir = "/var/lib/nextcloud-state";
  b2EnvFile = "${stateDir}/b2.env";
  rcloneConfig = "/var/lib/nextcloud-rclone/rclone.conf";

  # Nextcloud's `serverinfo` app (ships in core) serves the metrics document
  # the exporter reads, gated by a token rather than an admin login. Both sides
  # read this one file -- occ puts the value INTO Nextcloud, the exporter
  # presents it -- so they cannot drift.

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
  # ── WEBDAV, AND THE THING THAT SEPARATES TWO SERVERS OF IT ──
  #
  # Zotero's sync target was a literal `cloud.ivymect.in/remote.php/dav/files/ivy`
  # in ../programs/zotero.nix beside a hand-run script, so three facts the
  # server owns lived in the client: where the instance is published, which
  # account's DAV tree it is, and how a password for it comes about.
  #
  # TWO CAPABILITIES, BECAUSE THE THIRD FACT IS NOT UNIVERSAL. A DAV tree is
  # one thing; a DAV tree whose credential we can PROVISION is a stricter
  # thing, and the difference decides whether a client can be configured at
  # all without someone running a command against a live server.
  den.capabilities.webdav = {
    meta.description = "A WebDAV tree an account syncs against.";
    includes = [ den.capabilities.carried ];

    options = {
      username = lib.mkOption {
        type = lib.types.str;
        description = "The account whose DAV tree this is. It is part of the path, so it is not cosmetic.";
      };

      davPath = lib.mkOption {
        type = lib.types.str;
        description = "Path of that account's DAV root, below the published root.";
      };

      subdomain = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "The name this tree is published under, without the zone -- what a client composes its https URL from.";
      };
    };
  };

  # ── A DAV TREE WHOSE PASSWORD WE MINT AND PROVISION ON BOTH SIDES ──
  #
  # `credential` is already this tree's word for "a secret both ends of an edge
  # read, owned by neither": agenix generates it, the SERVER is configured with
  # it, the CLIENT reads the same file. Nothing is issued by a running service
  # and nothing has to be fetched, which is the only way a client's config can
  # be complete at build time.
  #
  # IT IS A NARROWING, so the closure does the work: a server that provisions
  # credentials also just serves DAV and satisfies a bare `takes.webdav`, while
  # a server that only serves DAV CANNOT answer `takes.webdav-provisioned` --
  # it fails to resolve, loudly, instead of a client silently being written
  # with a password nobody can produce.
  den.capabilities.webdav-provisioned = {
    meta.description = "A WebDAV tree whose password is minted by us and provisioned into both ends.";
    includes = [
      den.capabilities.webdav
      den.capabilities.credential
    ];
    taker.options = {
      account = lib.mkOption {
        type = lib.types.strMatching "[a-z][a-z0-9-]*";
        description = "The service account's name; each client host gets its own, suffixed with the host's name.";
      };
      shares = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Shares (../shares/) the account is added to, so it sees what their members see.";
      };
    };
  };

  den.aspects.nextcloud = {
    # ── WAS `den.serviceTypes.nextcloud` ──
    #
    # Every field of it is on the aspect now, which is where it belonged:
    # `reach`, the account it runs as, and the postgres ask (`takes.postgres`
    # below, which names no database and no owner because ./postgres.nix derives
    # both from this `user`).
    #
    # A HOUSE AMENITY -- not fleet infrastructure like celler, not host-local
    # like postgres.
    reach = "household";

    gives =
      {
        self,
        host ? { },
        ...
      }:
      {
        # ── A SERVICE ACCOUNT PER CLIENT HOST, WHOSE PASSWORD WE MINT ──
        # Not ivy's account: an account of its own, local to Nextcloud, so its
        # password is ours to set. One per (taker, host) because a credential
        # is one per link and a Nextcloud user has exactly one password.
        webdav-provisioned =
          { peer, ... }:
          let
            username = serviceAccountFor peer host;
          in
          {
            name = self.name;
            inherit username;
            davPath = "/remote.php/dav/files/${username}";
            inherit (dav) subdomain;
            scope = "webdav-${username}";
            secretName = "password";
            generator = "random-base64-32";
          };

        # A SERVICE THE HOUSE OFFERS. No `access`, and deliberately: the only
        # Nextcloud role is membership of its hardcoded `admin` group, and the
        # one declarative path into a Nextcloud group is user_oidc
        # provisioning it from a claim of the SAME NAME -- so an
        # `app-nextcloud-admin` group could never reach it. A member still
        # requests it; with no access character ../services/access.nix skips
        # the service and emits no group.
        service = _: { };

        user = _: {
          name = "nextcloud";
          group = "nextcloud";
          carrier = self.name;
        };

        # ── PLAIN `webdav`, AND THAT IS THE HONEST ANSWER ──
        #
        # NOT `webdav-provisioned`, because Nextcloud cannot do it. Its DAV
        # credential is an APP PASSWORD, and the only thing that can issue one
        # is `occ user:auth-tokens:add` against a RUNNING instance -- so it
        # cannot be generated by agenix, cannot be provisioned into the server
        # declaratively, and cannot be read back afterwards (only a hash is
        # stored). A client of this give therefore needs a password that came
        # from somewhere else, which is exactly what `webdav-provisioned`
        # exists to refuse.
        #
        # The alternative is the account's own password, which a DAV client
        # must not hold and which an OIDC-provisioned user does not have.
        #
        # The same `subdomain` `takes.proxy` publishes under and the same
        # account `nextcloud-admin` creates -- all read off `dav` so the three cannot
        # drift. A client had all three as literals.
        webdav = _: {
          name = self.name;
          inherit (dav) username davPath subdomain;

        };
      };

    includes = [
      # nginx + the wildcard ivymect.in ACME cert + the cloudflare tunnel that
      # already fronts every other *.ivymect.in vhost on this box.
      den.aspects.secondpc-web
      # The `postgres` quirk, which owns the server and collects every
      # service's database. See ./postgres.nix.
      den.aspects.postgres
      den.aspects.agenix-rekey
      # Shared folders. The shares themselves are aspects under ../shares/ and
      # are owned by whoever includes them; this aspect is only what provisions
      # the kanidm group each one is carried by. See ./shares.nix.
      den.aspects.shares
    ];

    # The three credentials that are just random strings. Each is consumed as a
    # FILE PATH -- by the nextcloud module, by kanidm, by occ and by the
    # prometheus exporter -- which is exactly what an agenix secret is, so these
    # stay separate secrets rather than being merged into one env template:
    # there is no consumer here that reads an env file.
    #
    # The sharing is done with ownership, not with copies: `oidc-client-secret`
    # is ONE SOURCE materialised twice -- `oidc-client-secret` owned by kanidm
    # and `oidc-client-secret-nextcloud` owned by nextcloud, both decrypted from
    # the same `.age` file -- so the two readers cannot drift and neither needs
    # a shared group. The serverinfo token still uses the group shape, because
    # there occ WRITES the value and the exporter reads it.
    secrets = {
      # The admin account's initial password (`nextcloud-admin` below). base64
      # rather than hex because it is a password a human may have to type once.
      admin-password = {
        owner = "nextcloud";
        generator.script = "random-base64-32";
      };

      # ── ONE SOURCE, TWO MATERIALISATIONS ──
      #
      # Read by kanidm (as `basicSecretFile`) and by occ (as
      # `--clientsecret-file`). It used to be ONE agenix secret owned by
      # `kanidm`, group `nextcloud-oidc` at mode 0640, with a unix group
      # manufactured solely so both could read the same bytes -- and the comment
      # justifying it said the alternative was "either side getting a copy that
      # can drift".
      #
      # That was the wrong axis. The two readers do not need one FILE ON DISK;
      # they need one SOURCE. agenix-rekey is happy with one `.age` file backing
      # two secrets on a host, so each service gets its own decrypted copy,
      # owned by itself at 0400, both derived from the same encrypted source.
      # They cannot drift because there is one source, and no third party sits
      # in a group that can read a credential it has no use for.
      #
      # ONLY ONE OF THEM CARRIES THE GENERATOR, and this is the part that is
      # easy to get wrong in a way nothing catches until runtime: give both a
      # `generator.script = "random-hex-32"` and each mints its OWN random
      # value, so kanidm would hold one secret and Nextcloud another. That is
      # precisely the drift the group existed to prevent, except silent. The
      # second entry carries no generator and points at the first's `rekeyFile`.
      oidc-client-secret = {
        owner = "kanidm";
        mode = "0400";
        restartUnits = [ "kanidm.service" ];
        generator.script = "random-hex-32";
      };

      # The same bytes, for occ. No generator, no group: just this service's own
      # copy of the one source above.
      # A FUNCTION, not a plain attrset, because it has to name its sibling.
      # ../../lib/age-scoped.nix's `entryType` is a submodule whose
      # `_module.args` carry the scope-local views, and its own comment says so:
      # "An entry is a module, not a plain attrset: `age.secrets.<n>` is a
      # submodule upstream, so `templates.env = { secrets, ... }: { ... }` has
      # always been legal." `config` is NOT in scope here -- the aspect-level
      # `secrets` block is not a module function -- and reaching for it is an
      # `undefined variable 'config'` rather than anything subtler.
      oidc-client-secret-nextcloud =
        { secrets, ... }:
        {
          inherit (secrets.oidc-client-secret) rekeyFile;
          owner = "nextcloud";
          mode = "0400";
        };

      # occ writes the value into Nextcloud; the exporter presents it.
      serverinfo-token = {
        owner = "nextcloud";
        group = "nextcloud-metrics";
        mode = "0640";
        generator.script = "random-hex-32";
      };
    };

    # Replaces `database.createLocally` (see the note on it below). Socket peer
    # auth, so the unix user `nextcloud` owns the role of the same name and no
    # password exists anywhere.
    # ── WHERE IT PUBLISHES AND HOW BIG AN UPLOAD IT TAKES ──
    #
    # No `port` and no `upstream`, deliberately: the nixpkgs module declares
    # this service's virtualHost itself (php-fpm, the .well-known redirects,
    # `client_max_body_size` from `maxUploadSize`), so a second entry would be a
    # conflicting definition rather than an override. ../services/proxy-nginx.nix
    # records the take and emits no vhost for it.
    #
    # What this IS for is the body limit. `10G` and the cloudflare tunnel's
    # 100MB cap are the same constraint seen from two ends, and until now only
    # one end knew: `cache.ivymect.in` was excluded from the tunnel by hand
    # while `cloud.ivymect.in` was not, so an upload over 100MB failed
    # MID-FLIGHT at run time with the vhost cheerfully advertising 10G.
    #
    # Declared here, ../network/cloudflare.nix's `excluded` derives the
    # exclusion from the arithmetic and the two cannot disagree again.
    takes.proxy = {
      inherit (dav) subdomain;
      maxBodyBytes = 10 * 1000 * 1000 * 1000;
    };

    # NO `database` OR `owner`: both are the asker's own name, which
    # ../services/postgres.nix's `gives.postgres` derives. They were literals
    # here and in the app's own config, with nothing checking the two agreed.
    #
    # `nextcloud-setup` is the one unit that touches the database; the rest of
    # the instance's units are ordered behind it by the nixpkgs module.
    takes.postgres.services = [ "nextcloud-setup" ];

    # ── IT SENDS SHARE NOTIFICATIONS AND PASSWORD RESETS ──
    #
    # Empty, and that is the whole declaration: the local part defaults to this
    # aspect's name and the relay, the port and the domain are the
    # mailserver's. See the `mail` link above.
    takes.smtp = { };

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

    # `kanidmClients.<name>`: the shared OIDC shape (short username, scope
    # maps, landing page, the domain) is defaulted by `gateway.oidcClients` in
    # ./gateway.nix, so this is only what differs. The name is written once
    # here; keying it off the aspect is not possible (see that option's note),
    # and keying it off the service entity waits on `{ service, ... }` binding.
    kanidmClients.nextcloud = {
      displayName = "Nextcloud";
      # Published at `cloud.<domain>`, not `nextcloud.<domain>`.
      subdomain = "cloud";
      # user_oidc's `login#code` route (appinfo/routes.php), i.e.
      # /apps/<appid>/code.
      callback = "/apps/user_oidc/code";
      # user_oidc maps groups to Nextcloud groups, so they have to arrive.
      groupsName = true;
    };

    nixos =
      {
        config,
        pkgs,
        scoped,
        host ? { },
        ...
      }:
      let
        occ = lib.getExe config.services.nextcloud.occ;

        # The declared shares, keyed by name (./shares.nix). Applied here
        # because `host` -- whose accounts have which share -- is all it needs.
        shares = sharesFor host;

        # The three generated credentials, by path. Named here so the use sites
        # below read the same as they did when these were hand-created files.
        adminpassFile = scoped.nextcloud.secrets.admin-password.path;
        # THE NEXTCLOUD-OWNED COPY, read by occ as the `nextcloud` user. kanidm
        # reads `oidc-client-secret` instead, which is its own copy of the same
        # source -- same bytes, different owner, neither able to read the
        # other's file. Pointing occ at kanidm's copy is what used to require
        # the `nextcloud-oidc` group.
        oidcSecretFile = scoped.nextcloud.secrets.oidc-client-secret-nextcloud.path;
        serverinfoTokenFile = scoped.nextcloud.secrets.serverinfo-token.path;

        # Every client of `webdav-provisioned`, one per host it is on.
        serviceEdges = map (e: {
          inherit e;
          r = den.lib.capability.realise "webdav-provisioned" {
            giver = den.aspects.nextcloud;
            taker = e.aspect;
            host = e.host;
            peer = e.aspect;
            giverHost = host;
            takerHost = e.host;
          };
        }) (den.lib.capability.takerEdgesReaching den.aspects.nextcloud "webdav-provisioned" host);
        serviceAccounts = map (
          { e, r }:
          {
            username = serviceAccountFor e.aspect e.host;
            passwordFile = r.giver.pathIn scoped;
            groups = map (n: shares.${n}.group) (e.aspect.takes.webdav-provisioned.shares or [ ]);
          }
        ) serviceEdges;
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
        # This end of each service account's credential: the server's copy,
        # owned by nextcloud, beside the taker's under one source.
        age.scoped = lib.listToAttrs (
          map ({ r, ... }: lib.nameValuePair r.scope { secrets = r.giverSecrets; }) serviceEdges
        );

        # A CONFIDENTIAL client -- user_oidc has a server side and takes a
        # client secret, unlike Home Assistant's public PKCE client.
        #

        # `users.groups.nextcloud-oidc` IS GONE. It existed only so kanidm and
        # nextcloud could read one file, and the client secret is now one SOURCE
        # materialised twice -- each service owning its own 0400 copy -- so
        # there is nothing left for a shared group to grant. Dropping it also
        # removes a group in which either member could read a credential the
        # other owns.
        #
        # `nextcloud-metrics` below is NOT the same case and stays: occ writes
        # the serverinfo token and the exporter reads it, and the exporter sets
        # `DynamicUser = false` so it has a stable user to grant.

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

          # Postgres over the unix socket, with socket peer auth -- so there is
          # no database password anywhere.
          #
          # `createLocally = false` and the database declared on the `postgres`
          # quirk instead (see the `postgres` block above): `createLocally`
          # writes straight into the list-typed
          # `services.postgresql.ensureDatabases`/`ensureUsers`, which works but
          # means every service that touches postgres has to know what the
          # others already put there. The quirk is the one collector, so the
          # coupling is gone -- and it is also what turns the server on.
          database.createLocally = false;
          config = {
            dbtype = "pgsql";
            dbname = "nextcloud";
            dbuser = "nextcloud";
            dbhost = "/run/postgresql";
            # `ivy`, not the repo's usual `auscyber`: the WebDAV path embeds the
            # username, so this name is load-bearing outside Nextcloud. Read off
            # `dav` above, which is also what `gives.webdav` furnishes.
          };

          # The official OIDC app. `extraApps` is what pins it to the store
          # instead of the appstore, and `extraAppsEnable` (default true) is what
          # has nextcloud-setup run `occ app:enable` for it. Taken off the
          # pinned package so the app and the server cannot drift apart.
          #
          # A non-empty `extraApps` switches the appstore off, which is left
          # that way on purpose: an app installed through the web UI would be
          # state this file does not describe.
          # `groupfolders` is what makes a share declarable at all: it is the one
          # Nextcloud mechanism where a folder's existence, its quota and which
          # group may do what in it are all reachable from `occ`. 22.0.6 declares
          # `<nextcloud min-version="34" max-version="34" />` in its own appinfo,
          # so it moves with the server package above and cannot drift from it.
          # (The UI calls them "Team folders" as of 22; the occ names are unchanged.)
          extraApps = {
            inherit (pkgs.nextcloud34.packages.apps)
              user_oidc
              groupfolders
              ;
          };

          settings = {
            overwriteprotocol = "https";

            # user_oidc reads the groups claim out of the ID TOKEN
            # (`provisionUserGroups($user, $providerId, $idTokenPayload)`), and
            # whether kanidm puts `groups_name` there as well as in userinfo is
            # not something this repo can establish at eval time. This flag
            # merges the userinfo document into that payload, giving ID-token
            # values PRIORITY and only filling in what is missing
            # (Controller/LoginController.php `enrich_login_id_token_with_userinfo`),
            # so it is correct either way instead of correct only if the claim
            # happens to be in the token.
            #
            # Cost: one extra userinfo request per login. IF LOGINS BREAK after
            # this change, this is the line to remove first -- it is the only
            # one here that adds a round trip to the login path.
            user_oidc.enrich_login_id_token_with_userinfo = true;
            trusted_proxies = [
              "127.0.0.1"
              "::1"
            ];
            default_phone_region = "AU";

            # ── MAIL, EVERY FIELD FROM THE LINK ──
            #
            # `mail_from_address` is the LOCAL PART and `mail_domain` the
            # domain -- nextcloud concatenates them itself -- which is why
            # ./mail.nix furnishes the address in parts as well as whole.
            #
            # `mail_smtpauth = false` is the furnished `credentialFile` being
            # null, written as the boolean this app wants rather than as a
            # decision taken here.
            mail_smtpmode = "smtp";
            mail_smtphost = mail.relayHost;
            mail_smtpport = mail.relayPort;
            mail_smtpsecure = mailSecure;
            mail_smtpauth = mail.credentialFile != null;
            mail_from_address = mail.fromLocalPart;
            mail_domain = mail.fromDomain;
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

        # Ordering against `postgresql-setup` comes from the `postgres` quirk's
        # `services` (./postgres.nix) rather than being written here.
        #
        # /mnt/hdd is a legacy-mountpoint ZFS dataset, so nothing derives a
        # dependency on it from the paths in this config -- every unit that
        # touches the instance has to be told.
        systemd.services = rcloneMounts // {
          nextcloud-setup.unitConfig.RequiresMountsFor = [ ncHome ];
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
          # ── THE ADMIN ACCOUNT ──
          # nixpkgs installs with `--disable-admin-user` now, so the account
          # `dav` names is created here: once, with the generated password, and
          # kept in `admin` on every run. An existing account's password is
          # left alone.
          nextcloud-admin = {
            description = "Ensure the Nextcloud admin account exists";
            after = [ "nextcloud-setup.service" ];
            requires = [ "nextcloud-setup.service" ];
            before = [ "nextcloud-oidc.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "nextcloud";
            };
            path = [ pkgs.jq ];
            script = ''
              user=${lib.escapeShellArg dav.username}
              if ! ${occ} user:info "$user" >/dev/null 2>&1; then
                OC_PASS="$(<${adminpassFile})"
                export OC_PASS
                ${occ} user:add --password-from-env --group admin "$user"
              elif ! ${occ} user:info --output=json "$user" | jq -e '.groups | index("admin")' >/dev/null; then
                ${occ} group:adduser admin "$user"
              fi
            '';
          };

          nextcloud-oidc = {
            description = "Register kanidm as a Nextcloud login provider";
            after = [ "nextcloud-setup.service" ];
            requires = [ "nextcloud-setup.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "nextcloud";
            };
            #
            # --- group provisioning, which is what makes ./shares.nix work ---
            #
            # The `groups_name` SCOPE was already requested here and did
            # nothing: user_oidc only looks at a groups claim when
            # `--mapping-groups` names it AND `--group-provisioning` is on
            # (Command/UpsertProvider.php EXTRA_OPTIONS). Both are set below,
            # and the Nextcloud groups the shares live in then appear and fill
            # themselves from kanidm on each login.
            #
            # `--group-whitelist-regex` is NOT optional decoration. user_oidc's
            # `provisionUserGroups` RECONCILES: a group the user is in that the
            # token does not carry is REMOVED from them. Unwhitelisted, one
            # login would therefore strip a Nextcloud-local group membership --
            # including `admin`. The regex scopes both halves (import and
            # removal, Service/ProvisioningService.php) to the one namespace
            # ./shares.nix owns, so everything else Nextcloud knows about a
            # user is left alone. Anchored, because it is a blast radius.
            script = ''
              ${occ} user_oidc:provider kanidm \
                --clientid nextcloud \
                --clientsecret-file ${oidcSecretFile} \
                --discoveryuri ${issuer}/.well-known/openid-configuration \
                --scope "openid profile email groups_name" \
                --unique-uid 0 \
                --mapping-uid preferred_username \
                --mapping-display-name name \
                --mapping-email email \
                --mapping-groups groups_name \
                --group-provisioning 1 \
                --group-whitelist-regex '^share-'
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

          # --- shared folders ---
          #
          # `sharesFor host` is ./shares.nix's view of every aspect that takes
          # the `share` capability at an account on this host: one record per
          # share, with the member set derived from WHICH accounts have it.
          # Nothing here learns who anybody is.
          #
          # What is genuinely declarative and what is not, precisely:
          #
          #   DECLARATIVE  the kanidm group and its membership (./shares.nix,
          #                via `provision.groups` -- kanidm removes a member
          #                the config no longer lists, so dropping a share
          #                aspect from a person revokes it).
          #   RECONCILED   the folder's existence, its quota and the group's
          #                permissions in it. These live in groupfolders' own
          #                tables with no config.php equivalent, so they are
          #                `occ` calls in this oneshot. Each is an upsert or
          #                guarded, so re-running is free -- but it IS a
          #                oneshot, not a rendered file: a change takes effect
          #                at activation, not at eval.
          #   NOT EITHER   the Nextcloud ACCOUNT. user_oidc creates it on first
          #                login and there is no supported way to pre-create an
          #                OIDC-backed user, so a person who has never logged in
          #                is in the kanidm group, is in the Nextcloud group the
          #                moment they log in, and does not exist until then.
          #                Nothing to do about it; nothing breaks meanwhile.
          #
          # BY HAND, the two things this does not do:
          #
          #   * RENAMING a share's `displayName` does not rename the folder.
          #     groupfolders' only stable handle is its numeric id and its
          #     mount point is the name, so a changed name reads as a NEW
          #     share here and the old folder stays beside it. Rename it with
          #       sudo -u nextcloud nextcloud-occ groupfolders:rename <id> <new>
          #     (ids from `groupfolders:list`) and this unit will then match it.
          #   * DELETING a share aspect leaves its folder and its contents.
          #     That is on purpose: `groupfolders:delete` destroys files, and a
          #     config change must not. Access is already gone (the kanidm
          #     group went with the aspect). Reclaim the space deliberately:
          #       sudo -u nextcloud nextcloud-occ groupfolders:delete <id>
          nextcloud-shares = {
            description = "Reconcile Nextcloud team folders with the declared shares";
            after = [ "nextcloud-setup.service" ];
            requires = [ "nextcloud-setup.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "nextcloud";
            };
            path = [ pkgs.jq ];
            script = ''
              # Enabled by nextcloud-setup already, since `groupfolders` is in
              # `extraApps` and `extraAppsEnable` defaults on. Restated so the
              # unit does not depend on that default staying true.
              ${occ} app:enable groupfolders

              existing="$(${occ} groupfolders:list --output=json)"

              share() {
                mount="$1"
                group="$2"
                quota="$3"
                shift 3

                # The Nextcloud group has to exist FIRST: `groupfolders:group`
                # returns -1 on a group it cannot resolve (groupfolders
                # Command/Group.php), and an OIDC-provisioned group does not
                # exist until somebody has logged in. Creating it here is what
                # lets the folder be bound before anyone has.
                if ! ${occ} group:info "$group" >/dev/null 2>&1; then
                  ${occ} group:add "$group" --display-name "$mount"
                fi

                # `groupfolders:create` FAILS on an existing mount point rather
                # than returning its id, so look first. The JSON key is
                # `mountPoint`, renamed from `mount_point` by the command itself.
                id="$(jq -r --arg m "$mount" \
                  'map(select(.mountPoint == $m)) | .[0].id // empty' <<<"$existing")"
                if [ -z "$id" ]; then
                  id="$(${occ} groupfolders:create "$mount")"
                fi

                ${occ} groupfolders:quota "$id" "$quota"
                ${occ} groupfolders:group "$id" "$group" "$@"
              }

              ${lib.concatStringsSep "\n" (
                lib.mapAttrsToList (
                  _: sh:
                  lib.escapeShellArgs (
                    [
                      "share"
                      sh.displayName
                      sh.group
                      # Quota.php lowercases and takes the literal string
                      # `unlimited` for no cap; it has no "unset" form.
                      (if sh.quota == null then "unlimited" else sh.quota)
                    ]
                    ++ sh.permissions
                  )
                ) shares
              )}
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
          # ── THE SERVICE ACCOUNTS, PROVISIONED ──
          # Created, set to the minted password and put in their shares' groups
          # on every run: the file is the truth, and nobody logs in as these.
          nextcloud-service-accounts = {
            description = "Provision Nextcloud's service accounts from their minted passwords";
            after = [
              "nextcloud-setup.service"
              "nextcloud-shares.service"
            ];
            requires = [ "nextcloud-setup.service" ];
            wants = [ "nextcloud-shares.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "nextcloud";
            };
            path = [ pkgs.jq ];
            script = ''
              account() {
                user="$1"
                OC_PASS="$(<"$2")"
                export OC_PASS
                shift 2
                if ${occ} user:info "$user" >/dev/null 2>&1; then
                  ${occ} user:resetpassword --password-from-env "$user"
                else
                  ${occ} user:add --password-from-env "$user"
                fi
                for group in "$@"; do
                  if ! ${occ} user:info --output=json "$user" | jq -e --arg g "$group" '.groups | index($g)' >/dev/null; then
                    ${occ} group:adduser "$group" "$user"
                  fi
                done
              }

              ${lib.concatMapStringsSep "
" (
                a:
                lib.escapeShellArgs (
                  [
                    "account"
                    a.username
                    a.passwordFile
                  ]
                  ++ a.groups
                )
              ) serviceAccounts}
            '';
          };

          nextcloud-files-scan = {
            description = "Reconcile Nextcloud's file index with the filesystem";
            after = [ "nextcloud-setup.service" ];
            requires = [ "nextcloud-setup.service" ];
            serviceConfig = {
              Type = "oneshot";
              User = "nextcloud";
            };
            script = ''
              ${occ} files:scan --path=${lib.escapeShellArg "/${dav.username}/files"}
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

  # NEXTCLOUD AS A SERVICE DEFINITION -- the first of the fifteen household
  # services to enter `den.serviceTypes`, and the one that forces the question,
  # because it has both relations the model exists for.
  #
  # ./nextcloud.nix stays exactly as it is and keeps producing the config. What
  # this adds is the service's RELATIONS, so the things currently arranged by hand
  # have somewhere to live:
  #
  #   uses.postgres   today the `postgres` quirk. NOT because a quirk is
  #                   one-way -- that claim was written here first and is false,
  #                   den resolves config-dependent emissions inside the
  #                   consuming fixpoint -- but because a quirk's collector is a
  #                   FIXED aspect, so nothing can answer "which postgres". That
  #                   is the gap, and it is what
  #                   the server reads its dependents but can never tell nextcloud
  #                   how to reach it. As an edge the connection comes back down,
  #                   and `uses.unix` is what makes peer auth a resolved fact
  #                   rather than an assumption ("no password exists anywhere").
  #
  #   uses.kanidm     today TWO hand-written `age.secrets` entries sharing one
  #                   `rekeyFile`, with the second naming the first. That is the
  #                   right MECHANISM -- one source, materialised per reader, each
  #                   owning its own 0400 copy -- applied by hand. As a fiber the
  #                   credential belongs to the EDGE, both ends materialise
  #                   because an edge has two ends, and nothing names a path or a
  #                   sibling. NOT DECLARED YET: there is no `kanidm` definition,
  #                   and inventing one here without its instance would be an edge
  #                   that cannot resolve.
  #
  # `aspect = null`, deliberately and for the reason §3a measured: every service
  # aspect includes `den.aspects.gateway`, so resolving one in its own service
  # scope double-declares the shared forward
  # (`den.fwd."provision/nixos/services/kanidm/provision" is already declared`).
  # That is structural, not a kanidm detail. So the aspect keeps running at HOST
  # scope and this entry carries the relations only -- which is also why adding it
  # changes no host's evaluated config.
  #
  # `reach`: the HOUSEHOLD. Nextcloud is a house amenity, not fleet
  # infrastructure like celler and not host-local like postgres -- so it is
  # planted in `household:ivymectin` and visible to the hosts that serve that
  # household, which is exactly what `reach` being a reference rather than a word
  # is for.
}
