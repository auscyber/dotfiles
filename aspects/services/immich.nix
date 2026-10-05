{ den, ... }:
# Immich (photo and video library), behind the gateway on its own vhost.
#
# `oauthProxy = false`, like ./audiobookshelf.nix and ./vikunja.nix: Immich has real
# per-user accounts, per-user albums and native mobile apps. oauth2-proxy would
# hand it one trusted identity for everybody, and the mobile apps cannot carry
# an oauth2-proxy cookie at all.
#
# --- OIDC IS DELIBERATELY NOT WIRED ---
#
# Immich's own docs (docs.immich.app/administration/oauth) require THREE
# redirect URIs registered at the IdP:
#
#   https://immich.<domain>/auth/login       web sign-in
#   https://immich.<domain>/user-settings    linking an existing account
#   app.immich:///oauth-callback             the mobile app, or the
#                                            `/api/oauth/mobile-redirect`
#                                            override for an IdP that rejects a
#                                            custom scheme
#
# `gateway.oidcClients` in ./gateway.nix renders exactly one -- ./kanidm.nix
# builds `originUrl = "${origin}${callback}"` from a single `callback` -- and
# kanidm's own option documents that origins "need to exactly match the OAuth2
# redirect target", so a one-URI client leaves account linking and the app
# broken rather than merely unconfigured. Immich also defaults
# `oauth.signingAlgorithm` to RS256 while kanidm issues ES256 unless the client
# sets `enableLegacyCrypto`, which that option does not expose either.
#
# Wiring it is therefore a change to ./gateway.nix (a list-valued `callbacks`)
# and ./kanidm.nix (render it), which touches all seven existing clients. Until
# that happens Immich uses its own local accounts: the first visit bootstraps the
# admin and every other account is created in its own UI.
#
# Note what that means for the vhost. `oauthProxy = false` takes oauth2-proxy off the
# browser leg entirely, so NOTHING in front of Immich checks a kanidm group --
# the `groups` on the gated entry and the `svc-immich` group the gateway mints
# are inert here, exactly as they are for ./audiobookshelf.nix and ./vikunja.nix.
# Immich's own login page is the only gate, and it faces the internet through
# the cloudflare tunnel. Do the first visit promptly: Immich lets whoever gets
# there first claim the admin account.
#
# --- SETTINGS STAY IN THE ADMIN UI ---
#
# `services.immich.settings` is left at its `null` default on purpose. Any
# non-null value makes the module render IMMICH_CONFIG_FILE, and Immich then
# treats that file as authoritative for the WHOLE admin config -- every
# unrelated setting (storage template, ML job concurrency, transcoding) becomes
# read-only in the UI, not just the ones declared here. Two things have to be
# set once by hand after the first deploy:
#
#   Administration -> Settings -> Server
#     External Domain   https://immich.<domain>      (shared links, app QR)
#   Administration -> Settings -> Video Transcoding
#     leave acceleration as "disabled" -- see `accelerationDevices` below
let
  # THE PORT, ONCE: read by `gives.tcp` below -- which is what
  # ../proxy-nginx.nix resolves the upstream from -- and by the service's own
  # NixOS option, which is what actually makes it listen there. They were the
  # same number written twice.
  port = 2283;
in
{
  den.aspects.immich = {
    includes = [
      den.aspects.gateway
      den.aspects.postgres
      # ./restic.nix's `backup` quirk -- see the `backup` entry below.
      den.aspects.restic
    ];

    # ── WHAT IT LISTENS ON, so the proxy can ASK rather than assume ──
    #
    # Given rather than passed to the gate as a number: ../../base/capability.nix
    # composes this endpoint with whichever carrier the two ends share -- the
    # loopback for nginx on this box -- so ../proxy-nginx.nix resolves the
    # upstream rather than hardcoding one, and the same endpoint is what makes
    # this service's public https address derivable.
    #
    # NOT EXPOSED: loopback only, because the gate is the way in. The firewall
    # is derived from this (../../base/firewall.nix), so saying nothing here
    # leaves the port shut on every wire.
    gives =
      { self, ... }:
      {
        # ── WHAT A GRANT ON IMMICH WOULD MEAN: NOTHING NIX CAN DO ──
        #
        # Immich keeps its own accounts and its OIDC role mapping is set in its
        # admin UI, so there is nothing here to open. No `access` either, which
        # is why no household declares this service and no grant on it exists
        # today -- this is the declaration that keeps it that way honestly
        # rather than by omission, and it is what ./access.nix's
        # `access-grants-land` check would read if one were ever declared.
        service = _: {
          externallyManaged = true;
        };

        tcp = _: {
          name = self.name;
          ports.http = {
            inherit port;
            meta.description = "Immich's server API and web app, which the gate proxies for both the browser and the mobile client.";
          };
        };

        # ── THE DASHBOARD TILE ──
        #
        # No `href`: the gate publishes this at `gateway.services.immich.url`,
        # which ../services/homepage.nix derives from the vhost and the subpath
        # rather than having it written here a second time.
        #
        # NO WIDGET, and it is the plex case rather than an omission. Immich's
        # widget authenticates with an `x-api-key` minted inside Immich's own
        # UI, so it is not the gateway key homepage holds -- and Immich names no
        # `api.clients`, so the dashboard correctly declines to present one. A
        # widget with no credential 401s on every refresh instead of sitting
        # quiet.
        homepage-widget = _: {
          group = "Media";
          icon = "immich.svg";
          description = "Photos and video";
        };
      };

    takes.gated = {
      # Immich authenticates itself -- see the header.
      oauthProxy = false;
      # socket.io: upload progress, job counts and new-asset notifications.
      websockets = true;
    };

    # `createDB = false` below, so this is the only thing that creates the
    # database and the role. Peer auth over the unix socket: the unit runs as a
    # real unix user named `immich`, so it reaches the role of the same name
    # with no password anywhere.
    #
    # NEITHER `extensions` NOR `preload`, and both omissions are deliberate --
    # the module already contributes exactly what Immich needs, and this quirk
    # unions rather than deduplicates:
    #
    #   * `extensions` is pgvector + vectorchord (NOT pgvecto.rs -- the pinned
    #     module removes `database.enableVectors` and `enableVectorChord`
    #     outright, "from now on vectorchord is used instead", so there is
    #     nothing to choose). `services.postgresql.extensions` is a `functionTo`
    #     whose results CONCATENATE, so restating them changes
    #     `postgresql.finalPackage` into a buildEnv holding each one twice --
    #     harmless (buildEnv skips an identical duplicate, and neither package
    #     has `wrapperArgs`) but a different store path for no gain.
    #
    #   * `preload` is `vchord.so`. `shared_preload_libraries` is `types.commas`
    #     under the hood, so definitions are JOINED, not merged -- the real value
    #     on this host is already `vchord.so,pg_cron` (./splitpro.nix owns the
    #     second), and restating it here would render `vchord.so,pg_cron,vchord.so`.
    #
    # Also worth knowing, since it is server-global and set by the module rather
    # than by anything here: `search_path` becomes `"$user", public, vectors`
    # for EVERY database on this cluster, not just immich's.
    takes.postgres.services = [
      "immich-server"
      "immich-machine-learning"
    ];

    # The photos. Everything irreplaceable on this box in one tree, minus the
    # two subtrees Immich can regenerate -- its own docs say only `library`,
    # `upload` and `profile` carry original content, and that restoring without
    # `thumbs`/`encoded-video` just means re-running those jobs.
    #
    # `backups` is excluded as well: it holds Immich's own pg_dump output, and
    # ./restic.nix already dumps every database in `ensureDatabases`, so keeping
    # it would ship the same rows twice.
    #
    # The whole root is declared rather than the three wanted subdirectories
    # because Immich creates those lazily -- restic fails a snapshot on a path
    # that does not exist, which on a fresh install would be every one of them.
    #
    # WORTH KNOWING: this is the largest thing in the repository's one restic
    # snapshot by orders of magnitude, and the repository is offsite. Check the
    # B2 bill before pointing the mobile apps at it.
    backup =
      { config, ... }:
      let
        root = config.services.immich.mediaLocation;
      in
      {
        name = "immich";
        paths = [ root ];
        exclude = [
          "${root}/thumbs"
          "${root}/encoded-video"
          "${root}/backups"
        ];
      };

    nixos =
      { config, ... }:
      {
        services.immich = {
          enable = true;
          # Loopback only: nginx is the one way in, the same reasoning as
          # ./audiobookshelf.nix's `host`.
          host = "127.0.0.1";
          port = port;

          # NOT /var/lib/immich, which is on zroot (the SSD holding `/`). A
          # photo library belongs on the data pool with the rest of the media.
          #
          # HARDCODED, and it should not stay that way: there is no host-level
          # storage root in this tree yet, so this follows what
          # ./audiobookshelf.nix and ../hosts/secondpc/media.nix already do.
          # Point it at `<root>/Immich` once such an option exists. A dataset of
          # its own under `zpool` would also be better than a directory --
          # snapshots and a quota for the one tree that will grow without bound
          # -- but that is a disko change in the host file.
          mediaLocation = "/mnt/hdd/Immich";

          database = {
            # Keeps the module's `postgresql-setup` ExecStartPost, which is the
            # load-bearing half: it CREATEs vector, vchord, cube, earthdistance,
            # pg_trgm, unaccent and uuid-ossp inside the immich database and
            # reindexes the face/clip indexes when vectorchord updates. Immich
            # cannot do that itself -- CREATE EXTENSION needs superuser.
            enable = true;
            # ...but not its `ensureDatabases`/`ensureUsers`, which would be a
            # second copy of what the `postgres` quirk above already declares.
            createDB = false;
          };

          # Machine learning stays ON (the module's default) but gets NO
          # accelerator. secondpc is an i3-4170 with Haswell HD 4400 graphics;
          # Immich's OpenVINO path wants Gen 9 (Skylake) or newer, and there is
          # no discrete GPU, so `accelerationDevices` is left at `[ ]` -- which
          # the module turns into `PrivateDevices`, i.e. no /dev/dri at all.
          # Smart search and face detection will work on two cores, slowly.
          #
          # Video transcoding has the same answer for now. The render device
          # exists and ../hosts/secondpc/plex.nix uses it for VAAPI, so granting
          # Immich `[ "/dev/dri/renderD128" ]` and selecting QSV in the admin UI
          # is plausible -- but Haswell's encoder is H.264-only and both services
          # would be contending for one device, so that is a deliberate change to
          # make and measure, not a default.
        };

        # The media root. The module's own entry for this path is an `e` rule,
        # which only ADJUSTS a directory that already exists -- nothing creates
        # one outside /var/lib, so without this Immich starts with no library
        # directory at all.
        #
        # Written into the module's own `immich` settings key, not a key of our
        # own, so both rules land in one tmpfiles.d file and `d` is emitted
        # before `e` (the types are an attrset, so they come out sorted). That
        # order is what matters: systemd-tmpfiles keeps the FIRST rule it reads
        # for a path and skips a later compatible one, and it only counts them
        # compatible when owner, group, mode and age all agree -- hence the mode
        # being spelled out identically to the module's rather than left at "-".
        systemd.tmpfiles.settings.immich."/mnt/hdd/Immich"."d" = {
          user = "immich";
          group = "immich";
          mode = "0700";
        };

        # Uploads. nginx defaults `client_max_body_size` to 1m, so without this
        # every video and most RAW files are a 413 before Immich sees them, and
        # a slow upload over a home connection trips the 60s proxy timeouts.
        # ./gateway.nix has no per-entry escape hatch for this and it is specific
        # to one service, so it merges into the vhost the gated entry created
        # rather than replacing it.
        #
        # `proxy_request_buffering off` so a multi-gigabyte upload streams to
        # Immich instead of landing in /var/lib/nginx first -- that is on zroot,
        # and buffering there is how a large upload fills the root pool.
        #
        # NOTE: this host is fronted by the cloudflare tunnel
        # (../network/cloudflare.nix), which caps a PROXIED request body at
        # 100MB -- the same limit ../hosts/secondpc/default.nix excludes
        # cache.ivymect.in from the tunnel over. Uploads larger than that will
        # fail from outside the LAN until immich.<domain> is excluded too and
        # given a direct record.
        services.nginx.virtualHosts.${config.gateway.services.immich.domain}.extraConfig = ''
          client_max_body_size 50g;
          proxy_request_buffering off;
          proxy_read_timeout 600s;
          proxy_send_timeout 600s;
          send_timeout 600s;
        '';
      };
  };
}
