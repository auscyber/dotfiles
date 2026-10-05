{ den, lib, ... }:
# Audiobookshelf (audiobooks + podcasts), behind the gateway on its own vhost.
#
# `oauthProxy = false`: Audiobookshelf has real per-user accounts and per-user
# listening progress, which is the entire point of it. oauth2-proxy would hand
# it one trusted identity for everybody and progress would stop meaning
# anything -- the same reasoning ../services/vikunja.nix spells out. It also
# has native mobile apps, which cannot carry an oauth2-proxy cookie. So it
# authenticates against kanidm itself, as a real OIDC client.
#
# The kanidm half IS declarative (below). The Audiobookshelf half is NOT, and
# cannot be: 2.36.1 keeps every OIDC setting in `ServerSettings` in its own
# sqlite database, written only through `PATCH /api/auth-settings` -- there is
# no environment variable and no config file for them, and the nixpkgs module
# exposes no `settings` option. So the client exists on the IdP from the first
# activation and the server side is entered once, by hand:
#
#   Settings -> Authentication -> OpenID Connect
#     Issuer URL     https://auth.ivymect.in/oauth2/openid/audiobookshelf
#                    ("Auto-populate" fills the authorize/token/userinfo/JWKS
#                     URLs from it)
#     Client ID      audiobookshelf
#     Client Secret  the contents of the secret generated below; read it with
#                      sudo cat /run/agenix/secrets/audiobookshelf/oidc-client-secret
#     Match Existing Users By   Username   (kanidm sends the short username,
#                                           see `preferShortUsername`)
#     Auto-register  on, so a kanidm account in `media-users` exists here on
#                    first login rather than needing to be pre-created
#
# --- Music Assistant ---
#
# Music Assistant 2.10.3 ships an `audiobookshelf` music provider
# (aioaudiobookshelf). The dependency half is declarative: the provider is not
# one of the package's builtins, so it has to be named in
# `services.music-assistant.providers` or the python import fails at runtime.
# That list is added to below rather than in ../services/homeassistant/default.nix
# where the rest of the provider list lives -- it is a NixOS list option, so it
# merges, and the dependency belongs next to the service it is a dependency ON.
#
# The provider INSTANCE is not declarative. Music Assistant stores provider
# configuration in /var/lib/music-assistant, written only through its websocket
# API (`ConfigController.save_provider_config`); there is no config file to
# render and the nixpkgs module exposes no settings. Add it once, by hand:
#
#   Music Assistant -> Settings -> Music Providers -> + -> Audiobookshelf
#     Server URL   http://127.0.0.1:8000
#     API Token    an Audiobookshelf API key (Settings -> Users -> pick the
#                  account -> API Keys), or leave blank and give it the
#                  username/password of a dedicated Audiobookshelf user
#     Verify SSL   off
#
# The URL is the LOOPBACK upstream, not `gated.audiobookshelf.url`. Both
# services run on this host, so the public name would resolve out through the
# cloudflare tunnel and back in for every request. This is the `localUrl`
# escape hatch ../services/gateway.nix documents.
#
# Nothing more is needed for the reverse direction: the Home Assistant
# `music_assistant` component already enabled in
# ../services/homeassistant/default.nix brings python3Packages.music-assistant-client
# (github.com/music-assistant/client) into home-assistant's python environment.
# That repository is a client LIBRARY with no console scripts and no player of
# its own, so there is nothing to package and no unit to run.
let
  # ── THE PORT, ONCE ──
  #
  # Read by two things that must agree: `gives.tcp` below, which is what
  # ../proxy-nginx.nix resolves the upstream from and what makes this
  # service's public address derivable, and the service's own NixOS option,
  # which is what actually makes it listen there. They were the same number
  # written twice.
  #
  # A `let` and not `capability.ownLinkFor`: that binds a capability by name
  # across the whole host and throws when two aspects give one ("there is no
  # one value to bind... declare the port in each aspect's own content
  # instead"), which on this box is every service that listens on tcp.
  port = 8000;

in
{
  den.aspects.audiobookshelf = {
    includes = [
      den.aspects.gateway
      den.aspects.agenix-rekey
      # ../restic.nix's `backup` quirk -- see the `backup` entry below.
      den.aspects.restic
    ];

    # A HOUSE AMENITY, which is the fact ../households/ivymectin.nix used to
    # restate beside this aspect: a take from another machine in the same
    # household may resolve to this one, and nothing outside the house can.
    # Stated here because the question -- may my audiobooks be served to
    # another box -- is a fact about the server, not about who is asking.
    reach = "household";

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
        # A SERVICE THE HOUSE OFFERS. No `access`: audiobookshelf does its own
        # authentication and its role mapping is not declarative at all, so a
        # level here would name nothing.
        service = _: {
          # NO `access`, and the grant that would carry one is honoured
          # elsewhere: audiobookshelf keeps its own accounts, and its OIDC
          # mapping onto them is set in its web UI rather than in its config.
          # So this says "not from nix" instead of leaving ./access.nix's
          # `access-grants-land` check to guess, which is the same statement
          # ../hosts/secondpc/plex.nix makes for the same reason.
          externallyManaged = true;
        };

        tcp = _: {
          name = self.name;
          ports.http = {
            inherit port;
            meta.description = "Audiobookshelf's web app and API, socket.io player progress included; the gate is the only way in.";
          };
        };
      };

    takes.gated = {
      # Audiobookshelf does its own authentication -- see the header.
      oauthProxy = false;
      # Progress, library scan status and the audio player all stream over a
      # socket.io connection.
      websockets = true;
    };

    # kanidm reads this directly as `basicSecretFile`, so it is owned by kanidm
    # and restarts kanidm -- not audiobookshelf. Audiobookshelf never reads the
    # file at all: the same value is pasted into its UI once (header), because
    # it has nowhere to read a secret from.
    #
    # `owner` and `restartUnits` are both spelled out because the scope's
    # inference is DISABLED for this scope -- see `age.scoped.audiobookshelf.service`
    # in the nixos block below, which is load bearing.
    secrets.oidc-client-secret = {
      owner = "kanidm";
      restartUnits = [ "kanidm.service" ];
      generator.script = "random-hex-32";
    };

    # The whole point of this service, for ../restic.nix: per-user accounts and
    # per-user listening progress (header), plus the OIDC settings the header
    # says have to be entered by hand -- all of it in one sqlite database under
    # `config/`.
    #
    # `metadata/` is deliberately absent. Cover art, cached stream segments and
    # logs: every byte re-derivable from the audio files on /mnt/hdd, and far
    # larger than the part that matters.
    #
    # The database goes through `sqlite`, not `paths`: audiobookshelf holds it
    # open in WAL mode, so a file-level copy is a copy of a torn page cache.
    # ../restic.nix runs `VACUUM INTO` and snapshots the result instead, and the
    # live files are excluded so a restore cannot pick the wrong one.
    backup =
      { config, ... }:
      let
        dir = "/var/lib/${config.services.audiobookshelf.dataDir}";
      in
      {
        name = "audiobookshelf";
        sqlite = [ "${dir}/config/absdatabase.sqlite" ];
        paths = [ "${dir}/config" ];
        exclude = [
          "${dir}/config/absdatabase.sqlite"
          "${dir}/config/absdatabase.sqlite-wal"
          "${dir}/config/absdatabase.sqlite-shm"
        ];
      };

    # `kanidmClients.<name>`: the shared OIDC shape (short username, scope
    # maps, landing page, the domain) is defaulted by `gateway.oidcClients` in
    # ./gateway.nix, so this is only what differs. The name is written once
    # here; keying it off the aspect is not possible (see that option's note),
    # and keying it off the service entity waits on `{ service, ... }` binding.
    kanidmClients.audiobookshelf = {
      displayName = "Audiobookshelf";
      callback = "/auth/openid/callback";
      # No `groupsName`: Audiobookshelf does its own authorisation, so the
      # scope map is itself the gate.
    };

    nixos =
      {
        config,
        scoped,
        ...
      }:
      {

        # Turns OFF the `age.scoped` backend's owner/group/restartUnits
        # inference for this scope.
        #
        # NO LONGER LOAD-BEARING. It was: the scope is named after the aspect,
        # and `audiobookshelf` also resolves as `services.audiobookshelf` --
        # which, unlike `services.grafana` or `services.vikunja`, DOES declare a
        # `user`. The old `ownership` then evaluated
        # `config.users.users ? "audiobookshelf"`, forcing the attribute names of
        # `users.users`, whose definitions include ../security/user-pwd.nix's
        # `hashedPasswordFile = <an age secret>.path` -- so `users.users` was
        # defined in terms of `age.secrets`, which was defined in terms of that
        # inference. Infinite recursion, taking the whole host's
        # `system.build.toplevel` with it rather than just this aspect.
        #
        # lib/age-scoped.nix now reads `services.<s>.group` instead, so the cycle
        # is gone at the source and this line is not needed to avoid it
        # (verified: the host evaluates with this removed).
        #
        # Kept anyway, because it is still CORRECT here rather than merely
        # harmless: the one entry this scope has belongs to kanidm, not to
        # audiobookshelf, and spells out its own `owner` and `restartUnits`.
        # Inferring audiobookshelf's would be wrong.
        age.scoped.audiobookshelf.service = null;

        services.audiobookshelf = {
          enable = true;
          # Loopback only: nginx is the one way in, the same reasoning as
          # ./grafana.nix's `http_addr`.
          host = "127.0.0.1";
          port = port;
        };

        # No `media` supplementary group on `users.users.audiobookshelf`: both
        # library roots below are OWNED by the service user at mode 0770, so it
        # needs no group membership to read or write them. Add one only if
        # Audiobookshelf is ever pointed at a directory it does not own, such as
        # /mnt/hdd/Music.

        # Both library roots. /mnt/hdd/AudioBooks is also created by
        # ../hosts/secondpc/media.nix; the same tmpfiles entry from two places
        # is idempotent, and declaring it here keeps the aspect self-contained
        # for any other host. The ownership matches what media.nix already uses.
        systemd.tmpfiles.settings.audiobookshelf = {
          "/mnt/hdd/AudioBooks"."d" = {
            user = "audiobookshelf";
            group = "media";
            mode = "0770";
          };
          "/mnt/hdd/Podcasts"."d" = {
            user = "audiobookshelf";
            group = "media";
            mode = "0770";
          };
        };

        # aioaudiobookshelf, for Music Assistant's `audiobookshelf` provider.
        # Only meaningful where Music Assistant actually runs, hence the guard
        # rather than an unconditional entry.
        services.music-assistant.providers = lib.mkIf config.services.music-assistant.enable [
          "audiobookshelf"
        ];
      };
  };
}
