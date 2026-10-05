{
  den,
  lib,
  accessLib,
  ...
}:
# Vikunja (tasks), authenticating against kanidm itself rather than through
# oauth2-proxy -- hence `oauthProxy = false` on the gated entry. The distinction
# matters: oauth2-proxy would hand Vikunja one trusted identity for everybody,
# and per-user task ownership and sharing would stop meaning anything. A real
# OIDC client gets real accounts.
#
# The redirect URL is `<publicurl>/auth/openid/<provider key>` -- the frontend
# builds it from the provider's KEY, not its display name -- so the key
# `kanidm` below and the `originUrl` in the provision block have to agree
# exactly or kanidm rejects the callback.
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
  port = 3456;

in
{
  den.aspects.vikunja = {
    includes = [
      den.aspects.gateway
      den.aspects.postgres
      den.aspects.agenix-rekey
      # ../restic.nix's `backup` quirk -- see the `backup` entry below.
      den.aspects.restic
    ];

    # What access this service exposes, for ../persons/<name>.nix to be granted.
    # Collected by ./access.nix, which turns a grant into a kanidm group and
    # adds that group to this service's own `gated` groups.

    # A PORT, NOT AN UPSTREAM. The proxy and this service are on one host by
    # construction, so the scheme and the loopback address are the proxy's to
    # supply -- `takes.gated` says what it needs and
    # ../services/proxy-nginx.nix supplies the rest.
    #
    # `oauthProxy = false` because vikunja is a real OIDC client (the `kanidmClient`
    # below): putting oauth2-proxy in front would bounce the browser through two
    # providers for one login. It still TAKES `gated`, because it needs the
    # groups collected and the client provisioned -- what it does not need is
    # the forward auth.
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
        # ── WHAT A GRANT HERE MEANS, given rather than restated ──
        #
        # Was in ../households/ivymectin.nix, which is the house describing an
        # app's permission model -- a fact only the app has. Vikunja has its
        # own per-user permissions, so `admin` here means only that the kanidm
        # group exists for an aspect to key off, not a Vikunja role.
        service = _: {
          access = {
            levels = [
              "user"
              "admin"
            ];
            defaultLevel = "user";
          };
        };

        tcp = _: {
          name = self.name;
          ports.http = {
            inherit port;
            meta.description = "Vikunja's API and the web frontend it serves beside it, dialed by the gate over the loopback.";
          };
        };

        # ── THE DASHBOARD TILE, AND THE ACCESS IT INHERITS ──
        #
        # The tile is gated by the SAME declaration as the service: the
        # `access` above is what ./access.nix turns into `app-vikunja-<level>`,
        # and ./homepage.nix reads the household's grants on this service to
        # decide whether a shared dashboard may show it. Nothing restates who
        # may see it.
        #
        # No widget: homepage has no vikunja widget type, so this is a link.
        homepage-widget = _: {
          group = "Productivity";
          icon = "vikunja.svg";
          description = "Tasks";
        };
      };

    takes.gated.oauthProxy = false;

    # Peer auth over the unix socket: Vikunja runs as `DynamicUser`, which still
    # means a unix user literally named `vikunja`, so it maps to the role of the
    # same name with no password anywhere.
    takes.postgres.services = [ "vikunja" ];

    # Tasks are in postgres and therefore already covered by ./restic.nix's dump
    # of every provisioned database. ATTACHMENTS are not: vikunja writes the
    # file to `files.basepath` and keeps only a row pointing at it, so a
    # database-only restore gives you every task with every attachment broken.
    #
    # `/var/lib/private/vikunja`, not `/var/lib/vikunja`: the unit runs with
    # `DynamicUser`, so the latter is a symlink into the former and restic does
    # not follow symlinks. The whole state directory rather than just `files/`
    # -- the rest of it is a few kilobytes.
    backup = _: {
      name = "vikunja";
      paths = [ "/var/lib/private/vikunja" ];
    };

    # kanidm reads this directly as `basicSecretFile`, so it is owned by kanidm
    # and restarts kanidm -- not vikunja. The scope's inferred defaults (which
    # would be `vikunja.service`) are overridden per entry.
    secrets.oidc-client-secret = {
      owner = "kanidm";
      restartUnits = [ "kanidm.service" ];
      generator.script = "random-hex-32";
    };

    # The same value reaching Vikunja, so the two cannot drift. Deliberately NOT
    # chowned to `vikunja`: a DynamicUser account does not exist when agenix
    # chowns, and systemd reads `environmentFiles` as root before dropping
    # privileges anyway. The scope infers `restartUnits` from its own name.
    templates.env = { secrets, ... }: {
      dependencies = { inherit (secrets) oidc-client-secret; };
      content = { placeholders, ... }: ''
        VIKUNJA_AUTH_OPENID_PROVIDERS_KANIDM_CLIENTSECRET=${placeholders.oidc-client-secret}
      '';
    };

    # `kanidmClients.<name>`: the shared OIDC shape (short username, scope
    # maps, landing page, the domain) is defaulted by `gateway.oidcClients` in
    # ./gateway.nix, so this is only what differs. The name is written once
    # here; keying it off the aspect is not possible (see that option's note),
    # and keying it off the service entity waits on `{ service, ... }` binding.
    kanidmClients.vikunja = {
      displayName = "Vikunja";
      # The redirect URL is `<publicurl>/auth/openid/<provider key>` -- the
      # frontend builds it from the provider's KEY -- so this path and the
      # `kanidm` key in `services.vikunja.settings` must agree exactly.
      callback = "/auth/openid/kanidm";
      # No `groupsName`: Vikunja does its own authorisation, so the scope map
      # is itself the gate -- kanidm refuses a token to anyone outside the
      # groups `scopes` below names.
      # ── AND WHO MAY HAVE A TOKEN AT ALL, WHICH IS THE GRANT ──
      #
      # `scopes` REPLACES the `media-users` default (./gateway.nix), so this
      # client issues a token only to the groups a grant on vikunja produces.
      # That is the narrowing the comment above already claimed: "the scope map
      # is itself the gate" was true of a scope EVERY client shared, so being
      # in `media-users` bought all of them at once and a grant on one bought
      # nothing.
      #
      # ./access.nix owns the spelling and answers with the GRANTED levels
      # rather than the declared ones -- a scope map naming a group nobody was
      # granted is a group kanidm never provisioned, and the provisioning run
      # fails on it.
      scopes = accessLib.groupsFor "vikunja";
    };

    nixos =
      {
        config,
        scoped,
        ...
      }:
      {

        services.vikunja = {
          enable = true;
          frontendScheme = "https";
          frontendHostname = "vikunja.${config.gateway.domain}";
          # Loopback only: nginx is the one way in, the same reasoning as
          # grafana's `http_addr`.
          address = "127.0.0.1";
          port = port;

          database = {
            type = "postgres";
            # A socket path, not a host -- the module passes it straight through
            # and Vikunja's driver accepts it.
            host = "/run/postgresql";
            user = "vikunja";
            database = "vikunja";
          };

          environmentFiles = [ scoped.vikunja.templates.env.path ];

          settings = {
            auth = {
              # kanidm is the only way in. Leaving local accounts enabled would
              # keep a password form on the login page that nothing provisions.
              local.enabled = false;
              openid = {
                enabled = true;
                providers.kanidm = {
                  name = "kanidm";
                  # The ISSUER, not the authorize endpoint: Vikunja fetches the
                  # discovery document from `<authurl>/.well-known/openid-configuration`.
                  authurl = "https://auth.${config.gateway.domain}/oauth2/openid/vikunja";
                  clientid = "vikunja";
                  # clientsecret arrives from environmentFiles above.
                  scope = "openid profile email";
                };
              };
            };
          };
        };

        # Ordering against postgres comes from the `postgres` quirk's `services`.
        systemd.services.vikunja.serviceConfig.RestartSec = "5s";
      };
  };
}
