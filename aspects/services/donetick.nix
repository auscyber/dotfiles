{
  den,
  lib,
  accessLib,
  ...
}:
# Donetick (chores/tasks). Like ./vikunja.nix it is a real OIDC client, so
# `oauthProxy = false` keeps oauth2-proxy off the browser leg.
#
# Donetick wants the OAuth2 endpoints spelled out individually rather than a
# discovery URL, so the four kanidm paths are written literally below. They are
# kanidm's, not a convention: authorize is `/ui/oauth2` (not under /oauth2/),
# token is shared across clients, and userinfo and the JWKS are per client.
{
  den.aspects.donetick = {
    includes = [
      den.aspects.gateway
      den.aspects.postgres
      den.aspects.agenix-rekey
      den.aspects.packages.donetick
    ];

    # What access this service exposes, for ../persons/<name>.nix to be granted.
    # Collected by ./access.nix, which turns a grant into a kanidm group and
    # adds that group to this service's own `gated` groups.

    takes.gated = {
      # A real OIDC client, like ./vikunja.nix -- see the note there.
      oauthProxy = false;
      # Server-sent events for its realtime updates, which nginx must not buffer.
      websockets = true;
    };

    # TCP rather than the socket: donetick's postgres config is host/port/user/
    # password with no socket path, so it needs a role password.
    #
    # `tcp = true` IS THE WHOLE REQUEST. The password is then the EDGE's
    # credential (../base/capability-types.nix makes `postgres` a narrowing of
    # `credential`), minted into THIS aspect's own scope and materialised once
    # per side -- the taker's copy under the plain name its template reads, the
    # giver's suffixed. So this file declares no `db-password` and
    # ./postgres.nix is told no path; each side reads
    # `postgres.side.pathIn scoped`.
    takes.postgres = {
      tcp = true;
      services = [ "donetick" ];
    };

    # THE ACCOUNT THIS RUNS AS, which is what the credential's taker-side copy
    # is owned by and what the database and role are named after. The `nixos`
    # body below creates it; this only states it, so nothing else has to name
    # it twice.
    gives = { self, ... }: {
      # WHAT A GRANT HERE MEANS. Donetick maps OIDC groups to roles
      # (`admin_groups`/`manager_groups` in its own config), so these names
      # are directly usable there -- which is exactly why the service and
      # not the household is the one that can state them.
      service = _: {
        access = {
          levels = [
            "user"
            "admin"
          ];
          defaultLevel = "user";
        };
      };

      # WHAT IT LISTENS ON, so ../proxy-nginx.nix can ASK rather than
      # assume: the upstream is resolved by composing this with the carrier
      # the two ends share. Not exposed -- the gate is the way in.
      tcp = _: {
        name = self.name;
        ports.http = {
          port = 2021;
          meta.description = "Donetick's own web server, UI and API together, dialed by the gate over the loopback.";
        };
      };

      user = _: {
        name = "donetick";
        group = "donetick";
        # The unit runs as `donetick` and the database role matches it, but the
        # edge's password is consumed through an age TEMPLATE -- this file's
        # whole config is one -- and agenix renders that as root. So the name
        # and the owner come apart, which this file's own comment said before
        # any of it was modelled: "owned by neither".
        owner = "root";
        carrier = self.name;
      };
    };

    secrets = {
      oidc-client-secret = {
        owner = "kanidm";
        restartUnits = [ "kanidm.service" ];
        generator.script = "random-hex-32";
      };

      # NO `db-password` HERE. It is the postgres edge's credential now, and
      # ../services/postgres.nix installs BOTH materialisations into THIS scope
      # -- `db-password` owned by donetick, and `db-password-postgres`
      # inheriting its `rekeyFile` for the role-password unit. The template
      # below still reads `placeholders.db-password`, which is why the taker's
      # copy keeps the plain name.

      # Signs donetick's own session tokens. Upstream asks for "32 characters
      # long"; hex of 32 bytes is 64 and satisfies it with room to spare.
      jwt-secret = {
        owner = "donetick";
        restartUnits = [ "donetick.service" ];
        generator.script = "random-hex-32";
      };
    };

    # Donetick reads ONE yaml file and has no env override for every key, so the
    # whole config is a template -- which is also what keeps the jwt secret, the
    # db password and the client secret out of the store.
    # ── THE ZONE, AND WHY NOT `config.gateway.domain` ──
    #
    # Neither `content` nor the body around it can reach the host's config.
    # agenix calls `content` with exactly `{ pkgs; lib; placeholders; }`
    # (agenix `modules/age.nix`), so a `config` argument there is `function
    # 'content' called without required argument 'config'`. And a per-ENTRY
    # `templates.<n>` body is a module in the ENTRY submodule, whose
    # `_module.args` is `{ secrets; templates; scoped; }` (../../lib/age-scoped.nix)
    # -- so `config` there is the template's own and `config.gateway.domain` is
    # `attribute 'gateway' missing`.
    #
    # So it is read from the aspect that DECLARES the zone, which is the same
    # single source `gateway.domain` is set from (../network/cloudflare.nix's
    # `publish-ivymect-in`) and therefore cannot drift from it. The limitation
    # this accepts: it names that zone rather than following whichever zone the
    # serving host publishes under. True for this service today, and a host
    # publishing a different zone would need it derived instead.
    templates."selfhosted.yaml" =
      { secrets, ... }:
      {
        owner = "donetick";
        restartUnits = [ "donetick.service" ];
        dependencies = {
          inherit (secrets) oidc-client-secret db-password jwt-secret;
        };
        content =
          { placeholders, ... }:
          let
            domain = den.aspects.publish-ivymect-in.domain;
            auth = "https://auth.${domain}";
          in
          ''
            name: "selfhosted"
            is_done_tick_dot_com: false
            # kanidm owns identity: nobody signs up here, and the password form
            # is gone rather than merely unused.
            is_user_creation_disabled: true
            disable_password_auth: true
            database:
              type: "postgres"
              migration: true
              host: "127.0.0.1"
              port: 5432
              user: "donetick"
              password: "${placeholders.db-password}"
              name: "donetick"
            jwt:
              secret: "${placeholders.jwt-secret}"
              session_time: 168h
              max_refresh: 1440h
            server:
              port: 2021
              read_timeout: 10s
              write_timeout: 10s
              rate_period: 60s
              rate_limit: 300
              serve_frontend: true
              serve_swagger: false
              public_host: "https://donetick.${domain}"
              cors_allow_origins:
                - "https://donetick.${domain}"
            logging:
              level: "info"
              encoding: "json"
              development: false
            scheduler_jobs:
              due_job: 30m
              overdue_job: 3h
              pre_due_job: 3h
            realtime:
              enabled: true
              sse_enabled: true
              heartbeat_interval: 60s
              connection_timeout: 120s
            oauth2:
              name: "kanidm"
              client_id: "donetick"
              client_secret: "${placeholders.oidc-client-secret}"
              auth_url: "${auth}/ui/oauth2"
              token_url: "${auth}/oauth2/token"
              user_info_url: "${auth}/oauth2/openid/donetick/userinfo"
              redirect_url: "https://donetick.${domain}/api/v1/auth/oauth2/callback"
          '';
      };

    # `kanidmClients.<name>`: the shared OIDC shape (short username, scope
    # maps, landing page, the domain) is defaulted by `gateway.oidcClients` in
    # ./gateway.nix, so this is only what differs. The name is written once
    # here; keying it off the aspect is not possible (see that option's note),
    # and keying it off the service entity waits on `{ service, ... }` binding.
    kanidmClients.donetick = {
      displayName = "Donetick";
      callback = "/api/v1/auth/oauth2/callback";
      # No `groupsName` yet: donetick CAN map OIDC groups to roles
      # (`admin_groups`/`manager_groups`), which is what the
      # `app-donetick-admin` group from ./access.nix is for -- but its config
      # does not reference them today, so the scope would be unused.
      # ── AND WHO MAY HAVE A TOKEN AT ALL, WHICH IS THE GRANT ──
      #
      # `scopes` REPLACES the `media-users` default (./gateway.nix), so this
      # client issues a token only to the groups a grant on donetick produces.
      # That is the narrowing the comment above already claimed: "the scope map
      # is itself the gate" was true of a scope EVERY client shared, so being
      # in `media-users` bought all of them at once and a grant on one bought
      # nothing.
      #
      # ./access.nix owns the spelling and answers with the GRANTED levels
      # rather than the declared ones -- a scope map naming a group nobody was
      # granted is a group kanidm never provisioned, and the provisioning run
      # fails on it.
      scopes = accessLib.groupsFor "donetick";
    };

    nixos =
      {
        config,
        pkgs,
        lib,
        scoped,
        ...
      }:
      {

        # No nixpkgs module for donetick, so the unit is ours.
        users.users.donetick = {
          isSystemUser = true;
          group = "donetick";
          home = "/var/lib/donetick";
        };
        users.groups.donetick = { };

        systemd.services.donetick = {
          description = "Donetick";
          # Ordering against postgres (and against the role password being set)
          # comes from `takes.postgres.services` above.
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];

          # Donetick resolves its config as `./config/$DT_ENV.yaml` relative to
          # the working directory, so the state dir gets a `config/` holding a
          # symlink to the rendered template. A symlink rather than a copy so
          # rotating the secret does not need this unit to re-render anything.
          environment.DT_ENV = "selfhosted";

          preStart = ''
            mkdir -p /var/lib/donetick/config
            ln -sf ${scoped.donetick.templates."selfhosted.yaml".path} \
                   /var/lib/donetick/config/selfhosted.yaml
          '';

          serviceConfig = {
            Type = "simple";
            User = "donetick";
            Group = "donetick";
            StateDirectory = "donetick";
            WorkingDirectory = "/var/lib/donetick";
            # `mainProgram` is `core` -- the go module is `donetick.com/core`,
            # so the binary takes the last path element, not the pname.
            ExecStart = lib.getExe' pkgs.donetick "core";
            Restart = "on-failure";
            RestartSec = "5s";

            NoNewPrivileges = true;
            PrivateTmp = true;
            ProtectSystem = "strict";
            ProtectHome = true;
            ReadWritePaths = [ "/var/lib/donetick" ];
          };
        };
      };
  };
}
