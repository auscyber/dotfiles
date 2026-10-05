{ den, ... }:
# Grafana over the gateway's loki + prometheus. Like the servarr apps it does
# not authenticate anyone itself: oauth2-proxy has already done that by the time
# a request arrives, and nginx forwards the identity it established, so grafana
# trusts the header and shows no login form. That makes `http_addr` load
# bearing -- on loopback only, nginx is the one way in.
let
  # ONE NAME. The quirk declaration below creates the database and the role;
  # the server settings have to address the same thing, and restating the
  # string is how the two drift. Every other consumer in this tree writes it
  # twice -- see ./donetick.nix and ./vikunja.nix -- which is a duplication the
  # SERVICE EDGE removes rather than something to copy: under `uses.postgres`
  # the app reads the name off the resolved link instead of being told it.
  dbName = "grafana";

  # THE PORT, ONCE: read by `gives.tcp` below -- which is what
  # ../proxy-nginx.nix resolves the upstream from -- and by the service's own
  # NixOS option, which is what actually makes it listen there. They were the
  # same number written twice.
  #
  # 3001 AND NOT GRAFANA'S CONVENTIONAL 3000, because splitpro is already
  # there: its container runs with `--network=host` and serves on
  # 127.0.0.1:3000 (./splitpro.nix), so both were binding one socket on this
  # box and nginx was proxying two vhosts to the same address. Whichever lost
  # the race was simply not running.
  #
  # Nothing had noticed because neither service declared its port where
  # anything could compare them; declaring them is what made
  # `derived-port-exclusion` able to see it, and the assertion named both.
  #
  # GRAFANA MOVED RATHER THAN SPLITPRO: this is one option, where splitpro's
  # port is baked into a container image's environment. And it is a one-line
  # change only because ../proxy-nginx.nix resolves the upstream from this
  # give now instead of restating the number.
  port = 3001;
in
{
  den.aspects.grafana = {
    includes = [ den.aspects.gateway ];

    # ── THE DATABASE IS DECLARED THROUGH THE SERVICES MECHANISM ──
    #
    # Setting `services.grafana.settings.database` alone would have pointed
    # Grafana at a database NOBODY CREATES: ./postgres.nix's collector is what
    # emits `ensureDatabases`/`ensureUsers`, so a consumer that configures a
    # connection without declaring the need gets a server that starts, connects,
    # and fails.
    #
    # No `tcp`, so this is socket peer auth: the unix user `grafana` owns the
    # role of the same name and no password exists anywhere -- the arrangement
    # ./nextcloud.nix documents. `services` is what makes the unit wait for
    # `postgresql-setup`.
    # NO `database` OR `owner`. Both were `dbName`, which is the unix account
    # this runs as -- and that account is now furnished by `gives.user` below,
    # so ../services/postgres.nix derives the database and the role from it.
    # The role MUST equal the unix user for peer auth to work, which is exactly
    # why deriving both from one place is better than two literals.
    takes.postgres.services = [ "grafana" ];

    # NO `oauthProxy = false`: grafana has no OIDC client of its own here, so the
    # forward auth IS its login -- which is the default and so goes unsaid.
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
        tcp = _: {
          name = self.name;
          ports.http = {
            inherit port;
            meta.description = "Grafana's web UI and HTTP API; the gate's forward auth is the login, so nginx is the only dialer.";
          };
        };
      };

    takes.gated = {
      websockets = true; # live tail / streaming panels
    };

    # Grafana signs its session cookies with this; nixpkgs removed the default
    # precisely so nobody ships the well-known one.
    # Owner spelled out: `services.grafana` has no top-level `user` option (the
    # unit hardcodes `User = "grafana"`), so scope inference finds nothing and
    # the secret would deploy root-owned and unreadable by the service.
    secrets.secret-key.owner = "grafana";
    secrets.secret-key.generator.script = "random-hex-32";

    nixos =
      {
        config,
        lib,
        scoped,
        ...
      }:
      let
        inherit (config.services.prometheus) port;
        url = "https://grafana.${config.gateway.domain}";
      in
      {
        options.grafana.tempoCorrelations = lib.mkOption {
          type = lib.types.listOf lib.types.attrs;
          default = [ ];
          description = ''
            Extra entries for the Tempo datasource's `jsonData.correlations`.
            An extension point rather than something to fill in here: a
            correlation is specific to whatever it links tempo TO (e.g.
            kanidm's opid-matched link lives in sso.nix, next to the kanidm
            config it depends on, not here).
          '';
        };

        config.services.grafana = {
          enable = true;
          settings = {
            # ── POSTGRES, NOT THE DEFAULT SQLITE3 ──
            #
            # Grafana defaults to a sqlite3 file under its state dir, which is
            # what this was on by saying nothing. Over the UNIX SOCKET with peer
            # auth, so there is no password anywhere: the unix user `grafana`
            # owns the role of the same name, which is the same arrangement
            # ./nextcloud.nix uses and the reason its own comment says "no
            # password exists anywhere".
            #
            # `host` is the socket DIRECTORY -- libpq treats a `host` beginning
            # with `/` as a unix socket path, and nixpkgs puts the socket in
            # /run/postgresql. Grafana has no separate socket option.
            database = {
              type = "postgres";
              # The socket DIRECTORY: libpq reads a `host` beginning with `/`
              # as a unix socket path, and nixpkgs puts the socket in
              # /run/postgresql. Grafana has no separate socket option.
              host = "/run/postgresql";
              name = dbName;
              user = dbName;
            };

            server = {
              http_addr = "127.0.0.1";
              http_port = port;
              domain = "grafana.${config.gateway.domain}";
              root_url = url;
            };

            # `X-Email` is set by the nginx locations the gateway writes, from
            # what oauth2-proxy resolved. `auto_sign_up` means a kanidm account
            # that gets through the gate exists in grafana on first sight.
            "auth.proxy" = {
              enabled = true;
              header_name = "X-Email";
              header_property = "email";
              auto_sign_up = true;
            };
            auth = {
              disable_login_form = true;
              disable_signout_menu = true;
            };
            users.auto_assign_org_role = "Admin";
            analytics.reporting_enabled = false;
            # `$__file{}` is grafana's own file provider, so the value is read
            # at startup rather than baked into the store-readable config.
            security.secret_key = "$__file{${scoped.grafana.secrets.secret-key.path}}";
          };

          provision.datasources.settings.datasources = [
            {
              name = "Prometheus";
              type = "prometheus";
              uid = "prometheus";
              url = "http://127.0.0.1:${toString port}";
              isDefault = true;
            }
            {
              name = "Loki";
              type = "loki";
              uid = "loki";
              url = "http://127.0.0.1:3100";
            }
            {
              name = "Tempo";
              type = "tempo";
              uid = "tempo";
              url = "http://127.0.0.1:3200";
              # Top-level, NOT under jsonData -- confirmed against grafana's
              # own devenv/datasources.yaml example. Grafana silently
              # ignores it as an unrecognised jsonData key if nested there,
              # which is why this did nothing the first time.
              correlations = config.grafana.tempoCorrelations;
              jsonData = {
                # Jump from a span to the logs around it. Filtered on trace ID
                # rather than a derived field on the Loki side, since nothing
                # here guarantees every log line carries one.
                tracesToLogsV2 = {
                  datasourceUid = "loki";
                  spanStartTimeShift = "-5m";
                  spanEndTimeShift = "5m";
                  filterByTraceID = true;
                };
                # Jump from a span to its rate/error/duration metrics, and the
                # service graph panel on the Tempo datasource page. Both read
                # span-metrics / service-graph series that tempo's own
                # metrics-generator has to be writing to prometheus for --
                # nothing here turns that generation on by itself.
                tracesToMetrics.datasourceUid = "prometheus";
                serviceMap.datasourceUid = "prometheus";
              };
            }
          ];
        };
      };
  };
}
