{
  den,
  lib,
  ...
}:
# Trace storage, so kanidm's OpenTelemetry export has somewhere to land and
# Grafana has something to read it from. Local filesystem backend -- this is one
# host's traces, not a fleet's.
{
  den.aspects.tempo = {
    includes = [ den.aspects.gateway ];

    # Public ingest path, gated by a per-caller key like every other service
    # here -- for an exporter that is NOT on this host's loopback. A caller on
    # loopback (kanidm) should keep using the plain grpc receiver below
    # instead: it is simpler and needs no key at all.
    #
    # No UI to protect, so `subpath` stays at its unused default and only
    # `api.subpath` matters -- pointed at OTLP/HTTP's own path (`/v1/traces`)
    # rather than the usual `/api`, so the proxied path lines up with what the
    # http receiver actually listens for.
    # `takes.gated` carries no port -- the OTLP/HTTP endpoint is given
    # above, and ../proxy-nginx.nix resolves it.
    takes.gated = { };

    # ── NGINX-SPECIFIC, SO DECLARED AS SUCH ──
    #
    # `internalKey = false` on purpose: tempo has no auth configuration at all,
    # so there is no internal key to swap TO -- the gate validates the caller's
    # key and then BLANKS the header, leaving tempo trusting loopback because
    # nginx is the only way in. That is a property of how this proxy handles a
    # keyless upstream, not of tempo.
    #
    # `subpath = "/v1"` points at OTLP/HTTP's own path rather than the usual
    # `/api`, so the proxied path lines up with what the http receiver listens
    # for.
    takes.nginx.api = {
      enable = true;
      subpath = "/v1";
      internalKey = false;
    };

    # ── WHAT TEMPO GIVES: AN OTLP/GRPC RECEIVER ──
    #
    # Four exporters currently hardcode `127.0.0.1:4317` -- ../base/caches.nix,
    # ../base/celler/server.nix, ../hosts/secondpc/otel-fail2ban.nix and the
    # kanidm leg -- each spelling the scheme differently (`insecure://` twice, a
    # bare `host:port` once) because nothing told them which form this receiver
    # accepts.
    #
    # As a capability the address and the port come from the resolution and the
    # SCHEME comes from the giver, so an exporter writes
    # `"${grpc.protocol}${grpc.address}:${toString (head grpc.ports)}"` and is
    # right over loopback today and over any carrier if tempo ever moves.
    #
    # `insecure://`, because that is the form the two URL-taking consumers need
    # and the bare-address consumer can strip; the receiver below serves plain
    # h2c either way.
    gives =
      { self, ... }:
      {
        # WHAT IT LISTENS ON, so ../proxy-nginx.nix can ASK rather than
        # assume: the upstream is resolved by composing this with the carrier
        # the two ends share. Not exposed -- the gate is the way in.
        tcp = _: {
          name = self.name;
          ports.http = {
            port = 4318;
            meta.description = "OTLP over HTTP, which nginx proxies at /v1/traces for an exporter that is not on this box's loopback.";
          };

          # ── DECLARED SO THE EXCLUSION CHECK CAN SEE IT ──
          # 3200 was set in the nixos body and declared to nobody, so
          # `den.checks.socket-exclusions` could not catch a second claim on
          # it -- the gap ../services/kanidm.nix's `gives.tcp` warns about.
          # No `expose`: Grafana queries it from this box, and it is not
          # gated, so nginx must not reach it either.
          ports.query = {
            port = 3200;
            meta.description = "Tempo's own query API, which Grafana reads traces from. Loopback only and deliberately ungated.";
          };
        };

        grpc = _: {
          port = 4317;
          ports.grpc.meta.description = "OTLP over gRPC, plain h2c: the receiver every exporter on this box ships spans to.";
          protocol = "insecure://";
          carrier = self.name;
        };

        # The account the unit runs as, for anything naming things after it.
        user = _: {
          name = "tempo";
          carrier = self.name;
        };
      };

    # THIS ASPECT'S OWN FURNISHED LINK, read back by name. So the receiver is
    # configured from the same value a consumer resolves, and the port is
    # written exactly once -- in `gives.grpc` above.
    #
    # `den.lib.capability.ownLinkFor host`, not a `{ grpc, ... }` module argument: den
    # resolves an aspect's arguments through its own binder before any aspect's
    # `_module.args` has merged, so the argument form fell through to
    # `config._module.args.grpc` and failed with `attribute 'grpc' missing`.
    nixos =
      {
        host ? { },
        ...
      }:
      let
        grpc = den.lib.capability.ownLinkFor host "grpc";
      in
      {
        services.tempo = {
          enable = true;
          settings = {
            server = {
              # BOTH READ OFF `gives.tcp.ports.query` ABOVE, so the number and
              # the bind live once. `bindAddressFor` resolves that port's own
              # exposure -- nothing exposes it and every consumer is on this
              # box, so it answers the loopback, which is the same
              # `127.0.0.1` this used to state.
              http_listen_address = den.lib.capability.bindAddressFor host den.aspects.tempo "tcp" "query";
              http_listen_port = den.lib.capability.portFor host den.aspects.tempo "tcp" "query";
              # 9095 is loki's; the internal modules dial this address in
              # single-binary mode, so it has to be reachable on loopback.
              grpc_listen_address = "127.0.0.1";
              grpc_listen_port = 9096;
            };

            distributor.receivers.otlp.protocols = {
              # What kanidm, celler and the fail2ban exporter send to -- no
              # gateway, no key, loopback trust like the query API above.
              #
              # FROM the bound link, which IS `gives.grpc` above -- one
              # declaration, two readers, so the address an exporter is handed and
              # the address this binds cannot disagree.
              grpc.endpoint = grpc.bind;
              # What the gateway above proxies remote/other-host exporters
              # to. Also loopback: reaching it at all still requires going
              # through nginx and presenting a key for one of tempo's
              # `gateway.services.tempo.api.clients`.
              http.endpoint = "127.0.0.1:4318";
            };

            storage.trace = {
              backend = "local";
              local.path = "/var/lib/tempo/traces";
              wal.path = "/var/lib/tempo/wal";
            };

            # Tempo 3 dropped the ingester for the live-store, whose state
            # defaults to /var/tempo -- outside the unit's StateDirectory.
            live_store = {
              wal.path = "/var/lib/tempo/live-store/wal";
              shutdown_marker_dir = "/var/lib/tempo/live-store/shutdown-marker";
            };

            # And dropped the compactor for the scheduler/worker pair, which is
            # where block retention now lives. Traces age out on their own;
            # without this they accumulate until the disk says otherwise.
            backend_scheduler = {
              local_work_path = "/var/lib/tempo/scheduler";
              provider.compaction.compaction.block_retention = "168h";
            };
            backend_worker.compaction.block_retention = "168h";

            # Derives span-metrics and a service graph from every trace and
            # remote-writes them to the same prometheus grafana already reads
            # -- `--web.enable-remote-write-receiver` is already on for this
            # (see observability.nix). This is what actually feeds grafana's
            # Tempo datasource "trace to metrics" / service-graph panel;
            # without it those panels have nowhere to query.
            metrics_generator = {
              registry.external_labels.source = "tempo";
              storage = {
                path = "/var/lib/tempo/generator/wal";
                remote_write = [
                  {
                    url = "http://127.0.0.1:9091/api/v1/write";
                    send_exemplars = true;
                  }
                ];
              };
            };
            overrides.defaults.metrics_generator.processors = [
              "service-graphs"
              "span-metrics"
            ];
          };
        };
      };
  };
}
