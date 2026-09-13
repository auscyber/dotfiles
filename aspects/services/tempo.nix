{ den, ... }:
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
    gated.tempo = {
      upstream = "http://127.0.0.1:4318";
      api = {
        enable = true;
        subpath = "/v1";
        # No declarative key surface on tempo's side either (no auth config
        # at all, in fact) -- same shape as qbittorrent: the gateway checks
        # the caller's key and then blanks the header, and tempo is left
        # trusting loopback because nginx is the only way in.
        internalKey = false;
      };
    };

    nixos =
      { ... }:
      {
        services.tempo = {
          enable = true;
          settings = {
            server = {
              # 3200 is tempo's own API, which Grafana queries. Loopback only:
              # it is not gated, so nginx must not be able to reach it either.
              http_listen_address = "127.0.0.1";
              http_listen_port = 3200;
              # 9095 is loki's; the internal modules dial this address in
              # single-binary mode, so it has to be reachable on loopback.
              grpc_listen_address = "127.0.0.1";
              grpc_listen_port = 9096;
            };

            distributor.receivers.otlp.protocols = {
              # What kanidm, on the same host, exports to directly -- no
              # gateway, no key, loopback trust like the query API above.
              grpc.endpoint = "127.0.0.1:4317";
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
