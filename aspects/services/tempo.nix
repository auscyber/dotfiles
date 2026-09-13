{ den, ... }:
# Trace storage, so kanidm's OpenTelemetry export has somewhere to land and
# Grafana has something to read it from. Local filesystem backend -- this is one
# host's traces, not a fleet's.
{
  den.aspects.tempo = {
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
            };

            # The OTLP gRPC endpoint kanidm exports to.
            distributor.receivers.otlp.protocols.grpc.endpoint = "127.0.0.1:4317";

            storage.trace = {
              backend = "local";
              local.path = "/var/lib/tempo/traces";
              wal.path = "/var/lib/tempo/wal";
            };

            # Traces age out on their own; without this they accumulate until
            # the disk says otherwise.
            compactor.compaction.block_retention = "168h";

            # Single binary: no ring to coordinate, so the ring store is local.
            ingester.lifecycler.ring = {
              kvstore.store = "inmemory";
              replication_factor = 1;
            };
          };
        };
      };
  };
}
