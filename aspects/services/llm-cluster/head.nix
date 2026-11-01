{
  den,
  lib,
  caps,
  ...
}:
# The head of an `llm-cluster` (./cluster.nix): one llama-server in router mode
# serving every model the cluster declares, each split across every other
# member's rpc-server. Its own GPU is used directly rather than through an
# rpc-server on the same box, which would count it twice.
let
  inherit (lib) mkOption types;
  llm = den.lib.llm;

  # Every worker this head resolves, strongest backend first. `llm-node` is
  # `reach = "llm-cluster"`, so these are this cluster's members only.
  workersFor =
    host:
    lib.sort (a: b: llm.rank a.link.backend < llm.rank b.link.backend) (
      lib.filter (d: !d.sameHost) (den.lib.capability.dialAll "llm-rpc" { every = true; } host)
    );

  rpcList = ws: lib.concatMapStringsSep "," (d: "${d.address}:${toString d.ports.rpc.port}") ws;

  # One section per model, keyed by the id served at /v1/models; `[*]` is what
  # every model shares. Keys are llama-server's flags without the dashes.
  presetFor =
    cluster: workers:
    lib.generators.toINI { } (
      {
        "*" = {
          n-gpu-layers = 99;
          jinja = true;
        }
        // lib.optionalAttrs (workers != [ ]) { rpc = rpcList workers; };
      }
      // lib.mapAttrs (
        _: m:
        {
          hf-repo = m.hf;
          ctx-size = m.contextSize;
          load-on-startup = m.loadOnStartup;
        }
        // m.extraArgs
      ) cluster.models
    );
in
{
  den.capabilities.llm-api = {
    meta.description = "An OpenAI-compatible LLM endpoint. A narrowing of http.";
    side = "endpoint";
    includes = [ caps.http ];
    options.models = mkOption {
      type = types.listOf types.str;
      description = "Model ids served at /v1/models.";
    };
  };

  den.aspects.llm-head = {
    # The API, for any host; the workers' own reach keeps the RPC in-cluster.
    reach = "flake";

    takes.llm-rpc.every = true;

    gives =
      { self, ... }:
      let
        ports.api = {
          port = llm.ports.api;
          meta.description = "OpenAI-compatible llama-server fronting the llama.cpp RPC cluster.";
          expose = [ "tailscale" ];
        };
      in
      {
        tcp = _: {
          name = self.name;
          inherit ports;
        };
        llm-api =
          { host, ... }:
          {
            name = self.name;
            protocol = "http://";
            models = lib.attrNames host.llm-cluster.models;
            inherit ports;
          };
      };

    nixos =
      { host, pkgs, ... }:
      let
        cluster = host.llm-cluster;
        pkg = llm.packageFor pkgs host.llmBackend;
        workers = workersFor host;
        preset = pkgs.writeText "llm-cluster-${cluster.name}.ini" (presetFor cluster workers);

        bench = pkgs.writeShellApplication {
          name = "llm-bench";
          text = ''exec ${pkg}/bin/llama-bench ${
            lib.escapeShellArgs (
              lib.optionals (workers != [ ]) [
                "--rpc"
                (rpcList workers)
              ]
            )
          } "$@"'';
        };
      in
      {
        environment.systemPackages = [ bench ];

        systemd.services.llm-head = {
          description = "llama.cpp head of ${cluster.name} over ${toString (lib.length workers)} RPC workers";
          wantedBy = [ "multi-user.target" ];
          wants = [ "network-online.target" ];
          after = [ "network-online.target" ];
          environment.LLAMA_CACHE = "/var/lib/llm-head";
          serviceConfig = {
            ExecStart = "${pkg}/bin/llama-server ${
              lib.escapeShellArgs [
                "--host"
                (den.lib.capability.bindAddressFor host den.aspects.llm-head "tcp" "api")
                "--port"
                (toString (den.lib.capability.portFor host den.aspects.llm-head "tcp" "api"))
                "--models-preset"
                "${preset}"
                "--models-max"
                (toString cluster.maxLoaded)
              ]
            }";
            DynamicUser = true;
            StateDirectory = "llm-head";
            SupplementaryGroups = [
              "video"
              "render"
            ];
            Restart = "on-failure";
            RestartSec = 10;
          };
        };
      };
  };
}
