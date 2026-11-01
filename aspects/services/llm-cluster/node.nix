{
  den,
  lib,
  caps,
  ...
}:
# A llama.cpp RPC worker (github.com/geerlingguy/beowulf-ai-cluster, as
# aspects). Delivered by ./cluster.nix to every member of an `llm-cluster`: the
# worker gives `llm-rpc`, its cluster's head takes it with `every = true`, and
# the firewall opens only on the carriers that head actually dials it over.
#
# rpc-server has no authentication. It is reachable over lan-home and the
# tailnet because those are the carriers a head resolves, and nowhere else.
let
  inherit (lib) mkOption types;
  llm = den.lib.llm;
in
{
  den.lib.llm = {
    ports = {
      rpc = 50052;
      api = 8090;
    };

    backends = [
      "cuda"
      "rocm"
      "vulkan"
      "metal"
      "cpu"
    ];

    # Fastest first, so a head lists its strongest workers first in `--rpc`.
    rank = b: lib.lists.findFirstIndex (x: x == b) (lib.length llm.backends) llm.backends;

    packageFor =
      pkgs: backend:
      pkgs.llama-cpp.override {
        rpcSupport = true;
        cudaSupport = backend == "cuda";
        rocmSupport = backend == "rocm";
        vulkanSupport = backend == "vulkan";
        metalSupport = backend == "metal";
      };

    # Upstream's target is `ggml-rpc-server` (tools/rpc/CMakeLists.txt).
    rpcServer = pkg: "${pkg}/bin/ggml-rpc-server";

    # Asked of the `tcp` give: only plain tcp has a loopback to fall back to.
    # `-H`/`-p` from the resolution, `-c` caches received tensors under
    # $LLAMA_CACHE/rpc so a restarted head does not resend the model.
    rpcArgs = host: [
      "-H"
      (den.lib.capability.bindAddressFor host den.aspects.llm-node "tcp" "rpc")
      "-p"
      (toString (den.lib.capability.portFor host den.aspects.llm-node "tcp" "rpc"))
      "-c"
    ];
  };

  den.schema.host =
    { config, ... }:
    {
      options.llmBackend = mkOption {
        type = types.enum llm.backends;
        default =
          if config.class or null == "darwin" then
            "metal"
          else if config.gpu == "nvidia" then
            "cuda"
          else
            "cpu";
        description = "The ggml backend this host's llama.cpp is built with.";
      };
    };

  den.capabilities.llm-rpc = {
    meta.description = "A llama.cpp RPC worker a head offloads layers to. A narrowing of tcp.";
    side = "endpoint";
    includes = [ caps.tcp ];
    options.backend = mkOption {
      type = types.enum llm.backends;
    };
  };

  den.aspects.llm-node = {
    # Only the head of this host's own cluster.
    reach = "llm-cluster";

    # CUDA builds pull these from cudaPackages; inert on every other backend.
    includes = [
      (den.batteries.unfree [
        "cuda_cccl"
        "cuda_cudart"
        "cuda_nvcc"
        "cuda_nvml_dev"
        "libcublas"
      ])
    ];

    gives =
      { self, ... }:
      let
        ports.rpc = {
          port = llm.ports.rpc;
          meta.description = "llama.cpp rpc-server: unauthenticated tensor offload, dialled by every llm-head in reach.";
        };
      in
      {
        tcp = _: {
          name = self.name;
          inherit ports;
        };
        # `self` is this aspect; `host` is the entity furnishing it.
        llm-rpc =
          { host, ... }:
          {
            name = self.name;
            backend = host.llmBackend;
            inherit ports;
          };
      };

    nixos =
      { host, pkgs, ... }:
      {
        systemd.services.llm-rpc = {
          description = "llama.cpp RPC worker (${host.llmBackend})";
          wantedBy = [ "multi-user.target" ];
          wants = [ "network-online.target" ];
          after = [ "network-online.target" ];
          environment.LLAMA_CACHE = "/var/cache/llm-rpc";
          serviceConfig = {
            ExecStart = "${llm.rpcServer (llm.packageFor pkgs host.llmBackend)} ${lib.escapeShellArgs (llm.rpcArgs host)}";
            DynamicUser = true;
            CacheDirectory = "llm-rpc";
            SupplementaryGroups = [
              "video"
              "render"
            ];
            Restart = "on-failure";
            RestartSec = 10;
          };
        };
      };

    # A user agent, not a daemon: Metal wants the login session, and the cache
    # lands in that user's ~/Library/Caches/llama.cpp without being told.
    darwin =
      {
        config,
        host,
        pkgs,
        ...
      }:
      let
        logs = "/Users/${config.system.primaryUser}/Library/Logs";
      in
      {
        launchd.user.agents.llm-rpc.serviceConfig = {
          ProgramArguments = [
            (llm.rpcServer (llm.packageFor pkgs host.llmBackend))
          ]
          ++ llm.rpcArgs host;
          ProcessType = "Interactive";
          KeepAlive = true;
          RunAtLoad = true;
          StandardOutPath = "${logs}/llm-rpc.log";
          StandardErrorPath = "${logs}/llm-rpc.err.log";
        };
      };
  };
}
