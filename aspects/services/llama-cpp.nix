{ den, ... }: {
  den.aspects.llama-cpp = {
    homeManager =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      let
        cfg = config.programs.llama-cpp;
        host = "127.0.0.1";
        cacheDir = "${config.xdg.cacheHome}/llama-cpp";

        # Qwen3-8B Q4_K_M (4.7 GiB) is the largest sane resident model here: 16
        # GiB unified memory is shared with the GPU, so weights, KV cache and
        # macOS all come out of one pool. The 30B-A3B MoE is not an option
        # despite its 3B active params -- MoE only reduces compute per token,
        # the full expert weights stay resident, and it quantises to ~18 GiB.
        #
        # `-hf` resolves through LLAMA_CACHE below, so the ~4.7 GiB pull happens
        # once on first start and every later start is local. The repo is
        # ungated, so no HF token is needed in this service.
        model = "Qwen/Qwen3-8B-GGUF:Q4_K_M";

        # Stable id for OpenAI-compatible clients. Without --alias, llama-server
        # advertises the GGUF basename at /v1/models, which then has to be kept
        # in sync by hand wherever a provider names the model.
        alias = "qwen3-8b";

        # The GGUF declares 40960 native, but the KV cache is drawn from that
        # same 16 GiB: ~144 KiB/token here (36 layers, 8 GQA KV heads, f16), so
        # full context alone is ~6 GiB on top of 4.7 GiB of weights. 16k holds
        # the resident set near 7 GiB and leaves the desktop usable.
        ctx = 16384;

        args = [
          "--host"
          host
          "--port"
          (toString cfg.port)
          "-hf"
          model
          "--alias"
          alias
          "-c"
          (toString ctx)
          # Offload all layers to Metal. The GPU shares the same physical
          # memory, so there is no host/device transfer cost to trade against.
          "-ngl"
          "99"
        ];
      in
      {
        options.programs.llama-cpp.port = lib.mkOption {
          type = lib.types.port;
          default = 8080;
          description = ''
            Port the shared llama-server (OpenAI-compatible) listens on.
            Every consumer (this service's own launchd/systemd unit, and any
            other aspect pointing a client at the server, e.g. cotabby) reads
            this option rather than hardcoding the port, so it only needs
            changing in one place.
          '';
        };

        config = {
          home.file."models/.keep".text = "";
          home.file.".cache/llama-cpp/.keep".text = "";
          home.file."Library/Logs/llama-cpp/.keep".text = "";

          launchd.agents.llama_cpp = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
            enable = true;
            config = {
              Program = "${pkgs.llama-cpp}/bin/llama-server";
              # Capitalised: the option is an enum of "Background" | "Standard" |
              # "Adaptive" | "Interactive", so lowercase fails to type-check.
              # Interactive keeps launchd from CPU-throttling inference the way it
              # would a Background job.
              ProcessType = "Interactive";
              ProgramArguments = args;
              EnvironmentVariables = {
                LLAMA_CACHE = cacheDir;
                XDG_CACHE_HOME = config.xdg.cacheHome;
              };
              KeepAlive = true;

              # Without these launchd discards stdout/stderr, which matters most
              # on the very first start: that run pulls ~4.7 GiB from HuggingFace,
              # and a failure there is otherwise invisible -- KeepAlive just
              # respawns it in a loop with nothing to show for it. Kept under the
              # user-owned ~/Library/Logs for the reason documented at length in
              # apps/openclaw.nix: a path a build sandbox can create first ends up
              # unopenable by launchd, which parks the job before it ever execs.
              StandardOutPath = "${config.home.homeDirectory}/Library/Logs/llama-cpp/server.log";
              StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/llama-cpp/server.err.log";
            };
          };

          systemd.user.services.llama-cpp = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
            Unit = {
              Description = "llama.cpp llama-server (OpenAI-compatible, user)";
              After = [ "network-online.target" ];
            };
            Service = {
              Type = "simple";
              ExecStart = "${pkgs.llama-cpp}/bin/llama-server ${lib.escapeShellArgs args}";
              Environment = [
                "LLAMA_CACHE=${cacheDir}"
                "XDG_CACHE_HOME=${config.xdg.cacheHome}"
              ];
              Restart = "on-failure";
              RestartSec = 10;
            };
          };
        };
      };
  };
}
