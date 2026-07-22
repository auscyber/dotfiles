{ den, ... }: {
  # Pulls the shared MCP registry (adds programs.mcp.servers.zotero) alongside
  # the servers declared inline below; all merge into the one registry.
  den.aspects.opencode.includes = [ den.aspects.mcp-servers ];

  # jj-mcp-server built from source (importNpmLock vendors its deps from the
  # committed package-lock.json), so `mcp_jujutsu` runs a pinned store path instead
  # of `npx -y` fetching from the network on every launch.
  #
  # `nixos` uses nixpkgs' mcp-nixos (2.1.1) — a pinned store path that builds
  # cleanly. The utensils flake's newer 2.4.3 was dropped: its bundled
  # fastmcp-slim-3.2.4 has a broken unpackPhase (sourceRoot points at a
  # non-existent `source/fastmcp_slim`), which failed the whole system build.

  den.aspects.opencode.homeManager =
    {
      pkgs,
      lib,
      ...
    }:
    {
      home.packages = with pkgs; [
        gemini-cli
      ];

      # enableMcpIntegration needs claude-code >= 2.1.76; nixpkgs pins 2.1.25, so
      # bump the prebuilt binary.
      programs.claude-code = {
        enable = true;
        enableMcpIntegration = true;
        package = pkgs.claude-code.overrideAttrs (_: rec {
          version = "2.1.211";
          src = pkgs.fetchurl {
            url = "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/${version}/darwin-arm64/claude";
            hash = "sha256-WnKKdhmLbsp/PHzb/0O6tEt3tIwhCPejEH2Il3M4Jik=";
          };
        });
      };

      # MCP servers live in the shared `programs.mcp` registry: it writes
      # ~/.config/mcp/mcp.json and, via `enableMcpIntegration` below, is merged into
      # opencode's `settings.mcp`. `command` is the executable (a string) and `args`
      # its argument list -- not one combined list.
      programs.mcp = {
        enable = true;
        servers = {
          mcp_jujutsu.command = lib.getExe pkgs.jj-mcp-server;
          nixos.command = lib.getExe pkgs.mcp-nixos;
        };
      };

      programs.opencode = {
        enable = true;
        # Merge `programs.mcp.servers` into opencode's `settings.mcp`.
        enableMcpIntegration = true;
        settings = {
          # `settings.lsp` is left to the lspmux aspect, which overrides opencode's
          # built-in servers to run through the multiplexer (see editors/lspmux).

          plugin = [
            "@slkiser/opencode-quota"
            "opencode-gemini-auth@latest"
            "opencode-with-claude"
          ];

          provider = {
            anthropic = {
              options = {
                baseUrl = "http://127.0.0.1:3456";
                apiKey = "dummy";
              };
            };

            "llama-local" = {
              name = "Llama.cpp (RTX4090)";
              npm = "@ai-sdk/openai-compatible";
              options = {
                baseURL = "http://10.100.0.3:8001/v1";
              };
              # The attribute name is sent as the `model` field on the wire, so it
              # has to match the id llama-server actually advertises at
              # /v1/models -- which is the `-a/--alias` if one was passed, else
              # the GGUF's basename. It is not a free-form label; `name` is.
              models = {
                "GLM-4.7-Flash-REAP-23B-A3B" = {
                  name = "GLM-4.7-Flash-REAP-23B-A3B";
                };
                "Qwen3-Coder-30B-A3B-Instruct" = {
                  name = "Qwen3-Coder-30B-A3B-Instruct";
                };
              };
            };

            # The on-box llama-server from services/llama-cpp. A distinct
            # provider rather than another model under `llama-local`, because it
            # is a different endpoint -- providers are keyed by baseURL, models
            # only nest under one. The model key is that aspect's `--alias`, so
            # the two have to be changed together.
            "llama-cpp-local" = {
              name = "Llama.cpp (local, M4)";
              npm = "@ai-sdk/openai-compatible";
              options = {
                baseURL = "http://127.0.0.1:8080/v1";
              };
              models = {
                "qwen3-8b" = {
                  name = "Qwen3-8B (local)";
                };
              };
            };
          };
        };
      };
    };
}
