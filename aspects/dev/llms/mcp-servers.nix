{ den, ... }: {
  # Shared MCP registry: opencode reads programs.mcp.servers natively, OpenClaw
  # via its adapter, so a server declared here reaches both.
  den.aspects.mcp-servers = {
    # Package overlays only reach a host's pkgs when their aspect is included.
    includes = [
      den.aspects.packages.zotero-mcp
      den.aspects.packages.jj-mcp-server
      den.aspects.agenix-rekey

      # Claude Code merges the shared registry into ~/.mcp.json but expands
      # `${VAR}`, not opencode's `{env:VAR}`, so the two Cloudflare entries are
      # re-declared here under programs.claude-code.mcpServers -- which wins over
      # the shared copy of the same name (sharedMcpServers // cfg.mcpServers).
      (den.lib.whenAspect den.aspects.claude {
        homeManager = {
          programs.claude-code.mcpServers = {
            cloudflare = {
              url = "https://mcp.cloudflare.com/mcp";
              headers.Authorization = "Bearer \${CLOUDFLARE_API_TOKEN}";
            };
            cloudflare_observability = {
              url = "https://observability.mcp.cloudflare.com/mcp";
              headers.Authorization = "Bearer \${CLOUDFLARE_API_TOKEN}";
            };
          };
        };
      })
    ];

    # Cloudflare API token, pasted in by hand:
    #   nix run .#secret-edit -- secrets/cloudflare_token.age
    # Create it at https://dash.cloudflare.com/profile/api-tokens (a user token
    # or an account token; mcp.cloudflare.com accepts either as a bearer).
    secrets.cloudflare_token.rekeyFile = ../../../secrets/cloudflare_token.age;

    homeManager =
      {
        pkgs,
        lib,
        scoped,
        ...
      }:
      let
        tokenPath = scoped.mcp-servers.secrets.cloudflare_token.path;

        # `headers` is a plain attrsOf str -- no file-ref support like `env` has --
        # and the generated mcp.json lives in the world-readable store, so the
        # token cannot be interpolated here. Both clients substitute at read time
        # from the environment instead; the shell hooks below put it there.
        cloudflareHeaders = {
          Authorization = "Bearer {env:CLOUDFLARE_API_TOKEN}";
        };
      in
      {
        programs.mcp = {
          enable = true;
          servers.zotero = {
            command = "${pkgs.zotero-mcp}/bin/zotero-mcp";
            args = [ "serve" ];
            env.ZOTERO_LOCAL = "true"; # local API at localhost:23119, no creds
          };
          servers.deepwiki.url = "https://mcp.deepwiki.com/mcp";

          # The unified Cloudflare API server -- the one endpoint that documents
          # `Authorization: Bearer <api-token>` alongside OAuth.
          servers.cloudflare = {
            url = "https://mcp.cloudflare.com/mcp";
            headers = cloudflareHeaders;
          };
          servers.cloudflare_observability = {
            url = "https://observability.mcp.cloudflare.com/mcp";
            headers = cloudflareHeaders;
          };
        };

        # Guarded on readability, not existence: the secret is absent until the
        # first rekey, and an unconditional `cat` would print an error on every
        # new shell.
        programs.fish.interactiveShellInit = lib.mkAfter ''
          if test -r ${tokenPath}
            set -gx CLOUDFLARE_API_TOKEN (cat ${tokenPath})
          end
        '';

        programs.zsh.initContent = lib.mkAfter ''
          [ -r ${tokenPath} ] && export CLOUDFLARE_API_TOKEN="$(cat ${tokenPath})"
        '';

        programs.bash.initExtra = lib.mkAfter ''
          [ -r ${tokenPath} ] && export CLOUDFLARE_API_TOKEN="$(cat ${tokenPath})"
        '';

        programs.nushell.extraEnv = lib.mkAfter ''
          if ("${tokenPath}" | path exists) {
            $env.CLOUDFLARE_API_TOKEN = (open --raw "${tokenPath}" | str trim)
          }
        '';
      };
  };
}
