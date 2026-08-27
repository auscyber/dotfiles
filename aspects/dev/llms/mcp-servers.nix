{ den, ... }: {
  # Shared MCP registry: opencode reads programs.mcp.servers natively, OpenClaw
  # via its adapter, so a server declared here reaches both.
  den.aspects.mcp-servers = {
    # Package overlays only reach a host's pkgs when their aspect is included.
    includes = [
      den.aspects.packages.zotero-mcp
      den.aspects.packages.jj-mcp-server
      den.aspects.agenix-rekey

      # Cloudflare MCP integration disabled: secrets/cloudflare_token.age was
      # never created (`nix run .#secret-edit -- secrets/cloudflare_token.age`).
      # Re-enable this block once that secret exists.
      # (den.lib.whenAspect den.aspects.claude {
      #   homeManager = {
      #     programs.claude-code.mcpServers = {
      #       cloudflare = {
      #         url = "https://mcp.cloudflare.com/mcp";
      #         headers.Authorization = "Bearer \${CLOUDFLARE_API_TOKEN}";
      #       };
      #       cloudflare_observability = {
      #         url = "https://observability.mcp.cloudflare.com/mcp";
      #         headers.Authorization = "Bearer \${CLOUDFLARE_API_TOKEN}";
      #       };
      #     };
      #   };
      # })
    ];

    # Cloudflare API token, pasted in by hand:
    #   nix run .#secret-edit -- secrets/cloudflare_token.age
    # Create it at https://dash.cloudflare.com/profile/api-tokens (a user token
    # or an account token; mcp.cloudflare.com accepts either as a bearer).
    # Disabled until that secret exists -- see above.
    # secrets.cloudflare_token.rekeyFile = ../../../secrets/cloudflare_token.age;

    homeManager =
      { pkgs, ... }:
      {
        programs.mcp = {
          enable = true;
          servers.zotero = {
            command = "${pkgs.zotero-mcp}/bin/zotero-mcp";
            args = [ "serve" ];
            env.ZOTERO_LOCAL = "true"; # local API at localhost:23119, no creds
          };
          servers.deepwiki.url = "https://mcp.deepwiki.com/mcp";
        };
      };
  };
}
