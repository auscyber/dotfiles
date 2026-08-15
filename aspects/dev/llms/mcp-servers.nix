{ den, ... }: {
  # Shared MCP registry: opencode reads programs.mcp.servers natively, OpenClaw
  # via its adapter, so a server declared here reaches both.
  den.aspects.mcp-servers = {
    # Package overlays only reach a host's pkgs when their aspect is included.
    includes = [
      den.aspects.packages.zotero-mcp
      den.aspects.packages.jj-mcp-server
    ];

    homeManager = { pkgs, ... }: {
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
