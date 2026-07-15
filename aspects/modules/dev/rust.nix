{ den, ... }:
{
  ff.crane.url = "github:ipetkov/crane";

  # Rust's editor integration is its own aspect; nixvim pulls it in. That keeps the
  # rust-analyzer server declaration co-located with the rest of the rust tooling
  # (and with the lspmux `lsp-servers` entry) instead of scattered onto nixvim.
  den.aspects.nixvim.includes = [ den.aspects.rust ];

  den.aspects.rust = {
    includes = [ den.aspects.lspmux ];

    # Multiplex rust-analyzer through lspmux (so zed's `pkgs.lspmuxed.rust_analyzer`
    # and the runtime shim exist), but leave nvim's rust-analyzer to rustaceanvim
    # below -- `nvim = false` keeps the lspmux `nvim` body from also wiring it into
    # `lsp.servers.rust_analyzer` and starting a second client. zed drives it through
    # the shim under zed's own `rust-analyzer` server name.
    lsp-servers =
      { pkgs, ... }:
      {
        rust_analyzer = {
          package = pkgs.rust-analyzer;
          exe = "rust-analyzer";
          nvim = false;
          zed = "rust-analyzer";
          opencode = "rust";
        };
      };

    nvim =
      {
        pkgs,
        lib,
        ...
      }:
      {
        plugins = {
          lsp.servers.rust_analyzer.packageFallback = true;
          #      lsp.servers.rust_analyzer.cmd = [
          #        "${pkgs.lspmux}"
          #        "client"
          #      ];
          rustaceanvim = {
            enable = true;

            settings = {
              lsp.clientOpts.auto_attach = true;

              server = {
                auto_attach = true;
                cmd = [
                  "${lib.getExe pkgs.lspmux}"
                  "client"
                ];

                default_settings = {
                  rust-analyzer = {
                    files = {
                      excludeDirs = [ ".direnv" ];
                    };
                    inlayHints = {
                      lifetimeElisionHints = {
                        enable = "always";
                      };
                    };
                    #              checkOnSave.command = "clippy";

                    procMacro.enable = true;
                  };
                };
              };
            };
          };

          conform-nvim.settings.formatters.rustfmt = {
            command = lib.getExe pkgs.rustfmt;
          };
          conform-nvim.settings.formatters_by_ft.rust = [
            "rustfmt"
          ];
        };
      };
  };
}
