{ den, ... }:
{
  ff.crane.url = "github:ipetkov/crane";

  den.aspects.nixvim.includes = [ den.aspects.lspmux ];
  den.aspects.nixvim.nvim = { pkgs, lib, ... }: {
    plugins = {
      lsp.servers.rust_analyzer.packageFallback = true;
      lsp.servers.rust_analyzer.cmd = [
        "${pkgs.lspmux}"
        "client"
      ];
      rustaceanvim = {
        enable = true;

        settings = {
          server = {
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

}
