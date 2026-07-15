{ lib, den, ... }:
{

  nvfetcher.sources.vscode-kanata = {
    src.git = "https://github.com/rszyma/vscode-kanata.git";
    fetch.git = "https://github.com/rszyma/vscode-kanata.git";
  };
  #  den.aspects.nixvim.includes = [ den.aspects.packages.kanata-ls ];

  den.aspects.packages.kanata-ls = {
    nvim = { pkgs, ... }: {
      filetype.extension = {
        "kbd" = "kanata";
      };
      lsp.servers.kanata-ls = {
        package = pkgs.kanata-ls;
        config = {
          cmd = [ "${lib.getExe pkgs.kanata-ls}" ];
          filetypes = [ "kanata" ];
          root_markers = [ ".git" ];
          capabilities = {
            offsetEncoding = "utf-16";

            textDocument = {
              definition = {
                dynamicRegistration = false;
                linkSupport = true;
              };

              documentFormatting = {
                dynamicRegistration = false;
                lineEnding = "LF";
              };

              hover = {
                dynamicRegistration = false;
                contentFormat = [
                  "plaintext"
                  "markdown"
                ];
              };

              rename = {
                dynamicRegistration = false;
                prepareSupport = true;
                honorsChangeAnnotations = false;
              };
            };
          };

        };
      };
    };
    overlays = { sources, ... }: {
      kanata-ls = final: prev: {
        kanata-ls = prev.callPackage ./package.nix { source = sources.vscode-kanata; };
      };
    };
  };
}
