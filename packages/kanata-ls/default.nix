{
  lib,
  den,
  ...
}:
{
  nvfetcher.sources.vscode-kanata = {
    src.git = "https://github.com/rszyma/vscode-kanata.git";
    fetch.git = "https://github.com/rszyma/vscode-kanata.git";

    # Post-fetch: kanata-ls is a workspace member of the vscode-kanata tree that
    # path-depends on a sibling `kanata` checkout, so its Cargo.lock can only be
    # resolved with that sibling present. Assemble the same layout package.nix
    # builds (kanata.src linked as ../kanata), run `cargo generate-lockfile`, and
    # emit the lock as the source output. Baked into the source's
    # _sources/sha256-<hash>/ folder and surfaced as sources.vscode-kanata.output
    # (see aspects/utils/nvfetcher.nix). Tools are referenced by full store path.
    script = pkgs: ''
      mkdir -p "$out"
      tree="$(mktemp -d)"
      cp -RL "$src"/. "$tree"/
      chmod -R u+w "$tree"
      rm -rf "$tree/kanata"
      ln -s ${pkgs.kanata.src} "$tree/kanata"
      cd "$tree/kanata-ls"
      export CARGO_HOME="$(mktemp -d)"
      export SSL_CERT_FILE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      ${pkgs.cargo}/bin/cargo generate-lockfile
      cp Cargo.lock "$out/Cargo.lock"
    '';
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
