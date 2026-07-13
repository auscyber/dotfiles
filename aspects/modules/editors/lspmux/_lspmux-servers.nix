# The language servers we multiplex through lspmux.
#
# A plain list of packages, each carrying the metadata needed to wrap it:
#
#   package     the pinned language server. Only a *fallback*: the shim prefers
#               whatever `exe` the current project puts on $PATH (a devshell /
#               direnv / rustup toolchain server) and runs that under lspmux
#               instead, so a repo pinning its own rust-analyzer still gets
#               multiplexed rather than bypassed.
#   exe         the binary the server is spawned as. Not always the package's
#               mainProgram: pyright ships a `pyright` CLI *and* the
#               `pyright-langserver` LSP entrypoint, and clangd lives in clang-tools.
#               This is also the name looked up on $PATH at runtime.
#   lspconfig   the nvim-lspconfig / nixvim server name. This is what the entry is
#               mapped onto in `plugins.lsp.servers.<name>`, and it names the
#               upstream `lsp/<name>.lua` the check reads.
#   args        the args the server needs, i.e. the tail of lspconfig's `cmd`.
#               Editors that spawn the shim by absolute path (nvim `cmd`, zed
#               `binary.path`) pass these; they are not baked into the shim, or the
#               server would see them twice.
#   dynamicCmd  lspconfig builds this server's `cmd` in a lua *function* at runtime
#               (jdtls derives `-data <workspace>`, ts_ls prefers a project-local
#               node_modules server, tailwindcss likewise). Their `cmd` cannot be
#               replaced with the shim's path without losing that logic, so instead
#               they are name-shadowed: the shim goes on $PATH under `exe` and the
#               function's bare-name spawn lands in it, args and all.
#
# `exe` and `args` are hand-written but not trusted: the `lspmux-server-names` flake
# check diffs every entry against nvim-lspconfig's own `lsp/<lspconfig>.lua`, so an
# upstream rename or arg change fails the check instead of silently leaving a server
# unmultiplexed. Keep this file importable without the overlay (plain `pkgs -> list`)
# -- the check imports it too.
pkgs: [
  {
    package = pkgs.clang-tools;
    exe = "clangd";
    lspconfig = "clangd";
  }
  {
    package = pkgs.dhall-lsp-server;
    exe = "dhall-lsp-server";
    lspconfig = "dhall_lsp_server";
  }
  {
    package = pkgs.docker-compose-language-service;
    exe = "docker-compose-langserver";
    lspconfig = "docker_compose_language_service";
    args = [ "--stdio" ];
  }
  {
    package = pkgs.gopls;
    exe = "gopls";
    lspconfig = "gopls";
  }
  {
    package = pkgs.haskell-language-server;
    exe = "haskell-language-server-wrapper";
    lspconfig = "hls";
    args = [ "--lsp" ];
  }
  {
    package = pkgs.jdt-language-server;
    exe = "jdtls";
    lspconfig = "jdtls";
    dynamicCmd = true;
  }
  {
    package = pkgs.kotlin-language-server;
    exe = "kotlin-language-server";
    lspconfig = "kotlin_language_server";
  }
  {
    package = pkgs.lua-language-server;
    exe = "lua-language-server";
    lspconfig = "lua_ls";
  }
  {
    package = pkgs.metals;
    exe = "metals";
    lspconfig = "metals";
  }
  {
    package = pkgs.nil;
    exe = "nil";
    lspconfig = "nil_ls";
  }
  {
    package = pkgs.ocamlPackages.ocaml-lsp;
    exe = "ocamllsp";
    lspconfig = "ocamllsp";
  }
  {
    package = pkgs.pyright;
    exe = "pyright-langserver";
    lspconfig = "pyright";
    args = [ "--stdio" ];
  }
  {
    package = pkgs.rust-analyzer;
    exe = "rust-analyzer";
    lspconfig = "rust_analyzer";
  }
  {
    package = pkgs.tailwindcss-language-server;
    exe = "tailwindcss-language-server";
    lspconfig = "tailwindcss";
    dynamicCmd = true;
  }
  {
    package = pkgs.typescript-language-server;
    exe = "typescript-language-server";
    lspconfig = "ts_ls";
    dynamicCmd = true;
  }
  {
    package = pkgs.zls;
    exe = "zls";
    lspconfig = "zls";
  }
]
