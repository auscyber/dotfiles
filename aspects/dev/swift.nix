{ den, lib, ... }:
{
  den.aspects.swift = {
    includes = [ den.aspects.lspmux ];
    lsp-servers = { pkgs, ... }: {
      sourcekit = {
        package = pkgs.sourcekit-lsp;
        exe = "sourcekit-lsp";
        extensionToLanguage.".swift" = "swift";
        opencode = "swift-ls";
      };
    };

  };
}
