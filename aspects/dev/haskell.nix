{ den, ... }: {
  den.aspects.haskell = {
    includes = [ den.aspects.lspmux ];
    lsp-servers = { pkgs, ... }: {
      hls = {
        package = pkgs.haskell-language-server;
        exe = "haskell-language-server-wrapper";
        args = [ "--lsp" ];
        zed = "hls";

        extensionToLanguage = {
          ".hs" = "haskell";
          ".lhs" = "haskell";
        };
      };

      opencode = "haskell-language-server";
      haskell-language-server = pkgs.haskell-language-server;
    };
  };
}
