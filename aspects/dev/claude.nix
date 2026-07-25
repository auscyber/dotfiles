{
  den.aspects.claude.homeManager = { pkgs, ... }: {
    programs.claude-code = {
      enable = true;
      enableMcpIntegration = true;
      package = pkgs.claude-code.overrideAttrs (_: rec {
        version = "2.1.211";
        src = pkgs.fetchurl {
          url = "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/${version}/darwin-arm64/claude";
          hash = "sha256-WnKKdhmLbsp/PHzb/0O6tEt3tIwhCPejEH2Il3M4Jik=";
        };
      });
    };
  };
}
