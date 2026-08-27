{
  den,
  inputs,
  ...
}:
{
  ff.pnpm-nix-provider = {
    url = "github:wmertens/pnpm-nix-provider";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  den.aspects.js = {
    # pnpm's `packageProvider` hook, so `pnpm install` materializes node_modules
    # out of the Nix store instead of downloading tarballs. The upstream module
    # brings the provider, the provider-aware pnpm build (released pnpm has no
    # `packageProvider` setting) and writes `packageProvider: pnpm-nix-provider`
    # into pnpm's global config.yaml -- installing the provider binary alone
    # leaves nothing pointing pnpm at it.
    homeManager = {
      imports = [ inputs.pnpm-nix-provider.homeManagerModules.default ];
      programs.pnpm-nix-provider = {
        enable = true;
        # Build against the host as a last resort instead of failing the install
        # when a package has no Nix expression.
        impure = true;
      };
    };
  };
}
