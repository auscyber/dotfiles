{
  den,
  inputs,
  ...
}:
{
  ff.pnpm-nix-provider = {
    url = "github:wmertens/pnpm-nix-provider";
    inputs.nixpkgs.follows = "nixpkgs";
    # patches/pnpm-nix-provider/execute-bit.patch: chmod -R u+w after
    # unpacking a tarball never adds the execute bit, so a package whose
    # published tarball stores directories without it (pngjs 4.0.1-7.0.0,
    # at least) ends up with an untraversable directory that later crashes
    # anything walking or removing the tree.
    patch.enable = true;
  };

  den.aspects.js = {
    # pnpm's `packageProvider` hook, so `pnpm install` materializes node_modules
    # out of the Nix store instead of downloading tarballs. The upstream module
    # brings the provider, the provider-aware pnpm build (released pnpm has no
    # `packageProvider` setting) and writes `packageProvider: pnpm-nix-provider`
    # into pnpm's global config.yaml -- installing the provider binary alone
    # leaves nothing pointing pnpm at it.
    provides.to-users.homeManager = {
      imports = [ inputs.pnpm-nix-provider.homeManagerModules.default ];
      programs.pnpm-nix-provider = {
        enable = false;
        # Build against the host as a last resort instead of failing the install
        # when a package has no Nix expression.
        impure = true;
      };
    };
  };
}
