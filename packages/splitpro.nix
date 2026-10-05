{
  # Splitwise alternative, published only as a container image -- there is no
  # Go/Rust binary to build and the Next.js + Prisma standalone build is not
  # worth reproducing in nix for one service.
  #
  # Pulled into the store as a loadable tarball rather than fetched at runtime,
  # so the image is pinned by digest and an activation never reaches the
  # network. Same shape as ./flaresolverr.nix.
  nvfetcher.sources.splitpro = {
    src.container = "ossapps/splitpro";
    fetch.docker = "docker.io/ossapps/splitpro";
  };

  den.aspects.packages.splitpro = {
    overlays = { sources, ... }: {
      splitpro = _self: _super: {
        splitpro-image = sources.splitpro.src;
      };
    };
  };
}
