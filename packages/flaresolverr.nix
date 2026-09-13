{
  # Tracked as a container image rather than a pinned digest in the consumer:
  # `nix run .#update-sources` moves the tag and the digest together, the same
  # way every other source here is bumped.
  nvfetcher.sources.flaresolverr = {
    src.container = "ghcr.io/flaresolverr/flaresolverr";
    fetch.docker = "ghcr.io/flaresolverr/flaresolverr";
  };

  den.aspects.packages.flaresolverr = {
    overlays = { sources, ... }: {
      flaresolverr = _self: _super: {
        # A loadable tarball, so the container starts from the store instead
        # of pulling at runtime.
        flaresolverr-image = sources.flaresolverr.src;
      };
    };
  };
}
