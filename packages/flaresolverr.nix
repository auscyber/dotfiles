{
  # Tracked as a container image rather than a pinned digest in the consumer:
  # `nix run .#update-sources` moves the tag and the digest together, the same
  # way every other source here is bumped.
  nvfetcher.sources.flaresolverr = {
    # `registry` is its own option: nvchecker's container source otherwise
    # defaults to docker hub and treats the host as a path on it, asking
    # registry-1.docker.io for /v2/ghcr.io/flaresolverr/... and getting a 401.
    src.container = "flaresolverr/flaresolverr";
    src.registry = "ghcr.io";
    # The fetcher does want the fully qualified name -- it becomes `imageName`
    # for dockerTools.pullImage, which resolves the host itself.
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
