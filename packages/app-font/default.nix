{ ... }: {
  nvfetcher.sources = {
    app_font = {
      src.git = "https://github.com/kvndrsslr/sketchybar-app-font";
      fetch.git = "https://github.com/kvndrsslr/sketchybar-app-font";
    };
  };
  den.aspects.packages.sketchybar_app_font = {
    overlays = { sources, ... }: {
      sketchybar_app_font = (
        self: super: {
          # Track git for icons added after the last tag. Upstream's pnpmDeps
          # follows finalAttrs.src, so it repins automatically; its hash stays
          # valid only while the pinned rev's pnpm-lock.yaml matches the tag's.
          # If a rev bump changes the lockfile, the build fails with
          # ERR_PNPM_NO_OFFLINE_TARBALL and pnpmDeps.hash needs overriding here.
          sketchybar-app-font = super.sketchybar-app-font.overrideAttrs (old: {
            inherit (sources.app_font) src;
            version = "${old.version}-unstable-${sources.app_font.date}";
          });
        }
      );
    };
  };
}
