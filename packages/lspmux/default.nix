{
  nvfetcher.sources.lspmux = {
    src.git = "https://codeberg.org/p2502/lspmux.git";
    fetch.git = "https://codeberg.org/p2502/lspmux.git";
    cargo_lock = [ "Cargo.lock" ];
  };

  den.aspects.packages.lspmux = {
    overlays = { sources, ... }: {
      lspmux = self: super: {
        lspmux = super.callPackage ./package.nix {
          source = sources.lspmux;
        };
      };
    };
  };
}
