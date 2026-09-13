{
  # Ghostty's "tip" channel: a signed .app built from every commit on `main`
  # that passes CI, published at tip.files.ghostty.org/<sha>/Ghostty.dmg.
  #
  # Version discovery goes through homebrew's `ghostty@tip` cask rather than
  # the ghostty repo itself, on purpose: not every commit on `main` gets a tip
  # build uploaded, so tracking `main`'s HEAD directly would regularly point at
  # a URL that 404s. The cask's JSON only ever names a build that exists, and
  # the regex takes the commit out of the download URL it publishes (the cask's
  # own `version` is `<build-number>,<sha>`, which is not a usable version
  # string).
  nvfetcher.sources.ghostty-tip = {
    src.webpage = "https://formulae.brew.sh/api/cask/ghostty@tip.json";
    src.regex = "tip\\.files\\.ghostty\\.org/([0-9a-f]{40})/Ghostty\\.dmg";
    fetch.url = "https://tip.files.ghostty.org/$ver/Ghostty.dmg";
  };

  den.aspects.packages.ghostty = {
    overlays =
      {
        sources,
        system,
        ...
      }:
      {
        ghostty =
          self: super:
          let
            # nixpkgs' ghostty-bin already does everything the tip DMG needs --
            # the 7zz unpack that works around the dangling terminfo symlink,
            # the bin/ wrapper, and the split man/terminfo/shell_integration/vim
            # outputs copied back out of the bundle. Only the artifact changes,
            # so override the source rather than re-package it.
            ghostty-tip = super.ghostty-bin.overrideAttrs (_: {
              inherit (sources.ghostty-tip) pname version src;
            });

            # No tip artifact exists for Linux -- ghostty publishes the tip
            # channel as a macOS .app only -- so Linux stays on the nixpkgs
            # source build.
            ghostty = {
              aarch64-darwin = ghostty-tip;
              x86_64-darwin = ghostty-tip;
              x86_64-linux = super.ghostty;
              aarch64-linux = super.ghostty;
            };
          in
          {
            ghostty = ghostty."${system}";
          };
      };
  };
}
