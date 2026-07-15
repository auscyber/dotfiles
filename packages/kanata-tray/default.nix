{ ... }: {
  nvfetcher.sources.kanata-tray = {
    src.github = "rszyma/kanata-tray";
    fetch.github = "rszyma/kanata-tray";
  };

  den.aspects.packages.kanata-tray = {
    overlays = { sources, ... }: {
      kanata-tray = self: super: {
        kanata-tray = super.callPackage ./package.nix {
          source = sources.kanata-tray;
        };
      };
    };
  };
}
