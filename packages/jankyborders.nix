{
  nvfetcher.sources.jankyborders = {
    src.git = "https://github.com/felixkratz/jankyborders";
    fetch.git = "https://github.com/felixkratz/jankyborders";

  };

  den.aspects.packages.jankyborders = {
    overlays = { sources, ... }: {
      jankyborders = self: super: {
        jankyborders = super.jankyborders.overrideAttrs (old: {
          inherit (sources.jankyborders) src version;
          patches = [ ../patches/jankyborders/offscreen.patch ];
        });

      };
    };

  };
}
