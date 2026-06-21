{

  nvfetcher.sources.sketchybar = {
    src.git = "https://github.com/felixkratz/sketchybar";
    fetch.github = "felixkratz/sketchybar";
  };

  den.aspects.packages.sketchybar = {

    overlays = { sources, ... }: {
      sketchybar = self: super: {
        sketchybar = super.sketchybar.overrideAttrs ({
          inherit (sources.sketchybar) src;
          version = "2.24.0";
          patches = super.sketchybar.patches or [ ] ++ [
            # Fixes the build on macOS Sonoma
            ./sketchybar-pid.patch
          ];
        });

      };

    };
  };

}
