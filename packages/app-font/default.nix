{
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
          sketchybar-app-font = super.sketchybar-app-font.overrideAttrs (old: {
          });
        }
      );
    };

  };
}
