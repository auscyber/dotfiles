{ lib, ... }: {
  nvfetcher.sources.sketchybar = {
    src.git = "https://github.com/felixkratz/sketchybar";
    fetch.github = "felixkratz/sketchybar";
  };

  den.aspects.packages.sketchybar = {
    overlays = { sources, ... }: {
      sketchybar = final: prev: {
        sbarlua = final.callPackage ./_sbarlua.nix { luaPackages = prev.luaPackages; };
        sketchybar = prev.sketchybar.overrideAttrs (old: {
          inherit (sources.sketchybar) src;
          version = "2.24.0";
          patches = prev.sketchybar.patches or [ ] ++ [
            # Fixes the build on macOS Sonoma
            ./sketchybar-pid.patch
          ];
        });
      };
    };
  };
}
