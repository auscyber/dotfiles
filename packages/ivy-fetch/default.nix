{ rootPath, ... }:
{

  den.aspects.packages.ivy-fetch = {
    overlays = { sources, ... }: {
      ivy-fetch = self: super: {
        ivy-fetch = super.callPackage ./package.nix {
          lyricsList = builtins.path {
            name = "lyricslist";
            path = rootPath + "phoebelyrics/lyricslist";
          };
        };
      };
    };

  };

}
