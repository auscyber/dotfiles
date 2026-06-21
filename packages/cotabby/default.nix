{
  ...
}:
{
  nvfetcher.sources.cotabby = {
    src.github = "fujacob/cotabby";
    fetch.url = "https://github.com/fujacob/cotabby/releases/download/$ver/Cotabby.dmg";
  };

  den.aspects.packages.cotabby = {
    overlays = { sources, ... }: {
      cotabby = self: super: {
        cotabby = super.callPackage ./package.nix {
          source = sources.cotabby;
        };
      };
    };
  };
}
