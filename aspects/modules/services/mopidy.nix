{ den, ... }:
{
  den.aspects.mopidy = {
    homeManager =
      { pkgs, ... }:
      {
        services.mopidy = {
          enable = true;
          settings.audio.output = "autoaudiosink";
          extensionPackages = with pkgs; [
            mopidy-iris
            mopidy-tidal
            mopidy-listenbrainz
          ];
        };
      };
  };
}
