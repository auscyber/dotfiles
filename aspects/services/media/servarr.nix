{
  den,
  lib,
  ...
}:
# sonarr / radarr / lidarr: the same program three times over, so they share the
# maker in ./_servarr.nix and differ only in the table below. Each is still its
# own aspect, so a host takes only the ones it wants.
let
  servarr = import ./_servarr.nix { inherit den lib; };
in
{
  den.aspects =
    # sonarr/radarr share one path-routed vhost, so each needs a urlbase
    # matching its location or the app generates links at the wrong prefix.
    servarr {
      name = "sonarr";
      port = 8989;
      apiVersion = "v3";
      category = "tv-sonarr";
      domain = "arr";
      subpath = "/sonarr";
      urlbase = "/sonarr";
      group = "media";
      clients = [
        "prowlarr"
        "seerr"
        "homepage"
      ];
    }
    // servarr {
      name = "radarr";
      port = 7878;
      apiVersion = "v3";
      category = "radarr";
      domain = "arr";
      subpath = "/radarr";
      urlbase = "/radarr";
      group = "media";
      clients = [
        "prowlarr"
        "seerr"
        "homepage"
      ];
    }
    // servarr {
      name = "lidarr";
      port = 8686;
      apiVersion = "v1";
      category = "lidarr";
      # Runs as `music` so its imports land next to navidrome's library and
      # slskd's downloads without a supplementary-group detour.
      user = "media";
      group = "media";
      apiOwner = "media";
      clients = [
        "prowlarr"
        "soularr"
        "homepage"
      ];
    };
}
