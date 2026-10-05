{
  den,
  lib,
  capability,
  accessLib,
  ...
}:
# sonarr / radarr / lidarr: the same program three times over, so they share the
# maker in ./_servarr.nix and differ only in the table below. Each is still its
# own aspect, so a host takes only the ones it wants.
let
  # Out through a private route, and not at all when it is down.
  privateRoute = {
    segment = "internet";
    private = true;
  };
  servarr = import ./_servarr.nix {
    inherit
      den
      lib
      capability
      accessLib
      ;
  };
in
{
  den.aspects =
    # All three share the one `arr` vhost, routed by path. The paths name the
    # LIBRARY rather than the program -- /tv, /movies, /music -- because that is
    # what someone typing the address is looking for, and it survives swapping
    # an app out for another that manages the same thing. `_servarr.nix` derives
    # each app's own `urlbase` from its `subpath`, so the app generates its links
    # at the prefix nginx routes on.
    servarr {
      name = "sonarr";
      port = 8989;
      apiVersion = "v3";
      category = "tv-sonarr";
      domain = "arr";
      subpath = "/tv";
      group = "media";
      serviceGroup = "arr";
      route = privateRoute;
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
      subpath = "/movies";
      group = "media";
      serviceGroup = "arr";
      route = privateRoute;
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
      domain = "arr";
      subpath = "/music";
      # Runs as `music` so its imports land next to navidrome's library and
      # slskd's downloads without a supplementary-group detour.
      user = "media";
      group = "media";
      serviceGroup = "arr";
      route = privateRoute;
      apiOwner = "media";
      clients = [
        "prowlarr"
        #        "soularr"
        "homepage"
      ];
    };
}
