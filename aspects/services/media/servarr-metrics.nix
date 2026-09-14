{ den, ... }:
# Prometheus metrics for sonarr/radarr/lidarr, via nixpkgs' own packaged
# exportarr modules (services.prometheus.exporters.exportarr-<name>) rather
# than anything hand-rolled -- exportarr polls each app's own REST API with
# its API key and re-exposes the result as /metrics.
#
# Separate file rather than folded into servarr.nix's own `servarr {...}`
# calls: `nixos` is a function per aspect, and servarr.nix already supplies
# one for each of these three -- a second file contributing its own `nixos`
# to the SAME aspect name is how this repo already does "more config for an
# aspect defined elsewhere" (aspects/hosts/secondpc/observability.nix does
# this to `secondpc`, defined in default.nix).
#
# Each app's OWN internal key (`config.age.secrets."<name>/internal"` --
# the same key gateway.nix already generates and hands to the app itself,
# via `internalSecret e = "${e.name}/internal"`) is what exportarr
# authenticates with; no new secret to mint.
{
  den.aspects.sonarr.nixos =
    { config, ... }:
    {
      services.prometheus.exporters.exportarr-sonarr = {
        enable = true;
        port = 9707;
        # Local, direct -- not through the gateway. urlbase is baked into
        # the app's own routing even on loopback, so it still has to be
        # here despite this being a same-host call.
        url = "http://127.0.0.1:8989/sonarr";
        apiKeyFile = config.age.secrets."sonarr/internal".path;
      };
    };

  den.aspects.radarr.nixos =
    { config, ... }:
    {
      services.prometheus.exporters.exportarr-radarr = {
        enable = true;
        port = 9708;
        url = "http://127.0.0.1:7878/radarr";
        apiKeyFile = config.age.secrets."radarr/internal".path;
      };
    };

  den.aspects.lidarr.nixos =
    { config, ... }:
    {
      services.prometheus.exporters.exportarr-lidarr = {
        enable = true;
        port = 9711;
        # No urlbase for lidarr (see servarr.nix).
        url = "http://127.0.0.1:8686";
        apiKeyFile = config.age.secrets."lidarr/internal".path;
      };
    };
}
