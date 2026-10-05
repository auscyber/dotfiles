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
#
# `gateway.services.<name>.localUrl`, not a hand-written loopback address: this
# leg is local and direct -- not through the gateway -- but the app's urlbase is
# baked into its own routing even on loopback, so a same-host call still has to
# carry the prefix. `localUrl` is `upstream + subpath`, which is the one place
# that prefix is decided, so repathing an app on the shared vhost cannot leave
# its exporter polling a 404.
{
  den.aspects.sonarr.nixos = { config, ... }: {
    services.prometheus.exporters.exportarr-sonarr = {
      enable = true;
      port = 9707;
      url = config.gateway.services.sonarr.localUrl;
      apiKeyFile = config.age.secrets."sonarr/internal".path;
    };
  };

  den.aspects.radarr.nixos = { config, ... }: {
    services.prometheus.exporters.exportarr-radarr = {
      enable = true;
      port = 9708;
      url = config.gateway.services.radarr.localUrl;
      apiKeyFile = config.age.secrets."radarr/internal".path;
    };
  };

  den.aspects.lidarr.nixos = { config, ... }: {
    services.prometheus.exporters.exportarr-lidarr = {
      enable = true;
      port = 9711;
      url = config.gateway.services.lidarr.localUrl;
      apiKeyFile = config.age.secrets."lidarr/internal".path;
    };
  };
}
