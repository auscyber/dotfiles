{ den, ... }: {
  den.aspects.bazarr = {
    includes = [
      den.aspects.gateway
      den.aspects.homepage
    ];

    # bazarr has no urlBase option upstream, so it cannot be path-routed onto
    # the shared arr vhost the way sonarr and radarr are.
    homepage =
      { config, ... }:
      {
        bazarr = {
          group = "Media";
          href = config.gateway.services.bazarr.url;
          icon = "bazarr.svg";
          # bazarr generates its API key on first run with no way to set it,
          # so the widget cannot be provisioned -- paste the key to light it up.
        };
      };

    gated.bazarr.upstream = "http://127.0.0.1:6767";

    nixos =
      { config, ... }:
      {

        services.bazarr = {
          enable = true;
          group = "media";
        };
        systemd.services.bazarr.serviceConfig.UMask = "0002";
      };
  };
}
