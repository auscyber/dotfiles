{ den, ... }: {
  den.aspects.bazarr = {
    includes = [
      den.aspects.gateway
      den.aspects.homepage
    ];

    # bazarr has no urlBase option upstream, so it cannot be path-routed onto
    # the shared arr vhost the way sonarr and radarr are.
    #
    # `internalEnv = null` with `internalKey = true`: bazarr reads no environment
    # configuration at all, so the gateway mints it a key but cannot hand it over
    # -- the unit below writes it into bazarr's own config instead.
    gated.bazarr = {
      upstream = "http://127.0.0.1:6767";
      api = {
        enable = true;
        internalKey = true;
        internalEnv = null;
        clients = [ "homepage" ];
      };
    };

    homepage = { config, ... }: {
      bazarr = {
        group = "Media";
        href = config.gateway.services.bazarr.url;
        icon = "bazarr.svg";
        widget = {
          type = "bazarr";
          url = config.gateway.services.bazarr.url;
          key = "{{HOMEPAGE_VAR_KEY}}";
        };
      };
    };

    nixos =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        services.bazarr = {
          enable = true;
          group = "media";
        };
        systemd.services.bazarr.serviceConfig.UMask = "0002";

        # bazarr keeps its settings in a YAML file it rewrites at runtime, so
        # this seeds them BEFORE it starts rather than trying to own the file.
        # `auth.type: null` turns off bazarr's own login -- reaching it at all
        # means oauth2-proxy let you through -- which is bazarr's equivalent of
        # the servarr apps' `auth.method = "External"`.
        systemd.services.bazarr-config = {
          description = "Seed bazarr's API key and disable its own login";
          before = [ "bazarr.service" ];
          requiredBy = [ "bazarr.service" ];
          serviceConfig.Type = "oneshot";
          path = [ pkgs.yq-go ];
          script =
            let
              dir = config.services.bazarr.dataDir;
            in
            ''
              cfg=${lib.escapeShellArg dir}/config/config.yaml
              install -d -o bazarr -g media ${lib.escapeShellArg dir}/config
              [ -f "$cfg" ] || echo '{}' > "$cfg"

              # Runs as root because the key is root-owned; hand the file back
              # afterwards or bazarr cannot rewrite its own settings.
              key=$(cat ${config.age.secrets."bazarr/internal".path})
              yq -i ".auth.apikey = \"$key\" | .auth.type = null" "$cfg"
              chown bazarr:media "$cfg"
            '';
        };
      };
  };
}
