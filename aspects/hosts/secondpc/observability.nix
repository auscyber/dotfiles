{ den, ... }: {
  den.aspects.secondpc.nixos = { config, ... }: {
    # Jitsi-meet
    services.jitsi-meet = {
      enable = false;
      hostName = "meet.ivymect.in";
      nginx.enable = true;
      config = {
        enableWelcomePage = false;
        prejoinPageEnabled = true;
        defaultLang = "en";
      };
      interfaceConfig = {
        SHOW_JITSI_WATERMARK = false;
        SHOW_WATERMARK_FOR_GUESTS = false;
      };
    };
    services.jitsi-videobridge.openFirewall = true;

    # Observability stack
    services.prometheus = {
      enable = true;
      port = 9091;
      extraFlags = [ "--web.enable-remote-write-receiver" ];
      exporters.node = {
        enable = true;
        port = 9000;
        enabledCollectors = [ "systemd" ];
      };
      globalConfig.scrape_interval = "10s";
      scrapeConfigs = [
        {
          job_name = "node";
          static_configs = [
            { targets = [ "localhost:${toString config.services.prometheus.exporters.node.port}" ]; }
          ];
        }
        {
          job_name = "ncps";
          static_configs = [
            { targets = [ "localhost:8501" ]; }
          ];
        }
        {
          job_name = "fail2ban";
          static_configs = [
            { targets = [ "localhost:${toString config.services.prometheus.exporters.fail2ban.port}" ]; }
          ];
        }
        # sonarr/radarr/lidarr's own exportarr instances -- see
        # media/servarr-metrics.nix. One job with per-target labels rather
        # than three jobs, so a panel can select on `app` instead of
        # juggling three job names.
        {
          job_name = "exportarr";
          static_configs = [
            {
              targets = [ "localhost:${toString config.services.prometheus.exporters.exportarr-sonarr.port}" ];
              labels.app = "sonarr";
            }
            {
              targets = [ "localhost:${toString config.services.prometheus.exporters.exportarr-radarr.port}" ];
              labels.app = "radarr";
            }
            {
              targets = [ "localhost:${toString config.services.prometheus.exporters.exportarr-lidarr.port}" ];
              labels.app = "lidarr";
            }
          ];
        }
      ];
    };
  };

  # ── LOKI, OUT OF THE HOST CATCH-ALL ──
  #
  # Was `den.aspects.secondpc.nixos` here with its vhost in ./web.nix. Its own
  # aspect so it can carry its own `takes.proxy`: a take is keyed by the taking
  # ASPECT, and the host's catch-all can hold exactly one.
  den.aspects.secondpc.includes = [ den.aspects.loki ];

  den.aspects.loki = {
    includes = [ den.aspects.gateway ];

    # WAS A RAW VHOST in ./web.nix: `logs.ivymect.in` -> localhost:3100, with
    # `recommendedProxySettings = true` written per location -- redundant, since
    # ../../services/nginx.nix sets it globally on `services.nginx`.
    #
    # On the same apex as everything else so one oauth2-proxy cookie covers it
    # (see ./sso.nix). The older `logs.pierlot.com.au` vhost in ./media.nix
    # stays on basic auth for whatever is already pointed at it.
    takes.proxy = {
      subdomain = "logs";
      port = 3100;
    };

    nixos.services.loki = {
      enable = true;
      configFile = ../../../packages/secondpc/loki.yaml;
    };
  };
}
