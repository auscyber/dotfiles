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
      ];
    };
    services.loki = {
      enable = true;
      configFile = ../../../packages/secondpc/loki.yaml;
    };
  };
}
