{ config, pkgs, ... }:
{
  #  services.ncps.prometheus.enable = true;
  sops.secrets."hass_token" = {
    sopsFile = ../../../secrets/secondpc/default.yaml;
    mode = "4444";

  };
  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "127.0.0.1";
        http_port = 3001;
        enforce_domain = true;
        domain = "grafana.pierlot.com.au";
      };

    };
    provision = {
      enable = true;
      dashboards.settings.providers = [
        {
          name = "my dashboards";
          disableDeletion = true;
          options = {
            path = "/etc/grafana-dashboards";
            foldersFromFilesStructure = true;
          };
        }
      ];

      datasources.settings.datasources = [
        # Provisioning a built-in data source
        {
          name = "Prometheus";
          type = "prometheus";
          url = "http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
          isDefault = true;
          editable = false;
        }
      ];
    };

  };
  services.prometheus = {
    enable = true;
    port = 9091;
    exporters.node = {
      enable = true;
      port = 9000;
      enabledCollectors = [ "systemd" ];

    };
    globalConfig.scrape_interval = "10s"; # "1m"
    scrapeConfigs = [
      {
        job_name = "node";
        static_configs = [
          {
            targets = [
              "localhost:${toString config.services.prometheus.exporters.node.port}"
              "100.64.0.3:9100"
            ];
          }
        ];
      }
      {
        job_name = "ncps";
        static_configs = [
          {

            targets = [ "localhost:8501" ];
          }

        ];
      }
      {
        job_name = "docker";
        static_configs = [
          {
            targets = [ "100.64.0.3:9323" ];
          }
        ];
      }
      #      {
      #        job_name = "home_asssistant";
      #        metrics_path = "/api/prometheus";
      #        bearer_token_file = config.sops.secrets.hass_token.path;
      #        static_configs = [
      #          {
      #            targets = [ "192.168.0.37:8123" ];
      #          }
      #        ];
      #      }

    ];
  };

  services.nginx.virtualHosts."${config.services.grafana.settings.server.domain}" = {
    addSSL = true;
    useACMEHost = "grafana.pierlot.com.au";
    locations."/" = {
      proxyPass = "http://${toString config.services.grafana.settings.server.http_addr}:${toString config.services.grafana.settings.server.http_port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
    };
  };

}
