{ config, pkgs, ... }:
{
  #  services.ncps.prometheus.enable = true;
  sops.secrets."hass_token" = {
    sopsFile = ../../../secrets/secondpc/default.yaml;
    mode = "4444";

  };
  sops.secrets."grafana/client_id" = {
    sopsFile = ../../../secrets/secondpc/grafana.yaml;
#    owner = "grafana";

  };
  sops.secrets."grafana/client_secret" = {
    sopsFile = ../../../secrets/secondpc/grafana.yaml;
#    owner = "grafana";
  };
  sops.secrets."grafana/smtp/password" = {
    sopsFile = ../../../secrets/secondpc/grafana.yaml;
#    owner = "grafana";
  };
  sops.secrets."htpasswd" = {
    sopsFile = ../../../secrets/secondpc/nginx.yaml;
    owner = config.services.nginx.user;
  };
  services.grafana = {
    enable = false;
    settings = {
      server = {
        http_addr = "127.0.0.1";
        http_port = 3001;
        enforce_domain = true;
        domain = "grafana.pierlot.com.au";
        root_url = "https://grafana.pierlot.com.au";
      };
      smtp = {
        enabled = true;
        host = "mail.imflo.pet:465";
        user = "alerts@ivymect.in";
        password = "$__file{${config.sops.secrets."grafana/smtp/password".path}}";
        from_address = "alerts@ivymect.in";
      };

      auth = {
        signout_redirect_url = "https://sso.imflo.pet/application/o/grafana/end-session/";
        oauth_auto_login = true;

      };
      "auth.generic_oauth" = {
        name = "imflo";
        enabled = true;
        client_id = "$__file{${config.sops.secrets."grafana/client_id".path}}";
        client_secret = "$__file{${config.sops.secrets."grafana/client_secret".path}}";
        scopes = "openid email profile";
        auth_url = "https://sso.imflo.pet/application/o/authorize/";
        token_url = "https://sso.imflo.pet/application/o/token/";
        api_url = "https://sso.imflo.pet/application/o/userinfo/";
        # Optionally map user groups to Grafana roles
        role_attribute_path = "contains(groups, 'Grafana Admins') && 'Admin' || contains(groups, 'Grafana Editors') && 'Editor' || 'Viewer'";
      };

    };
    provision = {
      enable = true;
      dashboards.settings.providers = [
        #        {
        #          name = "my dashboards";
        #          disableDeletion = true;
        #          options = {
        #            path = "/etc/grafana-dashboards";
        #            foldersFromFilesStructure = true;
        #          };
        #        }
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
  services.loki = {
    enable = true;
    extraFlags = [ "" ];
    configFile = ./loki.yaml;

  };
  services.alloy = {
    enable = true;
    configPath = ./alloy;

  };
  systemd.services.alloy.serviceConfig.SupplementaryGroups = [ "docker" ];
  services.prometheus = {
    enable = true;
    port = 9091;
    extraFlags = [ "--web.enable-remote-write-receiver" ];
    exporters.node = {
      enable = true;
      port = 9000;
      enabledCollectors = [ "systemd" ];

    };
    globalConfig.scrape_interval = "10s"; # "1m"
    remoteWrite = [
      {
        url = "http://100.64.0.1:9090/api/v1/write";
      }

    ];
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
        job_name = "cadvisor";
        static_configs = [
          {
            targets = [ "100.64.0.3:8069" ];
          }
        ];
      }
      {
        job_name = "authentik";
        static_configs = [
          {
            targets = [ "100.64.0.3:9300" ];
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

  services.nginx.virtualHosts."logs.pierlot.com.au" = {
    addSSL = true;
    useACMEHost = "logs.pierlot.com.au";
    basicAuthFile = config.sops.secrets."htpasswd".path;
    locations."/" = {
      proxyPass = "http://localhost:3100";
      recommendedProxySettings = true;
    };

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
