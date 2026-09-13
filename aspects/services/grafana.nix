{ den, ... }:
# Grafana over the gateway's loki + prometheus. Like the servarr apps it does
# not authenticate anyone itself: oauth2-proxy has already done that by the time
# a request arrives, and nginx forwards the identity it established, so grafana
# trusts the header and shows no login form. That makes `http_addr` load
# bearing -- on loopback only, nginx is the one way in.
{
  den.aspects.grafana = {
    includes = [ den.aspects.gateway ];

    gated.grafana = {
      upstream = "http://127.0.0.1:3000";
      websockets = true; # live tail / streaming panels
    };

    nixos =
      { config, ... }:
      let
        inherit (config.services.prometheus) port;
        url = "https://grafana.${config.gateway.domain}";
      in
      {
        services.grafana = {
          enable = true;
          settings = {
            server = {
              http_addr = "127.0.0.1";
              http_port = 3000;
              domain = "grafana.${config.gateway.domain}";
              root_url = url;
            };

            # `X-Email` is set by the nginx locations the gateway writes, from
            # what oauth2-proxy resolved. `auto_sign_up` means a kanidm account
            # that gets through the gate exists in grafana on first sight.
            "auth.proxy" = {
              enabled = true;
              header_name = "X-Email";
              header_property = "email";
              auto_sign_up = true;
            };
            auth = {
              disable_login_form = true;
              disable_signout_menu = true;
            };
            users.auto_assign_org_role = "Admin";
            analytics.reporting_enabled = false;
          };

          provision.datasources.settings.datasources = [
            {
              name = "Prometheus";
              type = "prometheus";
              uid = "prometheus";
              url = "http://127.0.0.1:${toString port}";
              isDefault = true;
            }
            {
              name = "Loki";
              type = "loki";
              uid = "loki";
              url = "http://127.0.0.1:3100";
            }
          ];
        };
      };
  };
}
