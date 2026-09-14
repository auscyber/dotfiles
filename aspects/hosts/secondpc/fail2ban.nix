# fail2ban for sshd, auth.ivymect.in (kanidm) and oauth2-proxy; nginx-botsearch/
# nginx-bad-request cover generic probing across the rest of nginx.
#
# kanidm never logs denied logins to stdout/journald itself (checked live),
# so the kanidm jail reads the plain-text feed otel-fail2ban.nix derives from
# nginx's OTLP spans instead. Live traffic during setup showed an active
# username-enumeration scan against /v1/account/<name>/_unix/_token -- that
# path is covered by the same regex as /v1/auth.
{ den, ... }:
{
  den.aspects.secondpc-fail2ban = {
    nixos = {
      environment.etc."fail2ban/filter.d/kanidm.conf".text = ''
        [Definition]
        failregex = net\.sock\.peer\.addr=<HOST>[\s"]
        ignoreregex =
      '';

      # oauth2-proxy already logs every gated request to its own unit's
      # journal, plain text with a real client IP -- no otel needed here.
      # Confirmed live: a bad bearer token logged
      #   ::1 - <reqid> - - [ts] grafana.ivymect.in GET - "/oauth2/auth?..." HTTP/1.1 "curl/8.21.0" 401 13 0.005
      environment.etc."fail2ban/filter.d/oauth2-proxy.conf".text = ''
        [Definition]
        failregex = ^<HOST> - \S+ - \S+ \[.*\] .* (401|403) \d+ [\d.]+$
        ignoreregex =
      '';

      services.fail2ban = {
        enable = true;
        maxretry = 5;
        bantime = "1h";
        bantime-increment.enable = true;
        bantime-increment.maxtime = "1w";
        ignoreIP = [
          "127.0.0.0/8"
          "::1"
          "192.168.0.0/24" # home LAN
          "10.100.0.0/24" # wireguard tunnel, aspects/network/vpn.nix
        ];

        jails = {
          sshd.settings = {
            enabled = true;
            backend = "systemd";
            filter = "sshd";
            maxretry = 5;
            findtime = "10m";
          };

          kanidm.settings = {
            enabled = true;
            backend = "auto";
            filter = "kanidm";
            logpath = "/var/lib/alloy/kanidm-auth-fail.log";
            maxretry = 5;
            findtime = "10m";
          };

          oauth2-proxy.settings = {
            enabled = true;
            backend = "systemd";
            filter = "oauth2-proxy";
            journalmatch = "_SYSTEMD_UNIT=oauth2-proxy.service";
            maxretry = 5;
            findtime = "10m";
          };

          nginx-botsearch.settings = {
            enabled = true;
            port = "http,https";
            logpath = "/var/log/nginx/*.log";
            maxretry = 3;
          };

          nginx-bad-request.settings = {
            enabled = true;
            port = "http,https";
            logpath = "/var/log/nginx/*.log";
            maxretry = 5;
          };
        };
      };

      # Scraped by prometheus -- see observability.nix's scrapeConfigs.
      services.prometheus.exporters.fail2ban.enable = true;
    };
  };
}
