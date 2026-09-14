# Taps nginx's and kanidm's OTLP export before tempo (both point here, 4319,
# instead -- see nginx.nix/sso.nix), fans out unchanged to tempo, and also
# through filter+spanlogs+file to turn denied auth.ivymect.in logins into
# plain lines fail2ban's kanidm jail tails. kanidm never logs these to
# stdout/journald itself (checked live), so nginx's span (real client IP,
# status, path) is what the fail2ban regex keys on; kanidm's own "auth" span
# (same trace ID) rides along in the same file just for a human-readable
# reason. Pipeline + file format (one JSON line per event, body contains
# `net.sock.peer.addr=<ip>`) confirmed live with `alloy run` against
# synthetic OTLP input before wiring this up.
{ den, ... }:
{
  den.aspects.secondpc-otel-fail2ban = {
    nixos = {
      services.alloy.extraFlags = [ "--stability.level=public-preview" ];

      environment.etc."alloy/kanidm-fail2ban.alloy".text = ''
        otelcol.receiver.otlp "ingest" {
          grpc {
            endpoint = "127.0.0.1:4319"
          }

          output {
            traces = [
              otelcol.exporter.otlp.tempo.input,
              otelcol.processor.filter.nginx_auth_fail.input,
              otelcol.processor.filter.kanidm_auth_fail.input,
            ]
          }
        }

        otelcol.exporter.otlp "tempo" {
          client {
            endpoint = "127.0.0.1:4317"
            tls {
              insecure = true
            }
          }
        }

        otelcol.processor.filter "nginx_auth_fail" {
          error_mode = "ignore"

          traces {
            span = [
              `resource.attributes["service.name"] != "nginx"`,
              `attributes["net.host.name"] != "auth.ivymect.in"`,
              `attributes["http.status_code"] < 400`,
              `not IsMatch(attributes["http.target"], "^/(v1/auth|v1/account/[^/]+/_unix/_token)$")`,
            ]
          }

          output {
            traces = [otelcol.connector.spanlogs.kanidm_auth_fail.input]
          }
        }

        otelcol.processor.filter "kanidm_auth_fail" {
          error_mode = "ignore"

          traces {
            span = [
              `resource.attributes["service.name"] != "kanidmd"`,
              `name != "auth"`,
              `status.code != STATUS_CODE_ERROR`,
            ]
          }

          output {
            traces = [otelcol.connector.spanlogs.kanidm_auth_fail.input]
          }
        }

        otelcol.connector.spanlogs "kanidm_auth_fail" {
          spans            = true
          events           = true
          span_attributes  = ["net.sock.peer.addr", "http.target", "http.status_code", "client_address", "uuid"]
          event_attributes = ["level"]

          output {
            logs = [otelcol.exporter.file.kanidm_auth_fail.input]
          }
        }

        otelcol.exporter.file "kanidm_auth_fail" {
          path   = "/var/lib/alloy/kanidm-auth-fail.log"
          format = "json"
        }
      '';
    };
  };
}
