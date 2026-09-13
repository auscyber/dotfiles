{ den, ... }:
# Journal -> loki, which is how "every service ships its logs" gets answered
# without touching a single service: they all log to the journal already, and
# the unit name is the label that separates them.
#
# nginx is the exception worth calling out -- it writes to files, and one of
# those files is the gateway's API access log, whose `gw_caller` field is the
# whole point of the per-caller keys. Shipping it makes "which caller hit this
# API" a grafana query rather than an ssh session.
{
  den.aspects.alloy = {
    nixos = { config, ... }: {
      services.alloy.enable = true;

      # DynamicUser, so reading nginx's log directory needs an explicit
      # supplementary group. systemd-journal is already granted by the module.
      systemd.services.alloy.serviceConfig.SupplementaryGroups = [
        config.services.nginx.group
      ];

      environment.etc."alloy/journal.alloy".text = ''
        loki.write "local" {
          endpoint {
            url = "http://127.0.0.1:3100/loki/api/v1/push"
          }
        }

        loki.relabel "journal" {
          forward_to = []

          rule {
            source_labels = ["__journal__systemd_unit"]
            target_label  = "unit"
          }
          rule {
            source_labels = ["__journal__hostname"]
            target_label  = "host"
          }
          rule {
            source_labels = ["__journal_priority_keyword"]
            target_label  = "level"
          }
        }

        loki.source.journal "read" {
          forward_to    = [loki.write.local.receiver]
          relabel_rules = loki.relabel.journal.rules
          labels        = { job = "systemd-journal" }
        }

        local.file_match "nginx" {
          path_targets = [{
            __path__ = "/var/log/nginx/*.log",
            job      = "nginx",
            host     = "${config.networking.hostName}",
          }]
        }

        loki.source.file "nginx" {
          targets    = local.file_match.nginx.targets
          forward_to = [loki.write.local.receiver]
        }
      '';
    };
  };
}
