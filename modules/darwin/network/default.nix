{ ... }:
let
in
{

}
#{
#  config,
#  lib,
#  pkgs,
#  ...
#}:
#
#with lib;
#
#let
#  cfg = config.services.tailscale;
#
#in
#{
#  imports = [
#    (mkRemovedOptionModule [
#      "services"
#      "tailscale"
#      "domain"
#    ] "Tailscale no longer requires setting the search domain manually.")
#    (mkRemovedOptionModule [ "services" "tailscale" "magicDNS" ]
#      "MagicDNS no longer requires overriding the DNS servers, if this is necessary you can use `services.tailscale.overrideLocalDns`."
#    )
#  ];
#
#  options.services.tailscale = {
#    enable = mkEnableOption "Tailscale client daemon";
#
#    package = mkOption {
#      type = types.package;
#      default = pkgs.tailscale;
#      defaultText = literalExpression "pkgs.tailscale";
#      description = "The package to use for tailscale";
#    };
#    port = mkOption {
#      type = types.port;
#      default = 41641;
#      description = "The port to listen on for tunnel traffic (0=autoselect).";
#    };
#    authKeyFile = mkOption {
#      type = types.nullOr types.path;
#      default = null;
#      example = "/run/secrets/tailscale_key";
#      description = ''
#        A file containing the auth key.
#        Tailscale will be automatically started if provided.
#
#        Services that bind to Tailscale IPs should order using {option}`systemd.services.<name>.after` `tailscaled-autoconnect.service`.
#      '';
#    };
#
#    authKeyParameters = mkOption {
#      type = types.submodule {
#        options = {
#          ephemeral = mkOption {
#            type = types.nullOr types.bool;
#            default = null;
#            description = "Whether to register as an ephemeral node.";
#          };
#          preauthorized = mkOption {
#            type = types.nullOr types.bool;
#            default = null;
#            description = "Whether to skip manual device approval.";
#          };
#          baseURL = mkOption {
#            type = types.nullOr types.str;
#            default = null;
#            description = "Base URL for the Tailscale API.";
#          };
#        };
#      };
#      default = { };
#      description = ''
#        Extra parameters to pass after the auth key.
#        See <https://tailscale.com/kb/1215/oauth-clients#registering-new-nodes-using-oauth-credentials>
#      '';
#    };
#
#    extraUpFlags = mkOption {
#      description = ''
#        Extra flags to pass to {command}`tailscale up`. Only applied if {option}`services.tailscale.authKeyFile` is specified.
#      '';
#      type = types.listOf types.str;
#      default = [ ];
#      example = [ "--ssh" ];
#    };
#
#    extraSetFlags = mkOption {
#      description = "Extra flags to pass to {command}`tailscale set`.";
#      type = types.listOf types.str;
#      default = [ ];
#      example = [ "--advertise-exit-node" ];
#    };
#
#    extraDaemonFlags = mkOption {
#      description = "Extra flags to pass to {command}`tailscaled`.";
#      type = types.listOf types.str;
#      default = [ ];
#      example = [ "--no-logs-no-support" ];
#    };
#
#    overrideLocalDns = mkOption {
#      type = types.bool;
#      default = false;
#      example = true;
#      description = ''
#        This option implements `Override local DNS` as it is not yet implemented in Tailscaled-on-macOS.
#
#        To use this option, in the Tailscale control panel:
#          1. at least one DNS server is added
#          2. `Override local DNS` is enabled
#
#        As this option sets 100.100.100.100 as your sole DNS server, if the requirements above are not met,
#        all non-MagicDNS queries WILL fail.
#      '';
#    };
#  };
#
#  config = mkIf cfg.enable {
#    assertions = [
#      {
#        assertion = !cfg.overrideLocalDns || config.networking.dns == [ "100.100.100.100" ];
#        message = ''
#          DNS servers should be configured on the Tailscale control panel when `services.tailscale.overrideLocalDns` is enabled.
#
#          A race condition can occur when DNS servers are set locally, leading to MagicDNS to not work.
#        '';
#      }
#    ];
#
#    environment.systemPackages = [ cfg.package ];
#    launchd.daemons.tailscaled-autoconnect = mkIf (cfg.authKeyFile != null) {
#      serviceConfig = {
#        Type = "notify";
#      };
#      path = [
#        cfg.package
#        pkgs.jq
#      ];
#      enableStrictShellChecks = true;
#      script =
#        let
#          paramToString = v: if (builtins.isBool v) then (lib.boolToString v) else (toString v);
#          params = lib.pipe cfg.authKeyParameters [
#            (lib.filterAttrs (_: v: v != null))
#            (lib.mapAttrsToList (k: v: "${k}=${paramToString v}"))
#            (builtins.concatStringsSep "&")
#            (params: if params != "" then "?${params}" else "")
#          ];
#        in
#        # bash
#        ''
#          getState() {
#            tailscale status --json --peers=false | jq -r '.BackendState'
#          }
#
#          lastState=""
#          while state="$(getState)"; do
#            if [[ "$state" != "$lastState" ]]; then
#              # https://github.com/tailscale/tailscale/blob/v1.72.1/ipn/backend.go#L24-L32
#              case "$state" in
#                NeedsLogin|NeedsMachineAuth|Stopped)
#                  echo "Server needs authentication, sending auth key"
#                  tailscale up --auth-key "$(cat ${cfg.authKeyFile})${params}" ${escapeShellArgs cfg.extraUpFlags}
#                  ;;
#                Running)
#                  echo "Tailscale is running"
#                  systemd-notify --ready
#                  exit 0
#                  ;;
#                *)
#                  echo "Waiting for Tailscale State = Running or systemd timeout"
#                  ;;
#              esac
#              echo "State = $state"
#            fi
#            lastState="$state"
#            sleep .5
#          done
#        '';
#    };
#
#    launchd.daemons.tailscaled = {
#      # derived from
#      # https://github.com/tailscale/tailscale/blob/main/cmd/tailscaled/install_darwin.go#L30
#      command = lib.getExe' cfg.package "tailscaled";
#      serviceConfig = {
#        Label = "com.tailscale.tailscaled";
#        RunAtLoad = true;
#      };
#    };
#
#    networking.dns = mkIf cfg.overrideLocalDns [ "100.100.100.100" ];
#
#    # Ensures Tailscale MagicDNS always works even without adding 100.100.100.100 to DNS servers
#    environment.etc."resolver/ts.net".text = "nameserver 100.100.100.100";
#
#    # This file gets created by tailscaled when `Override local DNS` is turned off
#    environment.etc."resolver/ts.net".knownSha256Hashes = [
#      "2c28f4fe3b4a958cd86b120e7eb799eee6976daa35b228c885f0630c55ef626c"
#    ];
#
#    # Cleaning up the .before-nix-darwin file is necessary as any files in /etc/resolver will be used.
#    system.activationScripts.etc.text = mkAfter ''
#      if [ -e /etc/resolver/ts.net.before-nix-darwin ]; then
#        rm /etc/resolver/ts.net.before-nix-darwin
#      fi
#    '';
#  };
#}
