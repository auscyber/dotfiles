{
  den,
  lib,
  capability,
  ...
}:
# ── CONFIGURATION THAT HOLDS WHILE A LINK IS UP ──
#
# A link is a resolved take (`dial`, `reachEndpoint`) and its probe chain
# (`probesFor`: carrier first, then the service). While the chain passes, what
# the link declares is in force; while it fails, it is not:
#
#   fragments  files present at their path -- a nix.conf part, say, which nix
#              re-reads on every invocation (./nix-conf-parts.nix), so a cache
#              that cannot be reached is simply not a substituter;
#   units      jobs running, and held down otherwise (`ConditionPathExists`
#              under systemd, `KeepAlive.PathState` under launchd);
#   toggle     state only an outside tool holds (Time Machine's), set on each
#              change -- the one place a command stands in for a declaration.
#
# A `link-<name>` job asserts that state on an interval and on network
# changes; every run converges, so a missed change is corrected by the next.
let
  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;
  stateDir = "/var/run/links";

  # `dial`'s answer for a link a body declares by capability and clauses.
  dialFor =
    host: cap: take:
    capability.dial cap take (hostsOf.${host.name or ""} or host);

  linkType = lib.types.submodule {
    options = {
      cap = lib.mkOption {
        type = lib.types.str;
        description = "The capability the link was resolved for; its probes are this capability's.";
      };
      resolved = lib.mkOption {
        type = lib.types.nullOr lib.types.raw;
        description = "The resolved link (`{ carrier; link; }`, as `dial` returns). Null holds nothing.";
      };
      fragments = lib.mkOption {
        type = lib.types.attrsOf lib.types.lines;
        default = { };
        description = "Files present at these absolute paths while the link is up.";
      };
      paths = lib.mkOption {
        type = lib.types.attrsOf lib.types.path;
        default = { };
        description = "Store paths placed at these absolute paths while the link is up -- an executable hook, say, where a fragment is only text.";
      };
      units = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Jobs running while the link is up: systemd units, or launchd daemon names.";
      };
      toggle = {
        on = lib.mkOption {
          type = lib.types.lines;
          default = "";
          description = "Run as root when the link comes up, for state no file expresses.";
        };
        off = lib.mkOption {
          type = lib.types.lines;
          default = "";
          description = "Run as root when it goes down.";
        };
      };
      interval = lib.mkOption {
        type = lib.types.ints.positive;
        default = 30;
        description = "Seconds between probes.";
      };
    };
  };

  # The same convergence script for both service managers; they differ only in
  # how a job is started and stopped, and how `ping` takes its timeout.
  runner =
    pkgs: platform: name: l:
    let
      state = "${stateDir}/${name}";
      files = lib.mapAttrs (_: text: pkgs.writeText "link-${name}-fragment" text) l.fragments // l.paths;
    in
    pkgs.writeShellScript "link-${name}" ''
      export PATH=/run/wrappers/bin:/sbin:/usr/bin:/bin:${
        lib.makeBinPath [
          pkgs.netcat
          pkgs.coreutils
        ]
      }
      ${platform.prelude}
      mkdir -p ${stateDir}
      up=1
      ${lib.concatMapStrings (p: ''
        ${p} >/dev/null 2>&1 || up=0
      '') (capability.probesFor l.cap l.resolved)}
      was=0
      [ -e ${state} ] && was=1
      if [ "$up" = 1 ]; then
        touch ${state}
        ${lib.concatStrings (
          lib.mapAttrsToList (path: f: ''
            mkdir -p "$(dirname ${path})"
            ln -sfn ${f} ${path}
          '') files
        )}
        ${lib.concatMapStrings (u: "${platform.start u}\n") l.units}
        [ "$was" = 1 ] || { echo "link-${name}: up"; ${l.toggle.on}
        }
      else
        rm -f ${state} ${lib.concatStringsSep " " (lib.attrNames files)}
        ${lib.concatMapStrings (u: "${platform.stop u}\n") l.units}
        [ "$was" = 0 ] || { echo "link-${name}: down"; ${l.toggle.off}
        }
      fi
    '';

  platforms = {
    nixos = {
      prelude = "";
      start = u: "systemctl start ${u}";
      stop = u: "systemctl stop ${u}";
    };
    darwin = {
      # Probes are written for Linux tools; macOS `ping -W` is milliseconds.
      prelude = ''
        ping() { /sbin/ping -c1 -t2 "''${@: -1}"; }
      '';
      start = u: "launchctl kickstart system/org.nixos.${u}";
      stop = u: "launchctl kill TERM system/org.nixos.${u}";
    };
  };

  linksOption.runtime.links = lib.mkOption {
    type = lib.types.attrsOf linkType;
    default = { };
    description = "Configuration held only while a resolved link is up.";
  };

  # Only links that resolved; an unresolvable one has nothing to probe.
  live = config: lib.filterAttrs (_: l: l.resolved != null) config.runtime.links;
in
{
  _module.args.runtimeLib = { inherit dialFor; };

  den.schema.host.includes = [ den.aspects.runtime-links ];

  den.aspects.runtime-links = {
    nixos =
      { config, pkgs, ... }:
      let
        links = live config;
      in
      {
        options = linksOption;
        config = {
          systemd.services = lib.mkMerge (
            lib.mapAttrsToList (name: l: {
              "link-${name}" = {
                description = "Hold ${name}'s configuration to its link";
                serviceConfig = {
                  Type = "oneshot";
                  ExecStart = runner pkgs platforms.nixos name l;
                };
              };
            }) links
            ++ lib.mapAttrsToList (
              name: l:
              lib.genAttrs l.units (_: {
                unitConfig.ConditionPathExists = "${stateDir}/${name}";
              })
            ) links
          );
          systemd.timers = lib.mapAttrs' (
            name: l:
            lib.nameValuePair "link-${name}" {
              wantedBy = [ "timers.target" ];
              timerConfig = {
                OnBootSec = "15s";
                OnUnitActiveSec = "${toString l.interval}s";
              };
            }
          ) links;
        };
      };

    darwin =
      { config, pkgs, ... }:
      let
        links = live config;
      in
      {
        options = linksOption;
        config.launchd.daemons = lib.mkMerge (
          lib.mapAttrsToList (name: l: {
            "link-${name}".serviceConfig = {
              ProgramArguments = [ "${runner pkgs platforms.darwin name l}" ];
              RunAtLoad = true;
              StartInterval = l.interval;
              # A network change re-runs it at once rather than at the next tick.
              WatchPaths = [ "/Library/Preferences/SystemConfiguration" ];
              StandardOutPath = "/var/log/link-${name}.log";
              StandardErrorPath = "/var/log/link-${name}.log";
            };
          }) links
          ++ lib.mapAttrsToList (
            name: l:
            lib.genAttrs l.units (_: {
              serviceConfig.KeepAlive.PathState."${stateDir}/${name}" = true;
            })
          ) links
        );
      };
  };
}
