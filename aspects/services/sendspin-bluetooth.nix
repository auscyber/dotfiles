{
  den,
  lib,
  capability,
  ...
}:
# A SENDSPIN PLAYER THAT OUTPUTS INTO THIS BOX'S BLUETOOTH SINKS.
#
# Sendspin is Music Assistant's native playback protocol, and this is the
# CLIENT half of it. ../nixos/turntable.nix says "there is no sendspin source
# client in nixpkgs and no binary to run" and that is still true -- of the
# SOURCE role, which `aiosendspin` has no capture backend for. The PLAYER role
# does have a binary: `python3Packages.sendspin` is Sendspin's own
# `sendspin-cli`, and `sendspin daemon` is its headless player.
#
# Output goes through pipewire's ALSA plugin. PortAudio has no PulseAudio
# backend on Linux -- nixpkgs builds it against alsa-lib and libjack2 only --
# so `audioDevice` is an ALSA PCM name and `pulse` is not one of the options.
#
# PAIRING IS MANUAL, ONCE. The daemon logs `Pairing required: enter PIN ...`
# and has no other surface for it, so the first connection to a new server
# needs `journalctl -u sendspin -f` and the PIN typed into Music Assistant.
let
  inherit (lib) mkOption types;

  # WHICH SERVER, resolved rather than named: ../services/homeassistant/
  # music-assistant.nix furnishes `tcp` with a `sendspin` port, and
  # `reach = "household"` on that aspect is what lets the link leave this box.
  # No `mtu` clause -- this is TCP and survives a tunnel; `preference` already
  # puts the house lan ahead of the tailnet where both exist.
  serverTake = {
    name = "music-assistant";
  };

  serverFor = host: capability.dial "tcp" serverTake host;
in
{
  den.aspects.sendspin-bluetooth = {
    includes = [ den.aspects.bluetooth ];

    takes.tcp = serverTake;

    nixos =
      {
        config,
        pkgs,
        host ? { },
        ...
      }:
      let
        cfg = config.services.sendspinBluetooth;

        resolved = serverFor host;
      in
      {
        options.services.sendspinBluetooth = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = "Run `sendspin daemon` as a player for this host's audio sinks.";
          };

          name = mkOption {
            type = types.str;
            default = config.networking.hostName;
            description = "Friendly name the server lists this player under.";
          };

          # ── WHERE THE SERVER IS: A DEFAULT, NOT A THROW ──
          #
          # The resolution IS the fallback for a value a host may also state
          # outright -- a Music Assistant this flake does not describe -- and
          # ../../CLAUDE.md puts a fallback in the option's `default`. Null
          # means nothing in this household serves it over a carrier this box
          # has; the unit is then not written and `warnings` below says so,
          # rather than one unreachable peer failing a build that is otherwise
          # fine.
          url = mkOption {
            type = types.nullOr types.str;
            default = lib.mapNullable (
              s: "ws://${s.address}:${toString s.ports.sendspin.port}/sendspin"
            ) resolved;
            defaultText = lib.literalMD "the resolved Music Assistant's Sendspin endpoint, or `null`";
            description = "WebSocket endpoint of the Sendspin server to play from.";
          };

          audioDevice = mkOption {
            type = types.str;
            default = "pipewire";
            description = ''
              ALSA PCM the player opens. `pipewire` is the device
              `services.pipewire.alsa.enable` installs; `pulse` is NOT
              available, because nixpkgs' portaudio has no PulseAudio backend.
            '';
          };

          sink = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "bluez_output.AA_BB_CC_DD_EE_FF.1";
            description = ''
              pipewire node to pin the stream to, through `PIPEWIRE_NODE`. Null
              follows the default sink, which is what bt-console's combine-sink
              is for when several speakers play as one.
            '';
          };

          staticDelayMs = mkOption {
            type = types.nullOr types.int;
            default = null;
            description = "Extra playback delay, for bluetooth latency the server cannot measure.";
          };
        };

        config = lib.mkIf cfg.enable {
          warnings = lib.optional (cfg.url == null) ''
            services.sendspinBluetooth is enabled on ${
              host.name or config.networking.hostName
            } but nothing resolves `tcp` from a service named `music-assistant` here, so no player is configured. Either a host in this household has to serve it, or `services.sendspinBluetooth.url` has to name a server this flake does not describe.
          '';

          systemd.services.sendspin = lib.mkIf (cfg.url != null) {
            wantedBy = [ "multi-user.target" ];
            wants = [
              "network-online.target"
              "bluetooth.service"
            ];
            after = [
              "network-online.target"
              "bluetooth.service"
              "pipewire.service"
            ];

            environment = {
              # System-wide pipewire's socket is `%t/pipewire-0`, and `%t` is
              # /run for a system unit -- see ./bt-console.nix.
              XDG_RUNTIME_DIR = "/run";
            }
            // lib.optionalAttrs (cfg.sink != null) { PIPEWIRE_NODE = cfg.sink; };

            serviceConfig = {
              # Root, like ./bt-console.nix: system-wide pipewire and bluez are
              # both on the system bus and this box is an appliance.
              ExecStart = lib.concatStringsSep " " (
                [
                  "${pkgs.python3Packages.sendspin}/bin/sendspin"
                  "daemon"
                  "--url ${cfg.url}"
                  "--name ${lib.escapeShellArg cfg.name}"
                  "--audio-device ${lib.escapeShellArg cfg.audioDevice}"
                  # The cryptographic identity and the pairing records live
                  # here, so re-pairing is not a reboot away.
                  "--settings-dir /var/lib/sendspin"
                  # No session bus under a system unit, so MPRIS has nothing to
                  # publish on.
                  "--disable-mpris"
                ]
                ++ lib.optional (cfg.staticDelayMs != null) "--static-delay-ms ${toString cfg.staticDelayMs}"
              );
              StateDirectory = "sendspin";
              Restart = "on-failure";
              RestartSec = 10;
            };
          };
        };
      };
  };
}
