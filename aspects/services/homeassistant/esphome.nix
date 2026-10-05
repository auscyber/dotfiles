{ den, ... }:
# What used to be the esphome add-on: the ESPHome Device Builder dashboard,
# where the YAML for every node in the house is edited and its firmware is
# compiled and flashed.
#
# The laptop can do the same work locally -- ../../dev/esp.nix -- which is the
# path for a node that has to be flashed over USB while nowhere near this
# machine. The two share only the device YAML, and only via whoever copies it.
let
  # The Device Builder's web UI, published through the gate. Declared here
  # and furnished in `gives.tcp` below, so ../proxy-nginx.nix resolves the
  # upstream instead of being told a number.
  esphomePort = 6052;
in
{
  # ── ITS OWN ASPECT, PULLED IN BY HOME ASSISTANT ──
  #
  # This file used to write into `den.aspects.homeassistant`, as
  # ./music-assistant.nix did -- so two services shared one aspect. That was
  # workable while everything was class content (den merges it) and stopped
  # being workable the moment either wanted a `takes.<cap>`: a take is keyed by
  # the taking ASPECT, so two services on one aspect contend for one slot and
  # the second collides with the first.
  #
  # So esphome is its own aspect and Home Assistant INCLUDES it. Nothing changes
  # for the host -- ../../hosts/secondpc/default.nix includes
  # `den.aspects.homeassistant`, which now reaches this -- and the same pattern
  # is already in the tree at ../_postgres-backup.nix
  # (`den.aspects.postgres.includes = [ den.aspects.postgres-backup ]`).
  den.aspects.homeassistant.includes = [ den.aspects.esphome ];

  den.aspects.esphome = {
    # ESPHome's dashboard and Music Assistant's UI were supervisor ingress
    # panels, reachable only through HA's own session. Native they are plain web
    # apps with no auth of their own, so they go behind oauth2-proxy.
    #
    # No `api`, and so no key: the dashboard has no API a machine caller here
    # uses, and `sso` is left at its default `true`, which is the whole gate. No
    # firewall opening either -- `services.esphome.address` below is loopback, so
    # nginx is the only way in and `openFirewall` would publish the dashboard
    # directly.
    #
    # A PORT, NOT AN UPSTREAM: the proxy and this service are on one host by
    # construction, so the scheme and the loopback address are the proxy's to
    # supply (../proxy-nginx.nix).
    # ── THE NODES' BROKER LOGIN ──
    #
    # `sendspin` and not `esphome`: that is the name the add-on's own login
    # list used and it is baked into device configs, so the account is stated
    # rather than renamed. ./mosquitto.nix mints the credential keyed by this
    # ASPECT and names the account from this field.
    takes.mqtt.username = "sendspin";

    # ── AND THE INTEGRATION, FROM THE ASPECT THAT RUNS THE DASHBOARD ──
    #
    # `esphome` was a static entry in ./core.nix's `extraComponents`, which is
    # a component named for a service declared in THIS file: the day the
    # dashboard goes away the entry stays. Contributed through the edge for the
    # same reason ./mosquitto.nix and ./matter.nix contribute theirs -- and
    # unlike those two, this one survives even without the dashboard, since the
    # nodes themselves speak the ESPHome native API. It is still this aspect
    # that puts them in the house.
    takes.home-assistant.extraComponents = [ "esphome" ];

    takes.gated = {
      websockets = true;
    };

    gives =
      { self, ... }:
      {
        # WHAT IT LISTENS ON, so ../proxy-nginx.nix can ASK rather than
        # assume: the upstream is resolved by composing this with the carrier
        # the two ends share. Not exposed -- the gate is the way in.
        tcp = _: {
          name = self.name;
          ports.http = {
            port = esphomePort;
            meta.description = "The Device Builder dashboard, where a node's YAML is edited, compiled and flashed; loopback only, the gate is the way in.";
          };
        };

        # DynamicUser, so the account is literally `esphome`.
        user = _: {
          name = "esphome";
          carrier = self.name;
        };
      };

    nixos = {
      # Loopback only; the gate is the way in. Device configs live in its state
      # dir, which is where the add-on's own copies go --
      # /var/lib/private/esphome, since the module is `DynamicUser`. ./state.nix
      # is what carries them off the box.
      services.esphome = {
        enable = true;
        address = "127.0.0.1";
        port = esphomePort;

        # The module's default is `[ "char-ttyS" "char-ttyUSB" ]`, and it is
        # combined with `DevicePolicy = "closed"`, so a device class absent
        # from this list is not merely un-chowned but invisible to the unit.
        # `char-ttyACM` is the gap: an ESP32-S2/S3/C3/C6 flashed over its
        # NATIVE USB (no CP210x or CH340 bridge) enumerates as a CDC-ACM
        # device, /dev/ttyACM*, and the dashboard's "plug into this computer"
        # option silently offers nothing for one. The two defaults are kept --
        # a bridge-chip board still appears as ttyUSB*.
        allowedDevices = [
          "char-ttyS"
          "char-ttyUSB"
          "char-ttyACM"
        ];
      };
    };
  };
}
