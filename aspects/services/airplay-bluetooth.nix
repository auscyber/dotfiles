{ den, ... }:
# One AirPlay 2 receiver per bluetooth audio device.
#
# Why a container per device rather than one shairport-sync with a switchable
# output: upstream AIRPLAY2.md says two AirPlay 2 instances cannot share an IP
# ("AirPlay 2 clients are confused by having multiple AirPlay 2 players at the
# same IP addresses"), and nqptp needs exclusive UDP 319/320. Both constraints
# are per-network-namespace, so giving every container its own L2 address on
# the LAN dissolves them AND is what mDNS needs to advertise each receiver
# separately.
#
# Default driver is `ipvlan`, not `macvlan`: this box's only uplink is wifi,
# and an 802.11 station cannot carry extra MAC addresses. ipvlan l2 hands out
# separate IPs behind the parent's single MAC, which is what works over wlan.
# Switch `network.driver` to "macvlan" on a wired host.
#
# Audio does NOT go over the network -- ipvlan/macvlan deliberately blocks
# container<->host traffic -- so the pipewire socket is bind-mounted in and
# each receiver is pinned to its device's bluez sink node.
#
# The image is `mikebrady/shairport-sync:latest`, which is the AirPlay 2 build
# (the `-classic` tags are AirPlay 1) and already bundles nqptp + its own
# avahi/dbus. nixpkgs has `shairport-sync-airplay2` and `nqptp`, but no module
# wiring them together, and the container gets a private avahi for free.
{
  den.aspects.airplay-bluetooth = {
    includes = [ den.aspects.bluetooth ];

    nixos =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        inherit (lib) mkOption types;

        cfg = config.services.airplayBluetooth;

        mac = types.strMatching "([0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2}";
        ipv4 = types.strMatching "([0-9]{1,3}\\.){3}[0-9]{1,3}";
        ipv4Cidr = types.strMatching "([0-9]{1,3}\\.){3}[0-9]{1,3}/[0-9]{1,2}";

        underscored = addr: lib.replaceStrings [ ":" ] [ "_" ] (lib.toUpper addr);

        containerName = name: "airplay-${name}";
        unitName = name: "docker-${containerName name}.service";

        confFile =
          name: dev:
          pkgs.writeText "shairport-sync-${name}.conf" ''
            general = {
              name = "${dev.displayName}";
              output_backend = "pipewire";
            };
            pipewire = {
              sink_target = "${dev.sinkTarget}";
            };
          '';

        pythonEnv = pkgs.python3.withPackages (ps: [
          ps.dbus-python
          ps.pygobject3
        ]);

        unitsByMac = lib.mapAttrs' (
          name: dev: lib.nameValuePair (lib.toUpper dev.address) (unitName name)
        ) cfg.devices;

        watcher = pkgs.writeText "airplay-bt-watch.py" ''
          import json
          import subprocess
          import time

          import dbus
          from dbus.mainloop.glib import DBusGMainLoop
          from gi.repository import GLib

          UNITS = json.loads(r"""${builtins.toJSON unitsByMac}""")
          AUDIO_UUIDS = {
              "0000110b-0000-1000-8000-00805f9b34fb",  # A2DP Audio Sink
              "0000110a-0000-1000-8000-00805f9b34fb",  # A2DP Audio Source
          }

          bus = None


          def is_audio(uuids):
              return bool(AUDIO_UUIDS & {str(u).lower() for u in uuids})


          def apply(address, connected):
              unit = UNITS.get(str(address).upper())
              if unit is None:
                  return
              action = "start" if connected else "stop"
              subprocess.run(["systemctl", "--no-block", action, unit], check=False)


          def props(path):
              obj = bus.get_object("org.bluez", path)
              iface = dbus.Interface(obj, "org.freedesktop.DBus.Properties")
              return iface.GetAll("org.bluez.Device1")


          def on_interfaces_added(path, interfaces):
              dev = interfaces.get("org.bluez.Device1")
              if dev and is_audio(dev.get("UUIDs", [])):
                  apply(dev.get("Address", ""), bool(dev.get("Connected", False)))


          def on_properties_changed(interface, changed, invalidated, path=None):
              if interface != "org.bluez.Device1" or "Connected" not in changed:
                  return
              try:
                  dev = props(path)
              except dbus.DBusException:
                  return
              if is_audio(dev.get("UUIDs", [])):
                  apply(dev.get("Address", ""), bool(changed["Connected"]))


          def wait_for_bluez(timeout=180):
              deadline = time.monotonic() + timeout
              while time.monotonic() < deadline:
                  try:
                      if bus.name_has_owner("org.bluez"):
                          return True
                  except dbus.DBusException:
                      pass
                  time.sleep(2)
              return False


          def reconcile():
              obj = bus.get_object("org.bluez", "/")
              mgr = dbus.Interface(obj, "org.freedesktop.DBus.ObjectManager")
              for path, interfaces in mgr.GetManagedObjects().items():
                  on_interfaces_added(path, interfaces)


          def main():
              global bus
              DBusGMainLoop(set_as_default=True)
              bus = dbus.SystemBus()
              if not wait_for_bluez():
                  raise SystemExit("airplay-bt-watch: org.bluez never appeared")
              bus.add_signal_receiver(
                  on_interfaces_added,
                  dbus_interface="org.freedesktop.DBus.ObjectManager",
                  signal_name="InterfacesAdded",
              )
              bus.add_signal_receiver(
                  on_properties_changed,
                  dbus_interface="org.freedesktop.DBus.Properties",
                  signal_name="PropertiesChanged",
                  arg0="org.bluez.Device1",
                  path_keyword="path",
              )
              reconcile()
              GLib.MainLoop().run()


          main()
        '';
      in
      {
        options.services.airplayBluetooth = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = "Run an AirPlay 2 receiver container per bluetooth audio device.";
          };

          image = mkOption {
            type = types.str;
            # Pinned rather than `:latest`: the backend only pulls when the tag
            # is absent locally, so a floating tag is pinned-on-first-pull
            # anyway -- just to an unknown version.
            default = "mikebrady/shairport-sync:5.5.2";
            description = ''
              The AirPlay 2 shairport-sync image. Unsuffixed tags are AirPlay 2;
              `-classic` tags are AirPlay 1 and will not work here.
            '';
          };

          imageFile = mkOption {
            type = types.nullOr types.package;
            default = null;
            description = ''
              Optional tarball to load instead of pulling. Must carry exactly the
              `name:tag` spelled in `image`, or the backend pulls anyway.
            '';
          };

          pipewireSocket = mkOption {
            type = types.path;
            default = "/run/pipewire/pipewire-0";
            description = ''
              Host pipewire socket bind-mounted into each container. The default
              is the system-wide one (`services.pipewire.systemWide`), which is
              what lets a non-login service user share one audio graph -- and
              one bluez media endpoint -- with whatever else is playing.
            '';
          };

          user = mkOption {
            type = types.str;
            default = "airplay";
            description = ''
              Service account the container units run as. It is NOT root: it
              only needs the docker socket (group `docker`) and the pipewire
              socket (group `pipewire`). Processes *inside* the container still
              run as root, because nqptp binds UDP 319/320.
            '';
          };

          group = mkOption {
            type = types.str;
            default = "airplay";
          };

          network = {
            name = mkOption {
              type = types.str;
              default = "airplay";
            };
            driver = mkOption {
              type = types.enum [
                "ipvlan"
                "macvlan"
              ];
              default = "ipvlan";
              description = "macvlan needs a wired parent; ipvlan also works over wifi.";
            };
            parent = mkOption {
              type = types.str;
              description = "Host interface the containers sit on, e.g. `wlp2s0`.";
            };
            subnet = mkOption {
              type = ipv4Cidr;
              description = "The LAN subnet, which each device's `ip` must fall inside.";
            };
            gateway = mkOption {
              type = types.nullOr ipv4;
              default = null;
            };
          };

          devices = mkOption {
            default = { };
            description = "Bluetooth audio devices to give a receiver to.";
            type = types.attrsOf (
              types.submodule (
                {
                  name,
                  config,
                  ...
                }:
                {
                  options = {
                    address = mkOption {
                      type = mac;
                      description = "The device's bluetooth address.";
                    };
                    ip = mkOption {
                      type = ipv4;
                      description = "LAN address for this device's container. Must be free and outside the DHCP pool.";
                    };
                    displayName = mkOption {
                      type = types.str;
                      default = name;
                      description = "Name the receiver advertises over AirPlay.";
                    };
                    sinkTarget = mkOption {
                      type = types.str;
                      default = "bluez_output.${underscored config.address}.1";
                      description = ''
                        pipewire node name of the device's sink. The default is
                        wireplumber's usual spelling; check `wpctl status` if the
                        device lands on a different profile index.
                      '';
                    };
                  };
                }
              )
            );
          };
        };

        config = lib.mkIf cfg.enable {
          virtualisation.docker.enable = true;
          virtualisation.oci-containers.backend = lib.mkDefault "docker";

          users.groups.${cfg.group} = { };
          users.users.${cfg.user} = {
            isSystemUser = true;
            group = cfg.group;
            extraGroups = [
              "docker"
              "pipewire"
              "audio"
            ];
          };

          virtualisation.oci-containers.containers = lib.mapAttrs' (
            name: dev:
            lib.nameValuePair (containerName name) {
              inherit (cfg) image imageFile;
              # Started by the bluez watcher, not at boot.
              autoStart = false;
              volumes = [
                "${confFile name dev}:/etc/shairport-sync.conf:ro"
                "${cfg.pipewireSocket}:/tmp/pipewire-0"
              ];
              environment.XDG_RUNTIME_DIR = "/tmp";
              extraOptions = [
                "--network=${cfg.network.name}"
                "--ip=${dev.ip}"
                # shairport-sync's timing thread wants realtime priority.
                "--cap-add=SYS_NICE"
              ];
            }
          ) cfg.devices;

          systemd.services = lib.mkMerge [
            {
              # oci-containers cannot create networks, and `docker network
              # create` is not idempotent.
              airplay-network = {
                wantedBy = [ "multi-user.target" ];
                after = [ "docker.service" ];
                requires = [ "docker.service" ];
                path = [ config.virtualisation.docker.package ];
                serviceConfig = {
                  Type = "oneshot";
                  RemainAfterExit = true;
                };
                script =
                  let
                    args = [
                      "--driver"
                      cfg.network.driver
                      "--subnet"
                      cfg.network.subnet
                    ]
                    ++ lib.optionals (cfg.network.gateway != null) [
                      "--gateway"
                      cfg.network.gateway
                    ]
                    ++ [
                      "--opt"
                      "parent=${cfg.network.parent}"
                      "--opt"
                      (if cfg.network.driver == "ipvlan" then "ipvlan_mode=l2" else "macvlan_mode=bridge")
                      cfg.network.name
                    ];
                  in
                  ''
                    if docker network inspect ${lib.escapeShellArg cfg.network.name} >/dev/null 2>&1; then
                      exit 0
                    fi
                    docker network create ${lib.escapeShellArgs args}
                  '';
              };

              airplay-bt-watch = {
                wantedBy = [ "multi-user.target" ];
                after = [
                  "bluetooth.service"
                  "airplay-network.service"
                ];
                requires = [ "bluetooth.service" ];
                path = [ pkgs.systemd ];
                serviceConfig = {
                  ExecStart = "${pythonEnv}/bin/python3 ${watcher}";
                  Restart = "on-failure";
                  RestartSec = 5;
                };
              };
            }

            (lib.mapAttrs' (
              name: _:
              lib.nameValuePair "docker-${containerName name}" {
                after = [ "airplay-network.service" ];
                requires = [ "airplay-network.service" ];
                # The docker daemon stays root -- ipvlan needs it -- but the
                # unit that drives it does not.
                serviceConfig = {
                  User = cfg.user;
                  Group = cfg.group;
                  SupplementaryGroups = [ "docker" ];
                };
              }
            ) cfg.devices)
          ];
        };
      };
  };
}
