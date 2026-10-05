{
  den,
  lib,
  capability,
  ...
}:
# The power supply's own telemetry, published to the house MQTT broker as a
# Home Assistant power sensor.
#
# ── WHAT THE HARDWARE ACTUALLY REPORTS, BECAUSE MOST PSUs REPORT NOTHING ──
#
# A PSU is mute unless it has a microcontroller and a USB cable to the
# motherboard. ../hosts/auspc/facter.json has one: alongside the H100i cooler
# (`1b1c:0c19`) there is a second Corsair device at
#
#   vendor 1b1c (Corsair Inc.), device 1c08, driver usbhid
#
# and `1b1c:1c08` is in `corsairpsu_idtable` in drivers/hwmon/corsair-psu.c as
# "Corsair HX1200i Legacy" (the 2022 series; the 2023 and 2025 revisions are
# `1c23` and `1c27`). So this box has a digital HXi unit and the kernel has a
# driver for it. The facter report shows `usbhid` rather than `corsair-psu`
# because the report was taken with the module not loaded -- `hid-generic`
# claims any HID device nothing more specific has asked for -- which is what
# `boot.kernelModules` below fixes.
#
# ── AND WHAT `power1_input` IS, WHICH IS NOT WALL POWER ──
#
# Documentation/hwmon/corsair-psu.rst calls `power1_input` "Total power usage",
# which is ambiguous, and the ambiguity matters: a power entity with the wrong
# label is worse than no power entity. The PSU's `0xEE` register is the one
# liquidctl reads for `Total power output` (`_CORSAIR_READ_OUTPUT_POWER` in
# liquidctl/driver/corsair_hid_psu.py), and liquidctl then DERIVES `Estimated
# input power` from it with a pair of efficiency polynomials interpolated on the
# measured AC voltage. So:
#
#   * `power1_input` is the DC power the unit delivers to the machine, the sum
#     of `power2..4_input` (+12V, +5V, +3.3V);
#   * the AC draw at the wall is higher by the conversion loss -- an HX1200i is
#     80 PLUS Platinum, so roughly 4-8% at these loads;
#   * the driver exposes `in0_input` (AC volts) but NO AC watts channel, and
#     this aspect does not reproduce liquidctl's efficiency curve. Guessing a
#     wall figure from a polynomial and a nameplate rating is exactly the wrong
#     label on a power entity.
#
# Hence the sensor is called "PSU output power" and nothing here claims to
# measure what the house meter measures. ../services/homeassistant/prom-device.nix's
# `CPU package power` has the same shape of honesty for RAPL.
#
# ONE CHANNEL, not all four. The ask was the PSU's power consumption, and the
# rails, the two temperatures, the fan and the AC voltage are each one more row
# a minute into HA's recorder for a number nobody reads. They are all in the
# same hwmon directory the script below already locates, so adding one is a
# line, not a redesign.
#
# ── WHY MQTT DISCOVERY AND NOT ANY OF THE OTHER ROUTES ──
#
# The reading is on auspc and Home Assistant is on secondpc, so something has
# to cross the house either way. The alternatives:
#
#   * node_exporter's hwmon collector already exports this as
#     `node_hwmon_power_watt`, and ./homeassistant/prom-device.nix would turn it
#     into an entity with one more query. That is genuinely less machinery --
#     no broker account, no timer, no credential -- and it is deliberately NOT
#     done, because then there would be two entities for one number and no way
#     to tell which a dashboard was pinned to. If the MQTT path is ever dropped,
#     that query is the replacement.
#   * a `rest` or `command_line` sensor in HA polling something on auspc needs a
#     listener here, and therefore a firewall hole, for a value that is pushed
#     perfectly well.
#
# Discovery, specifically, is why this aspect contributes almost nothing to Home
# Assistant's own config: a retained `homeassistant/sensor/.../config` message
# IS the entity definition, so the sensor appears without secondpc's
# configuration knowing that auspc has a power supply. The only HA-side
# requirement is that it speaks MQTT at all, which ./homeassistant/mosquitto.nix
# already contributes -- the declaration below is a union member, not a second
# copy.
#
# NO `gives`, SO NO FIREWALL HOLE. This is an outbound client; ../base/firewall.nix
# derives holes from `gives.<cap>.ports.<n>.expose` and there is nothing to
# derive. That is also why the "expose on both `lan-home` and `house-wifi`"
# reasoning in ./openrgb.nix does not apply here: auspc's `lan-home` attachment
# names a NIC with no cable in it, which breaks INBOUND rules written for that
# wire and is irrelevant to a connection this box opens itself -- the kernel
# picks the route, and the broker's own hole is on secondpc's wire where the
# packets arrive.
let
  # ── ONE NAME FOR THE ASPECT, THE SCOPE AND THE BROKER ACCOUNT ──
  #
  # ./homeassistant/mosquitto.nix names the account after the TAKING ASPECT
  # unless the take states a `username`, and ../security/age-scope.nix names the
  # secret scope after the aspect too. So this string is the aspect, the
  # `/run/agenix/<scope>/` directory and the MQTT login all at once, and writing
  # it three times is three chances for them to disagree.
  name = "corsair-psu";

  # The hwmon driver, and the `name` it registers its device under. The script
  # searches for the second rather than trusting a `hwmonN` number: hwmon
  # indices are assigned in probe order, so `hwmon3` is the PSU until a kernel
  # upgrade reorders the probes and then it is the GPU.
  kernelModule = "corsair-psu";
  chip = "corsairpsu";

  # Home Assistant's MQTT discovery prefix -- `mqtt.discovery_prefix`, whose
  # default this is. Named because the broker ACL and the topic below both have
  # to agree with whatever HA is actually listening to.
  discoveryPrefix = "homeassistant";

  # How often the timer fires. 30s is the same order as Prometheus's own scrape
  # of this box and well under HA's 60s statistics bucket; the PSU's own
  # averaging window is longer than either.
  intervalSeconds = 30;
in
{
  den.aspects.${name} = {
    # ── THE BROKER ACCOUNT, MINTED FROM THIS TAKE ──
    #
    # ./homeassistant/mosquitto.nix derives its whole user list from
    # `takes.mqtt`, so this one line is the login, the password, the hash the
    # broker checks it against and the ACL. No `username`: the default is the
    # aspect name, which is what `name` above already is.
    #
    # The ACL is narrowed from the capability's `readwrite #` default. A
    # publisher that only ever writes two topics has no business with a
    # subscribe on everything, and this is the first taker for which that is
    # actually true -- Home Assistant and Music Assistant both need the wildcard.
    takes.mqtt.acl = [
      "write ${discoveryPrefix}/sensor/#"
      "write sensors/#"
    ];

    # ── AND THE INTEGRATION, THROUGH THE EDGE ──
    #
    # `mqtt`, which ./homeassistant/mosquitto.nix also contributes. That is not
    # a duplicate definition: the capability's `extraComponents` merges as a
    # UNION (./homeassistant/core.nix), so the component appears once however
    # many takers ask for it. Stated here anyway because it is this aspect's
    # own requirement -- it is unpublishable without it -- and because a house
    # that moved to an external broker would drop mosquitto's contribution
    # while this one still holds.
    #
    # NOTHING ELSE. `takes.home-assistant.config` is where an aspect pushes YAML
    # and there is deliberately no entry: the retained discovery message is the
    # entity definition, so a hand-written `mqtt: sensor:` block here would be
    # the same sensor declared twice, in two places, on two boxes.
    #
    # This reaches Home Assistant at all only because core.nix folds with
    # `capability.contributionsReaching` -- see ./openrgb.nix, which is the
    # taker that found that gap.
    takes.home-assistant.extraComponents = [ "mqtt" ];

    # ── THE CREDENTIAL, PINNED TO THE BROKER'S COPY OF THE SAME FILE ──
    #
    # This is the one genuinely awkward part, and it is awkward because it is the
    # first mqtt taker that is not on the broker's box.
    #
    # mosquitto mints `${name}-password` and `${name}-hash` in ITS scope, on ITS
    # host -- `secrets/generated/<brokerHost>/mosquitto/`. The broker needs the
    # hash; this machine needs the plaintext; and they must be two rekeyings of
    # ONE encrypted value or the login does not work. agenix-rekey's unit of
    # identity for that is the `rekeyFile`: two hosts declaring the same file
    # with the same generator get the same plaintext, each encrypted to its own
    # key. ../base/celler/_lib.nix leans on exactly this for a signing key held
    # by a server and read by its clients, and says so.
    #
    # So the path is stated, and `brokerHost` is ASKED FOR rather than written:
    # it is whichever host includes the broker, so moving mosquitto moves this
    # file with it instead of leaving a dangling path to a machine that no
    # longer has one.
    secrets =
      let
        # Every host in the fleet, flattened out of the per-system attrset --
        # the same `concatMapAttrs` ../base/capability.nix's own `hostsOf` does.
        # `hasAspect` is a PROJECTED lookup (see ../../CLAUDE.md), so this reads
        # the inclusion graph and not any host's configuration fixpoint.
        hosts = lib.attrValues (lib.concatMapAttrs (_: hs: hs) den.hosts);
        brokers = lib.filter (h: h.hasAspect den.aspects.mosquitto) hosts;

        # BOTH FAILURES NAMED, because they are different mistakes with the
        # same symptom -- an `.age` file nothing generates. Zero brokers is a
        # missing include; two is a house with two brokers, where "the"
        # credential is not a thing and whoever adds the second has to decide
        # which one this publisher authenticates to.
        brokerHost =
          if brokers == [ ] then
            throw "${name}: no host in this flake includes den.aspects.mosquitto, so there is no broker whose credential this could share. Include ../services/homeassistant/mosquitto.nix somewhere."
          else if lib.length brokers > 1 then
            throw "${name}: ${toString (lib.length brokers)} hosts include den.aspects.mosquitto (${
              lib.concatMapStringsSep ", " (h: h.hostName) brokers
            }), so there is no single broker scope to share a credential with. Name the one this publishes to."
          else
            (lib.head brokers).hostName;
      in
      {
        mqtt-password = {
          # THE BROKER'S FILE, not a second secret that happens to be a
          # password. `mosquitto.nix` declares this same path implicitly (its
          # scope is `mosquitto`, its key is `<taker>-password`) with this same
          # generator, and agenix-rekey dedups the two definitions by file.
          rekeyFile = ../../secrets/generated + "/${brokerHost}/mosquitto/${name}-password.age";
          generator.script = "random-base64-32";
        };
      };

    # ── THE PASSWORD REACHES mosquitto_pub AS A CONFIG FILE, NOT AS ARGV ──
    #
    # `mosquitto_pub -P <password>` puts the secret in `/proc/<pid>/cmdline`,
    # which is world-readable, for as long as the process lives. mosquitto_pub
    # reads default options from `$XDG_CONFIG_HOME/mosquitto_pub`, one
    # `-option value` per line, so the credential can be a FILE the process
    # opens instead of a string every process on the box can read.
    #
    # A template rather than something the unit assembles at runtime: this way
    # the rendered file exists exactly once, root-owned at 0400, and the
    # plaintext never passes through a shell.
    #
    # The BASENAME is what mosquitto_pub cares about, and the agenix path is
    # not it -- MEASURED, this lands at
    # `/run/agenix/templates/corsair-psu/mosquitto_pub`, with the scope's `/`
    # separator (../../lib/age-scoped.nix) making the middle component. What
    # makes `XDG_CONFIG_HOME` below work is the `LoadCredential` ID, which is
    # the name systemd gives the copy in the credentials directory, and which
    # is `mosquitto_pub` because that is what this template is called. Keeping
    # the two spellings the same is the whole trick: no symlink, no copy, no
    # rename at runtime.
    #
    # Templates implicitly depend on every secret in their own scope, so
    # `mqtt-password` needs no `dependencies` entry.
    templates.mosquitto_pub = {
      content =
        { placeholders, ... }:
        ''
          -u ${name}
          -P ${placeholders.mqtt-password}
        '';
    };

    nixos =
      {
        pkgs,
        host,
        scoped,
        ...
      }:
      let
        # ── THE BROKER, RESOLVED ──
        #
        # `dial`, not `linkTo`: an `mqtt` link carries `port` but gets its
        # `address` from the carrier it is composed with, and only `dial`
        # composes. ./homeassistant/mosquitto.nix's own note explains why
        # forcing a whole `mqtt` link does not work.
        broker = capability.dial "mqtt" { } host;

        # `broker.link.port`, not `broker.port`. `dial` derives the latter from
        # a `ports` attrset, which an `mqtt` giver does not furnish -- it
        # furnishes the capability's own single `port`.
        brokerPort = toString broker.link.port;

        node = host.hostName;

        # `homeassistant/sensor/<node>/<object>/config`, the two-level discovery
        # topic. The node id groups every sensor this box ever publishes under
        # one subtree, which is what makes a stale entity findable later.
        objectId = "psu_output_power";
        configTopic = "${discoveryPrefix}/sensor/${node}/${objectId}/config";
        stateTopic = "sensors/${node}/psu/power";

        # ── THE SCRIPT AS A STRING, NOT A `writeShellApplication` ──
        #
        # The wrapper would get shellcheck, and it would also put the whole
        # body behind a store path that `nix eval` cannot show: the config
        # would carry an `ExecStart` and nothing else, so "what does this
        # actually publish" could only be answered by building it on an
        # x86_64-linux machine. `systemd.services.<n>.script` is a plain string
        # in the rendered config, so the resolved broker address, the topics
        # and the discovery payload are all readable from an eval -- which is
        # the loop ../../CLAUDE.md asks for. ./homeassistant/prom-device.nix's
        # own unit is written the same way.
        publishScript = ''
          # The PSU's hwmon directory, found by the driver's registered chip
          # name. See `chip` above for why not a hwmonN literal.
          dir=""
          for d in /sys/class/hwmon/hwmon*; do
            if [ "$(cat "$d/name" 2>/dev/null)" = ${lib.escapeShellArg chip} ]; then
              dir="$d"
              break
            fi
          done

          if [ -z "$dir" ] || [ ! -r "$dir/power1_input" ]; then
            # NO AVAILABILITY MESSAGE, and no failure either. The sensor's
            # `expire_after` below is what turns silence into "unavailable"
            # in Home Assistant, so a PSU that is unplugged, a module that
            # did not load, or a kernel that dropped the driver all arrive as
            # the same unavailable entity rather than as a unit in a restart
            # loop. Stderr is what says which.
            echo "no hwmon device named ${chip}: the ${kernelModule} module is not bound" >&2
            exit 0
          fi

          # hwmon power channels are MICROWATTS. Divided with `jq` rather
          # than with `$(( ))`, which is integer-only and would round a 220 W
          # draw to 220 before the decimal could say otherwise.
          uw=$(cat "$dir/power1_input")
          watts=$(jq -n --argjson uw "$uw" '(($uw / 1000000) * 10 | round) / 10')

          # ── THE MODEL, READ OFF THE DEVICE RATHER THAN TYPED ──
          #
          # The hwmon device's `device` link is the HID device; the USB
          # product string ("HX1200i") is two levels above it, on the usb
          # device rather than the interface. Walked rather than spelled out
          # as `device/../../product`, because that depth is a fact about one
          # USB topology. Falls back to the chip name, which is always true.
          model=${lib.escapeShellArg chip}
          p="$dir/device"
          for _ in 1 2 3 4; do
            p="$p/.."
            if [ -r "$p/product" ]; then
              model=$(cat "$p/product")
              break
            fi
          done

          # RETAINED, EVERY RUN. The discovery message is the entity
          # definition, so republishing it is how a broker that lost its
          # persistence, or a Home Assistant that was reinstalled, gets the
          # sensor back without anything on secondpc knowing it was gone.
          # `jq -n` builds it so that a model string with a quote in it
          # cannot produce invalid JSON.
          jq -nc \
            --arg node ${lib.escapeShellArg node} \
            --arg object ${lib.escapeShellArg objectId} \
            --arg state ${lib.escapeShellArg stateTopic} \
            --arg model "$model" \
            '{
               name: "PSU output power",
               unique_id: ($node + "_" + $object),
               object_id: ($node + "_" + $object),
               state_topic: $state,
               device_class: "power",
               state_class: "measurement",
               unit_of_measurement: "W",
               suggested_display_precision: 1,
               expire_after: ${toString (3 * intervalSeconds)},
               device: {
                 identifiers: [ $node + "_psu" ],
                 name: ($node + " PSU"),
                 manufacturer: "Corsair",
                 model: $model
               }
             }' \
            | mosquitto_pub -h ${lib.escapeShellArg broker.address} -p ${brokerPort} \
                -t ${lib.escapeShellArg configTopic} -r -s

          # NOT RETAINED. A retained reading outlives the machine that took
          # it, so Home Assistant would restore a power figure for a box that
          # is switched off. `expire_after` is the mechanism that is supposed
          # to answer that, and a retained state defeats it.
          mosquitto_pub -h ${lib.escapeShellArg broker.address} -p ${brokerPort} \
            -t ${lib.escapeShellArg stateTopic} -m "$watts"
        '';
      in
      {
        # Binds `corsair-psu` to the HID device in place of `hid-generic`, which
        # is what creates the hwmon node at all. If the running kernel was built
        # without `CONFIG_SENSORS_CORSAIR_PSU` this fails LOUDLY at activation,
        # which is the right outcome: the alternative is a timer that logs "no
        # hwmon device" forever.
        boot.kernelModules = [ kernelModule ];

        # `lm_sensors` so that the same reading is available by hand on the box
        # -- `sensors corsairpsu-hid-*` -- without deriving it from a sysfs path
        # out of a comment.
        environment.systemPackages = [ pkgs.lm_sensors ];

        systemd.services.psu-power-mqtt = {
          description = "Publish the PSU's output power to MQTT for Home Assistant";

          # `set -euo pipefail` is not systemd's default and matters here: the
          # discovery message is piped into mosquitto_pub, so without pipefail
          # a broker that refuses the connection is an exit 0.
          script = ''
            set -euo pipefail
          ''
          + publishScript;

          path = with pkgs; [
            coreutils
            jq
            mosquitto
          ];

          serviceConfig = {
            Type = "oneshot";

            # ── THE CREDENTIAL, HANDED OVER BY PID 1 ──
            #
            # `LoadCredential` reads the template as root and drops a copy into
            # a private tmpfs readable only by this unit's user, which is what
            # makes `DynamicUser` possible at all: a dynamic uid cannot be the
            # `owner` of an agenix file, because the name does not exist at
            # evaluation time.
            #
            # The credential ID is the filename mosquitto_pub looks for, so
            # `XDG_CONFIG_HOME` can be the credentials directory itself.
            LoadCredential = "mosquitto_pub:${scoped.${name}.templates.mosquitto_pub.path}";
            Environment = [ "XDG_CONFIG_HOME=%d" ];

            DynamicUser = true;

            # Reads one sysfs attribute and opens one TCP connection. Nothing
            # here needs a writable filesystem, a device node or a capability.
            ProtectSystem = "strict";
            ProtectHome = true;
            PrivateDevices = true;
            NoNewPrivileges = true;
            CapabilityBoundingSet = "";
            RestrictAddressFamilies = [
              # AF_UNIX as well as the two inet families: the journal and the
              # credentials machinery are unix sockets, and a unit that cannot
              # open one fails in a way that looks like a network problem.
              "AF_UNIX"
              # AF_NETLINK, which is the one that is not obvious. glibc's
              # `getaddrinfo` opens a netlink socket to find out whether the
              # host has usable IPv6 before it returns -- even for a numeric
              # address, which is all this unit ever passes. Blocking it makes
              # mosquitto_pub fail to resolve a literal IP, which reads as a
              # broker problem rather than as a sandbox one.
              "AF_NETLINK"
              "AF_INET"
              "AF_INET6"
            ];
          };
        };

        systemd.timers.psu-power-mqtt = {
          description = "Publish the PSU's output power to MQTT for Home Assistant";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            # From boot rather than from a wall clock: the reading is a rate,
            # not an appointment, and every box publishing on the minute is a
            # thundering herd at the broker for no benefit.
            OnBootSec = "1min";
            OnUnitActiveSec = "${toString intervalSeconds}s";
            AccuracySec = "5s";
          };
        };
      };
  };
}
