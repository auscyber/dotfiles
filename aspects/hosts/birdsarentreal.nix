{
  den,
  inputs,
  lib,
  houseWifi,
  ...
}:
# HP Chromebook (coreboot board GOOGLE "setzer", Braswell Celeron N3060, 4G
# RAM, 32G eMMC) running MrChromebox UEFI firmware. Kodi appliance + AirPlay 2
# and Sendspin bridge for the bluetooth speakers.
let
  # ⚠ PLACEHOLDER -- CONFIRM OR CHANGE BEFORE DEPLOYING.
  #
  # The house lan is one flat 192.168.0.0/16 (../network/segments.nix) behind a
  # router at 192.168.0.1, with auspc on 192.168.1.11 and secondpc on
  # 192.168.0.26. Nothing in this repo records the router's DHCP pool, so this
  # repo cannot tell whether .1.12 is free -- the same unknown
  # ../services/airplay-bluetooth.nix's `ip` option describes as "must be free
  # and outside the DHCP pool". Pick a free address or pin a reservation.
  lanAddress = "192.168.1.12";
in
{
  den.hosts.x86_64-linux.birdsarentreal = {
    roles = [ "gui" ];

    # HOW THIS BOX IS ON THE HOUSE LAN. Static rather than a lease, for
    # ../hosts/fleet.nix's reason: a lease is not a fixed address, so a value
    # derived from one goes stale silently.
    #
    # THE INTERFACE IS THE RADIO, and that is honest rather than a compromise.
    # There is no ethernet on this chromebook, and ../network/house-wifi.nix's
    # `alsoVia` already says the radio is a second wire into this one segment;
    # auspc's own ⚠ calls `attachments.lan-home.interface = <radio>` the
    # "honest finish" for a host whose lan address lives on wifi.
    #
    # NOT `den.aspects.house-wifi`: that aspect drives the association through
    # NetworkManager, and this host uses `networking.wireless` below -- the two
    # cannot both own a radio. The association is configured here; the
    # ATTACHMENT is what the segment reads.
    attachments.lan-home = {
      address = lanAddress;
      interface = "wlp2s0";
    };

    # An appliance in a room of the household, so it needs the room/area
    # vocabulary as much as the service host does.
    # The household whose config this host produces: rooms, members, and who may
    # reach its services. See ../../base/household.nix.
    household = den.households.ivymectin;

    # Whose machine this is -- a different fact from the household it serves.
    owner = den.persons.ivy;
    users.ivy = {
      # The human behind this account (../../persons/ivy.nix). The link
      # points account -> person, never the reverse; ../../base/person.nix
      # says why.
      person = den.persons.ivy;
      roles = [ "gui" ];
    };
  };

  den.aspects.birdsarentreal = {
    layers = [ "nixos" ];

    includes = [
      den.aspects.nixos-hardware
      den.aspects.facter
      den.aspects.disko
      den.aspects.impermanence
      den.aspects.agenix-rekey
      den.aspects.tailscale
      # THE HOUSE LAN, as the include that makes membership true. Without it
      # the attachment above is data nothing reads: ../base/capability.nix
      # computes siblinghood by aspect name, so a host that does not include
      # this carrier is not on the segment however it is addressed.
      den.aspects.lan-home
      den.aspects.bluetooth
      den.aspects.kodi
      den.aspects.airplay-bluetooth
      # A SENDSPIN PLAYER for the same speakers, beside the AirPlay receivers
      # rather than instead of them: ../services/sendspin-bluetooth.nix dials
      # the Music Assistant on secondpc, which the house lan include above is
      # what makes resolvable over the wire rather than the tailnet.
      den.aspects.sendspin-bluetooth
      den.aspects.bt-console
    ];

    # "no-sendspin": the player, dropped. ../base/service-reach.nix derives a
    # `requires` from that aspect's household take, and ../tooling/nh-checked.nix
    # matches a failed aspect against what each specialisation excludes -- so
    # without this profile a Music Assistant that is off would REFUSE the build
    # rather than select a configuration that comes up without it. Same shape
    # as auspc's `no-turntable`, for the same reason.
    specialisations.no-sendspin.excludes = [ den.aspects.sendspin-bluetooth ];

    # The house wifi passphrase, RAW -- the same one secondpc's AP advertises
    # (../network/hostapd.nix), so it is one static secret and not a per-host
    # copy that can drift. The PATH is ../network/house-wifi.nix's too, so the
    # three declarations of this secret cannot come to name different files.
    secrets.wifi-password.rekeyFile = houseWifi.secretFile;

    # wpa_supplicant's `ext:` indirection reads a NAMED VARIABLE out of
    # `secretsFile`, so it needs `bees_psk=<passphrase>` rather than the bare
    # value -- which is why this used to be its own hand-entered
    # `bees_wifi.age` holding that whole line. It is a pure function of the
    # passphrase, so it is a template over it instead: one secret to type, and
    # hostapd (which wants the file to be JUST the passphrase) and
    # wpa_supplicant (which wants key=value) can no longer disagree.
    #
    # The dependency is in this aspect's OWN scope. A template depending on
    # another scope's secret closes `age.templates` ->
    # `age.scoped.<other>.access` -> `age.templates` -- see the note in
    # ../services/postgres.nix.
    templates."wpa.env" =
      { secrets, ... }:
      {
        restartUnits = [ "wpa_supplicant.service" ];
        dependencies = { inherit (secrets) wifi-password; };
        content =
          { placeholders, ... }:
          ''
            ${houseWifi.pskVar}=${placeholders.wifi-password}
          '';
      };

    # A module FUNCTION, not a bare attrset: as an attrset this is evaluated
    # while den collects class content, which forces the nixos-layer
    # `inputs.nixos-hardware` during darwin evaluations too. See
    # ./surfacelaptop.nix and `checks.layer-isolation`.
    nixos =
      {
        pkgs,
        scoped,
        ...
      }:
      {
        imports = [
          # No nixos-hardware module covers a Braswell chromebook -- its
          # `google-*` modules are Alder/Meteor Lake. These two are the generic
          # parts of what such a module would have given us.
          inputs.nixos-hardware.nixosModules.common-pc-laptop
          inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
        ];

        hardware.facter.reportPath = builtins.path {
          name = "birdsarentreal-facter";
          path = ./birdsarentreal.json;
        };

        networking.hostName = "birdsarentreal";

        # wpa_supplicant rather than NetworkManager: one appliance, one
        # network, and the PSK comes out of agenix instead of a GUI keyring.
        # `ext:` makes wpa_supplicant read the named variable out of
        # `secretsFile` at runtime, so no key reaches the store.
        networking.wireless = {
          enable = true;
          secretsFile = scoped.birdsarentreal.templates."wpa.env".path;
          # Recovery hatch for a wifi-only box: if the secret is ever missing
          # (no host key yet, failed rekey) `wpa_cli` can still add a network
          # from the console, and /var/lib/wpa_supplicant is persisted below.
          allowAuxiliaryImperativeNetworks = true;
          # Both APs share one PSK, so both point at the same variable. The
          # SSIDs come from ../network/house-wifi.nix rather than being spelled
          # out here -- they were a copy of what that file now declares, and a
          # third AP appearing is an edit there and nowhere else.
          networks = lib.genAttrs houseWifi.allSsids (_: {
            pskRaw = "ext:${houseWifi.pskVar}";
          });
        };

        # ── THE STATIC ADDRESS THE ATTACHMENT ABOVE DECLARES ──
        #
        # Scripted networking, which is what this host already has (nothing
        # here turns on networkd), and the same shape auspc and secondpc use.
        # ../hosts/fleet.nix's `attachment-interface-check` compares the
        # attachment against exactly this block, so the two cannot drift
        # without a warning.
        #
        # /16, not /24: the house really is one flat segment --
        # ../network/segments.nix has the measurement.
        networking.useDHCP = false;
        networking.interfaces.wlp2s0 = {
          useDHCP = false;
          ipv4.addresses = [
            {
              address = lanAddress;
              prefixLength = 16;
            }
          ];
        };
        networking.defaultGateway = {
          address = "192.168.0.1";
          interface = "wlp2s0";
        };
        networking.nameservers = [ "1.1.1.1" ];

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        # Battery/charger. MrChromebox's UEFI firmware exposes a normal ACPI
        # battery, so BAT0 comes from the ACPI drivers; the cros_ec pair is
        # what reaches the embedded controller itself (charge thresholds,
        # keyboard backlight, tablet switch) rather than the percentage.
        boot.kernelModules = [
          "cros_ec"
          "cros_ec_lpcs"
          "cros_charge-control"
        ];
        services.upower.enable = true;
        # common-pc-laptop turns TLP on; the two conflict.
        services.power-profiles-daemon.enable = false;

        # 4G of RAM, and no swap partition on a 32G eMMC.
        zramSwap.enable = true;

        # Braswell: i965 is the VAAPI driver for Gen8 Atom graphics, and kodi
        # cannot decode 1080p on an N3060 without it.
        hardware.graphics = {
          enable = true;
          extraPackages = with pkgs; [
            intel-vaapi-driver
            libvdpau-va-gl
          ];
        };
        environment.sessionVariables.LIBVA_DRIVER_NAME = "i965";

        # ONE audio graph for the whole box. Kodi, the airplay containers and
        # bt-console's combine-sink all have to see the same bluez sinks, and
        # bluez only hands its media endpoint to one pipewire instance -- so a
        # per-login session would race the service user for it.
        services.pipewire.systemWide = true;

        # One receiver per speaker. Each container needs its own LAN address
        # because two AirPlay 2 receivers cannot share one -- see
        # ../services/airplay-bluetooth.nix. `parent` is the wifi interface
        # from the facter report, which is why the driver stays ipvlan.
        services.airplayBluetooth = {
          enable = true;
          network = {
            parent = "wlp2s0";
            subnet = "192.168.1.0/24";
            gateway = "192.168.1.1";
          };
          # Fill in once the speakers are paired -- `bt-console` lists their
          # addresses, and each needs a free IP outside the DHCP pool.
          devices = { };
        };

        # The Sendspin player. No url and no port here: the aspect resolves the
        # Music Assistant on secondpc and reads its port off the same link.
        #
        # NO `name`, so it defaults to the hostname. The rooms in
        # ../households/ivymectin.nix deliberately do not line up with the wifi
        # SSIDs and nothing records which of them this appliance sits in, so a
        # friendlier name here would be invented rather than read.
        #
        # `sink` stays null until the speakers are paired -- `bt-console` lists
        # their nodes, and its combine-sink is the right target when they play
        # as one.
        services.sendspinBluetooth.enable = true;

        services.btConsole = {
          enable = true;
          openFirewall = true;
        };

        users.users.ivy = {
          isNormalUser = true;
          uid = 1000;
          description = "Ivy";
          extraGroups = [
            "wheel"
            "video"
            "audio"
            "pipewire"
            "input"
            "networkmanager"
            "docker"
          ];
        };

        # Root is a tmpfs, so everything that must survive a reboot is listed.
        # /var/lib/bluetooth is the important one: without it every speaker
        # has to be re-paired.
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
          options = [
            "defaults"
            "size=2G"
            "mode=755"
          ];
        };
        fileSystems."/persist".neededForBoot = true;

        environment.persistence."/persist" = {
          hideMounts = true;
          directories = [
            "/var/log"
            "/var/lib/nixos"
            "/var/lib/bluetooth"
            "/var/lib/docker"
            "/var/lib/upower"
            # The Sendspin client's cryptographic identity and pairing
            # records. Losing it makes the box a brand-new device to the
            # server and needs the PIN typed in again.
            "/var/lib/sendspin"
            "/var/lib/systemd/coredump"
            "/var/lib/tailscale"
            "/etc/NetworkManager/system-connections"
          ];
          files = [
            "/etc/machine-id"
            "/etc/ssh/ssh_host_ed25519_key"
            "/etc/ssh/ssh_host_ed25519_key.pub"
            "/etc/ssh/ssh_host_rsa_key"
            "/etc/ssh/ssh_host_rsa_key.pub"
          ];
          users.ivy.directories = [
            ".ssh"
            ".local/share/kodi"
          ];
        };

        # eMMC, by-id rather than /dev/mmcblk0 which is not stable. btrfs for
        # the zstd compression -- 32G does not go far with a nix store on it.
        disko.devices.disk.emmc = {
          type = "disk";
          device = "/dev/disk/by-id/mmc-HBG4a2_0x56d63d7d";
          content = {
            type = "gpt";
            partitions = {
              ESP = {
                size = "512M";
                type = "EF00";
                content = {
                  type = "filesystem";
                  format = "vfat";
                  mountpoint = "/boot";
                  mountOptions = [ "umask=0077" ];
                };
              };
              root = {
                size = "100%";
                content = {
                  type = "btrfs";
                  extraArgs = [ "-f" ];
                  subvolumes = {
                    "/nix" = {
                      mountpoint = "/nix";
                      mountOptions = [
                        "compress=zstd"
                        "noatime"
                      ];
                    };
                    "/persist" = {
                      mountpoint = "/persist";
                      mountOptions = [
                        "compress=zstd"
                        "noatime"
                      ];
                    };
                  };
                };
              };
            };
          };
        };
      };
  };

  den.aspects.ivy.provides.birdsarentreal = {
    includes = [
      den.aspects.fish
      den.aspects.neovim
      den.aspects.gpg
      den.batteries.primary-user
    ];
  };
}
