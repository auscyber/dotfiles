{
  den,
  inputs,
  ...
}:
# HP Chromebook (coreboot board GOOGLE "setzer", Braswell Celeron N3060, 4G
# RAM, 32G eMMC) running MrChromebox UEFI firmware. Kodi appliance + AirPlay 2
# bridge for the bluetooth speakers.
{
  den.hosts.x86_64-linux.birdsarentreal = {
    roles = [ "gui" ];
    users.ivy.roles = [ "gui" ];
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
      den.aspects.bluetooth
      den.aspects.kodi
      den.aspects.airplay-bluetooth
      den.aspects.bt-console
    ];

    # The wifi PSK. Create it before the first build with
    #   nix run .#secret-edit -- secrets/bees_wifi.age
    # one line, no quotes:  bees_psk=<passphrase>
    secrets.bees-wifi = {
      rekeyFile = ../../secrets/bees_wifi.age;
      restartUnits = [ "wpa_supplicant.service" ];
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
          secretsFile = scoped.birdsarentreal.secrets.bees-wifi.path;
          # Recovery hatch for a wifi-only box: if the secret is ever missing
          # (no host key yet, failed rekey) `wpa_cli` can still add a network
          # from the console, and /var/lib/wpa_supplicant is persisted below.
          allowAuxiliaryImperativeNetworks = true;
          # Both APs share one PSK, so both point at the same variable.
          networks = {
            "Bedroom Beehive".pskRaw = "ext:bees_psk";
            "Hallway Beehive".pskRaw = "ext:bees_psk";
          };
        };

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
