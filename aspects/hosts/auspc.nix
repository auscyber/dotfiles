{
  den,
  __findFile,
  ...
}:
{
  den.hosts.x86_64-linux.auspc = {
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFYM1mZ3fYfOjhyMhIiKbUOLYTQifG82P2NnGWHyIwHt root@nixos";
    gpu = "nvidia";
    roles = [
      "gui"
      "gaming"
      "dev"
    ];

    # Advertised to every host that includes `den.aspects.builders`. auspc has 4
    # cores reserved for builds and is the fastest box.
    builder = {
      ipAddress = "10.100.0.2";
      systems = [ "x86_64-linux" ];
      maxJobs = 10;
      speedFactor = 20;
      features = [
        "big-parallel"
        "cached-compilation"
        "kvm"
      ];
      sshUser = "builder";
    };
    # The household whose config this host produces: rooms, members, and who may
    # reach its services. See ../../base/household.nix.
    household = den.households.ivymectin;

    # Reachable on the LAN here rather than through the wireguard tunnel.
    #
    # Declared on the ENTITY, not in this host's config, because `fleet`
    # (../hosts/fleet.nix) and ../network/segments.nix's `lan-home` giver read
    # it fleet-wide at evaluation time -- reading it out of another host's
    # config would force that host's whole configuration to answer "what is its
    # LAN address". It is also the address the VBAN stream resolves OVER: the
    # segment furnishes `mtu = 1500`, which is what lets that take exclude the
    # 1420-byte tunnel by comparison rather than by name.
    # MEMBERSHIP of the other transports is not declared anywhere: it derives
    # from `hasAspect` (see ../network/transports.nix).
    #
    # Static, not a DHCP lease: a lease is not a fixed address, so a value
    # derived from one would go stale silently and hand out a WRONG address
    # instead of throwing. See the `networking.interfaces` note below.
    attachments.lan-home = {
      address = "192.168.1.11";
      # THE RADIO, because that is where this address actually is. `ip -br addr`
      # on the box reports `192.168.1.11/16` on `wlo1`; there is no cable in
      # this machine right now. ../base/firewall.nix opens a lan-exposed port
      # on the attachment's interface, so naming the cable wrote inbound rules
      # for a wire carrying nothing -- which is what the `expose = [ "lan-home"
      # "house-wifi" ]` workarounds elsewhere were compensating for.
      interface = "wlo1";
    };

    # THE OTHER WIRE INTO THE SAME HOUSE LAN: the radio
    # (../network/house-wifi.nix, included below). Its own key rather than a
    # second definition of `lan-home` above, because the NIC name is different
    # and this box holds both at once -- one key could not answer for both.
    #
    # `wlo1` is from ./auspc/facter.json: an `iwlwifi` interface on
    # 0000:00:14.3, the only radio the report lists.
    #
    # NO `address`, and that is the honest entry rather than a hole. The wifi
    # side takes a DHCP lease (`ipv4.method = "auto"` in the profile that aspect
    # writes), and ../hosts/fleet.nix's own `attachments` note says a lease must
    # not be written here: "a lease is not a fixed address, so a value derived
    # from one would go stale silently". So the carrier furnishes a null address
    # and nothing INBOUND resolves over the radio -- outbound, which is what
    # this include was asked for, does not read it. Pinning a reservation on the
    # router and declaring it here is what would change that.
    attachments.house-wifi.interface = "wlo1";

    # NO `runs.lan-home.address`: it was this same value again, and
    # ../network/segments.nix's `lan-home` giver reads `lanAddress` directly.
    #
    # Still worth knowing: auspc is on 192.168.1.x while secondpc is on
    # 192.168.0.x, so whether these are SIBLINGS on one segment depends entirely
    # on `lan-home`'s prefix being a /16 -- CONFIRMED on the box, where
    # `ip -br addr` reports `192.168.1.11/16`. Had the house been a /24 these
    # would be two segments with no lan link between them, which
    # `capability.linkBetween` reports as an empty intersection rather than
    # silently handing out an unroutable address.
    #
    # THE ADDRESS IS ON THE RADIO, and `attachments.lan-home` now says so. This
    # note used to pose the choice -- cable in, or `lan-home` on the radio --
    # and the radio is the answer: there is no ethernet in this machine.
    #
    # The ASSIGNMENT follows: ../network/house-wifi.nix reads this attachment
    # and gives its NM profile `method = "manual"` with this address, so the
    # declaration and the wire now agree. ⚠ PRECONDITION: `192.168.1.11` must
    # be outside the router's DHCP pool or reserved for this host -- it was a
    # lease until now, and nothing in this repo records the pool.

    # A PULL OF THE FLEET'S CACHES. Including the aspect is the whole
    # declaration: ../services/celler-service.nix resolves, for whichever host
    # is asking, an http link to every cache the fleet serves -- over loopback
    # if it is the same box, the lan if they are cabled together, the tailnet
    # otherwise. This host names no cache, no server and no carrier.
    #
    # Was `runs.celler-cache-pull.uses.celler-cache.identifying = true`, which
    # asked the registry to spawn an instance here and to stamp which cache it
    # was a pull OF. The spawn is what failed -- `attribute
    # 'celler-cache-pull' missing` -- and inclusion needs no spawn.
    includes = [ den.aspects.celler-cache-pull ];

    # Whose machine this is -- a different fact from the household it serves.
    owner = den.persons.ivy;

    users.auscyber = {
      # The human behind this account (../../persons/ivy.nix). The link
      # points account -> person, never the reverse; ../../base/person.nix says why.
      person = den.persons.ivy;
      wallpaper = builtins.path {
        name = "wallpaper";
        path = ../../backgrounds/phoebebridgers-2.jpg;
      };
      roles = [
        "gui"
        "gaming"
        "dev"
      ];
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMOJWDxkmOeVGAq7WcPI+BygJ2zbsn4J0UAq0R6B6ZVx auscyber@auspc";
    };
  };
  den.aspects.qemu = {
    provides.to-users.homeManager = { pkgs, ... }: {
      home.packages = with pkgs; [ virt-manager ];
    };

    nixos = {
      virtualisation.libvirtd.enable = true;

      # Enable TPM emulation (optional)
      # install pkgs.swtpm system-wide for use in virt-manager (optional)
      virtualisation.libvirtd.qemu = {
        swtpm.enable = true;
      };

      # Enable USB redirection (optional)
      virtualisation.spiceUSBRedirection.enable = true;
      users.users.auscyber.extraGroups = [ "libvirtd" ];
    };
  };
  den.aspects.auspc = {
    includes = [
      den.aspects.qemu
      den.aspects.vpn
      # On the tailnet as well as the wireguard tunnel. Needed for the laptop to
      # reach this box as a builder from off the LAN -- the wireguard hub is
      # secondpc, so a client that cannot reach its fixed endpoint has nothing,
      # whereas tailscale negotiates directly and falls back to a DERP relay
      # (../network/tailscale.nix's own reasoning). Membership of the tailscale
      # transport IS this include; nothing declares it separately.
      den.aspects.tailscale
      # On the house LAN: ../../network/segments.nix. Inclusion IS the
      # attachment, and the address comes from this host's `lanAddress`.
      den.aspects.lan-home
      # The house wifi, as a CARRIER (../network/house-wifi.nix): the include
      # both joins the network and furnishes the link joining it creates, so
      # this box names no SSID, no passphrase and no second segment.
      #
      # ALONGSIDE `lan-home`, not instead of it. This machine is cabled AND
      # associated, so it declares both attachments above and both includes
      # here -- and because `house-wifi` sits at `preference = 25` against the
      # cable's 20, every existing consumer keeps resolving over the cable and
      # falls to the radio only where there is no cable to use.
      #
      # A SECOND ROUTE TO THE INTERNET rather than a replacement, at the
      # kernel's level too: the static wired default route below carries metric
      # 0 and the wifi profile asks for 700, so wifi carries traffic when the
      # cable does not.
      den.aspects.house-wifi
      den.aspects.packages.alx-wol
      den.aspects.cachyos-kernel
      # Inert until `nix run .#gen-kernel-trim` (run on auspc) writes
      # ./auspc/kernel-trim.json; then it rides along with the cachyos kernel
      # above as one more `boot.kernelPatches` entry.
      #      den.aspects.kernel-trim
      den.aspects.bootlogo
      # Turntable on the onboard Realtek line-in, sent to the household's
      # Music Assistant over VBAN. The receiver is RESOLVED, not named: the
      # stream declares `takes.udp = { name = "music-assistant"; mtu >= 1500; }`
      # and ../services/vban.nix dials it, so moving that service to another
      # machine moves the stream with it.
      den.aspects.vban-turntable
      # THE SOUND CARD HALF, included DIRECTLY as well as through
      # `vban-turntable` above (which includes it too). That is not redundant:
      # `specialisations.no-turntable` below excludes `vban-turntable`, and
      # den's `excludes` is a by-name constraint over the subtree -- so an
      # aspect reached only through the excluded one would go with it. Named
      # here, the jack retasking and the capture node survive into the
      # no-turntable profile, which is what should happen: the receiver being
      # off is not a reason for the hardware to be configured differently.
      den.aspects.turntable-input
      # The RGB hardware in this box, offered to the house as something Home
      # Assistant can switch (../services/openrgb.nix). `reach = "household"`
      # on that aspect is what lets HA resolve it from secondpc; the integration
      # itself is contributed BY that aspect, so this one include is the whole
      # wiring and nothing is named in Home Assistant's own file.
      den.aspects.openrgb
      # The POWER SUPPLY in this box, which is a digital Corsair HXi and so
      # reports what it is delivering (../services/corsair-psu.nix -- read its
      # header for which USB id in ./auspc/facter.json says so, and for why the
      # sensor is called OUTPUT power rather than wall power). Resolution goes
      # the other way round from openrgb above: this box DIALS the house broker
      # rather than being dialled, so nothing is exposed here and the one thing
      # that had to change elsewhere was mosquitto's `reach`.
      den.aspects.corsair-psu
      #      den.aspects.builders
      den.aspects.builder-server
      den.aspects.secure-boot
      (den.batteries.unfree [ "castlabs-electron" ])
      den.aspects.disko
      den.aspects.facter
      # gaming content is delivered via the `gaming` role (see gaming.nix +
      # roles.nix); no explicit include needed.
    ];

    # "no-turntable": come up without the VBAN stream, for when the receiver
    # is not there. ../base/service-reach.nix derives the check from
    # `vban-turntable`'s own take -- Music Assistant answering on the LAN --
    # and ../tooling/nh-checked.nix picks this profile because it excludes the
    # aspect that check belongs to.
    #
    # WITHOUT THIS THE BUILD IS REFUSED, which is the wrapper working as
    # designed rather than a wrinkle: a failing check with no profile to cover
    # it names what failed and stops, instead of activating a configuration
    # with a known-broken aspect in it. A stream to a machine that is off is
    # exactly that -- pipewire runs the sender, nothing receives, and nothing
    # says so.
    specialisations.no-turntable.excludes = [ den.aspects.vban-turntable ];

    # "no-celler": no celler caches at all, for when they are down -- the same
    # profile ../hosts/laptop.nix carries, and for the same reason. Without it
    # this box had a `requires` (`celler-host`, ../base/celler/push.nix) that
    # nothing could cover, so a dead celler REFUSED the build rather than
    # selecting a configuration that comes up without it.
    #
    # `celler-user` as well as `celler-host`: the host half is the substituters
    # and the push hook, the user half is the client config, and a dead server
    # takes both.
    specialisations.no-celler.excludes = [
      den.aspects.celler-host
      den.aspects.celler-user
    ];

    provides.to-users.homeManager = {
      stylix.targets.gnome.enable = false;
    };
    nixos =
      {
        pkgs,
        config,
        host,
        ...
      }:
      {
        stylix.targets.gnome.enable = false;
        boot.extraModulePackages = with config.boot.kernelPackages; [ alx-wol ];
        networking.hostName = "auspc";
        networking.hostId = "230c61e9";
        networking.networkmanager.enable = true;

        # A STATIC wired address, replacing the DHCP lease this box used to take.
        #
        # Why static at all: `lanAddress` on the host record (above) is what lets
        # anything reach this box over the LAN rather than through the wireguard
        # tunnel -- ../network/segments.nix's `lan-home` giver reads it, which
        # is how the VBAN stream resolves its receiver, and the builders and
        # ssh config will. A DHCP lease is not a fixed address, so a `lanAddress`
        # derived from one would go stale silently and return a WRONG answer
        # instead of throwing, which is worse than having none.
        #
        # /16, not /24: the house LAN really is one flat /16 -- secondpc is
        # 192.168.0.26/16 and hosts live on 192.168.1.x too, which is why
        # ../hosts/secondpc/fail2ban.nix's `ignoreIP` had to be widened from a
        # /32 to 192.168.0.0/16.
        # NO STATIC BLOCK AND NO `defaultGateway` HERE ANY MORE. The address is
        # on the radio, and NetworkManager addresses the radio because NM is
        # what ASSOCIATES it -- a `networking.interfaces` block on the same wire
        # would fight the profile. ../network/house-wifi.nix now derives
        # `ipv4.method = "manual"` and `address1` from this host's own
        # `attachments.lan-home`, with the router from
        # `den.aspects.lan-home.gatewayAddress`, so the address and the gateway
        # are each stated once in the fleet rather than per host.

        # NetworkManager stays -- it manages the wifi (`wlo1`, iwlwifi per the
        # facter report below) and the desktop applet on this machine, and
        # converting the whole host to networkd just to pin one interface would
        # be a far larger blast radius than the change needs. But it must not
        # fight the static config above, so the wired interface is handed over
        # explicitly.
        #
        # The house network itself is NOT configured here: `house-wifi` in the
        # includes above declares the profile, so this block only has to keep
        # `enp6s0` out of NM's hands.
        networking.networkmanager.unmanaged = [ "interface-name:enp6s0" ];
        time.timeZone = "Australia/Melbourne";
        i18n.defaultLocale = "en_AU.UTF-8";
        boot.initrd.compressor = "xz";

        boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
        boot.supportedFilesystems = [
          "zfs"
          "ntfs"
        ];
        nixpkgs.config.allowUnfree = true;
        boot.zfs.requestEncryptionCredentials = true;
        boot.zfs.forceImportRoot = true;
        services.zfs.autoScrub.enable = true;

        # Hardware detection (kernel modules, microcode, ...) from the facter
        # report instead of a hand-written hardware-configuration.nix.
        hardware.facter.reportPath = builtins.path {
          name = "facter";
          path = ./auspc/facter.json;
        };

        # Root/nix/var/home live on the natively-encrypted zpool `zpool`; disko
        # generates their fileSystems from the dataset layout. ZFS device paths
        # are dataset names, independent of which physical disk backs the pool,
        # so the generated mounts are correct regardless of the `disk` section
        # below. Pool unlock is handled by requestEncryptionCredentials above.
        #
        # NOTE: the physical `disk` entry is a best-effort description for
        # reprovisioning only — `disko` is NEVER run against this live machine.
        # `/boot` and `/mnt/hdd` are kept as explicit by-uuid mounts (matching
        # the running system) instead of disko-generated by-partlabel devices.
        disko.devices = {
          disk.nvme = {
            type = "disk";
            device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S4EWNG0M205506Z";
            content = {
              type = "gpt";
              partitions.zpool = {
                size = "100%";
                content = {
                  type = "zfs";
                  pool = "zpool";
                };
              };
            };
          };
          zpool.zpool = {
            type = "zpool";
            # Native ZFS encryption on the pool root — matches the original
            # config's `boot.zfs.requestEncryptionCredentials = true` (datasets
            # inherit the key, prompted at boot / via initrd-ssh).
            rootFsOptions = {
              encryption = "aes-256-gcm";
              keyformat = "passphrase";
              keylocation = "prompt";
              mountpoint = "none";
            };
            # Datasets as declared by the original hardware-configuration.nix:
            # zpool/{root,nix,var,home} -> /,/nix,/var,/home.
            datasets = {
              root = {
                type = "zfs_fs";
                mountpoint = "/";
                options.mountpoint = "legacy";
              };
              nix = {
                type = "zfs_fs";

                options.mountpoint = "legacy";
                mountpoint = "/nix";
              };
              var = {
                options.mountpoint = "legacy";
                type = "zfs_fs";
                mountpoint = "/var";
              };
              home = {
                type = "zfs_fs";

                options.mountpoint = "legacy";
                mountpoint = "/home";
              };
            };
          };
        };

        # `zpool/swap` zvol used as swap space; documents the dataset for
        # reprovisioning (see NOTE above). `swapDevices` below is set
        # explicitly since disko is never run against this live machine.
        disko.devices.zpool.zpool.datasets.swap = {
          type = "zfs_volume";
          size = "10G";
          content = {
            type = "swap";
          };
        };

        # ESP + data disk kept on their existing UUIDs (see NOTE above).
        hardware.bluetooth = {
          enable = true;
          powerOnBoot = true;
        };
        services.blueman.enable = true;

        services.printing.enable = true;
        security.rtkit.enable = true;
        services.pipewire = {
          enable = true;
          alsa.enable = true;
          pulse.enable = true;
          jack.enable = true;

          # ── SYSTEM-WIDE, BECAUSE THE TURNTABLE CAPTURE HAS NO SESSION ──
          #
          # PipeWire is a USER service by default and logind grants `/dev/snd`
          # ACLs only to the user of the ACTIVE session on a seat. This box runs
          # headless most of the time, so there is no active session, no ACLs,
          # and wireplumber enumerates NOTHING -- measured: `wpctl status` shows
          # no ALSA devices at all while `arecord -D plughw:0,0` works fine from
          # the same shell, because direct device access comes from `audio`
          # group membership and does not need a seat.
          #
          # The capture node in ../nixos/turntable.nix is created by PipeWire
          # and read by `vbanSend`, which is a SYSTEM service. A user-session
          # PipeWire is the wrong place for a pipeline neither end of which
          # belongs to a login session, and it fails exactly as measured: the
          # node cannot be created because there is no device to open.
          #
          # ── WHAT THIS COSTS, because nixpkgs advises against it ──
          #
          # The option's own description points at PipeWire's NEWS saying
          # system-wide is not recommended: there is no per-user isolation, and
          # every user of the daemon shares one graph. For a desktop that is a
          # real downside. For this box it is the right trade: the turntable has
          # to stream whether or not anyone is logged in, and that is not
          # expressible with a per-session daemon.
          #
          # Access is by GROUP rather than by seat once this is on -- see
          # `users.users.auscyber.extraGroups` below.
          systemWide = true;
        };

        # ── THE TURNTABLE'S HARDWARE, as data for ../nixos/turntable.nix and
        # ../nixos/hda-jack-retask.nix ──
        #
        # Everything below is READ OFF THE RUNNING MACHINE
        # (`/proc/asound/card0/codec#0`), not inferred from the board model.
        # facter (./auspc/facter.json) is no help here and it is worth saying
        # why: it reports the PCI CONTROLLER -- `8086:a348`, sub `1462:cb12`, at
        # `0000:00:1f.3`, driver `snd_hda_intel` -- and an HDA codec is a device
        # on a bus behind that controller, which hwinfo does not walk. So the
        # codec, its pins and its ADCs are not in the hardware report and cannot
        # be.
        #
        #   Codec        Realtek ALC1220
        #   Vendor Id    0x10ec1220
        #   Subsystem Id 0x1462cb12
        #   Revision Id  0x100101      (not part of the patch's match)
        #   Address      0
        hardware.hdaJackRetask = {
          codecId = "0x10ec1220";
          subsystemId = "0x1462cb12";
          address = 0;

          # ── NO PINS RETASKED, BECAUSE NOTHING NEEDS TO BE ──
          #
          # With a TRS cable in the blue line-in (0x1a), the only jack in use is
          # already `[Jack] Line In at Ext Rear` and already stereo. The pink
          # mic jack is not in the signal path at all, so there is nothing to
          # convert.
          #
          # ── WHAT WAS LEARNED WHILE IT WAS NEEDED, KEPT BECAUSE IT COST A LOT ──
          #
          # When each RCA had its own socket, the pink jack (0x18) was one leg
          # and had to be retasked from Mic In to Line In -- `0x01a11040` ->
          # `0x01811040`, the default-device field `0xa` -> `0x8` -- to stop the
          # codec feeding it mic BIAS (measured as a -850 DC offset on that
          # channel) and reading it at mic sensitivity (~13 dB quieter than the
          # line jack on the same source).
          #
          # Two things were established on the box and are worth not relearning:
          #
          #   * `0x17` CANNOT be an input however it is configured. Writing
          #     `0x01811040` to it via `user_pin_configs` + `reconfig` added no
          #     `Input Source` item at all. `Pincap`'s `IN` bit means the pin can
          #     accept a signal electrically, not that an ADC can read it -- a
          #     pin outside the ADC's connection list is unreachable, and the
          #     factory `0x411111f0` ("no physical connection") was correct.
          #
          #   * `user_pin_configs` ACCUMULATES. Every write stays active until
          #     `echo 1 > .../clear` or a reboot, so testing several candidates
          #     in sequence gives an enum that is the sum of all of them and
          #     tells you nothing about any one. From a clean state the factory
          #     enum is `Rear Mic` / `Front Mic` / `Line`; with 0x18 retasked it
          #     becomes `Mic` / `Line` / `Line 1`.
          pins = { };
        };

        hardware.turntable = {
          # ⚠ BLOCKED DATA, and deliberately left unset so evaluation says so.
          # ../nixos/turntable.nix declares `card` with no default because the
          # alternative is guessing: "PCH" is the usual ALSA name for a Realtek
          # on an Intel PCH and a wrong name is a capture node pointed at
          # another card, which fails silently. Fill it in from the second
          # column of `/proc/asound/cards` on auspc.
          #
          # ── THE CARD BY NAME, CONFIRMED ON THE BOX ──
          #
          # `PCH`, the second column of `/proc/asound/cards` for the Realtek.
          # NOT an index: this box has two HDA controllers -- the Intel PCH with
          # the jacks and the nVidia one at `0000:01:00.1` which is HDMI audio
          # and has no inputs -- and the index is probe order. On a boot where
          # the GPU enumerates first, `plughw:0,0` is the GPU and the turntable
          # captures silence with nothing reporting a fault.
          card = "PCH";

          # TWO LEGS, one channel from each -- the sketch's shape, and MEASURED
          # to be the right one here: two cables are plugged in, one into the
          # factory line-in (0x1a) and one into 0x17, which the patch above
          # enables. So each jack contributes one channel and the two are merged
          # to stereo. `device` is the ALSA PCM index for each of the codec's
          # two `Audio Input` widgets (0x08 -> DEV=0, 0x09 -> DEV=2).
          #
          # WHICH JACK ENDS UP ON WHICH ADC IS NOT SET BY THE PATCH -- see
          # `mixerControls` below and the header of ../nixos/turntable.nix.
          # ── ONE STEREO JACK, WHICH IS WHAT A TRS CABLE MAKES IT ──
          #
          # A 2xRCA -> 3.5mm TRS cable carries both channels, properly
          # referenced, into the blue line-in (0x1a) -- a jack that is already a
          # stereo line input and needs no retasking. So this is a single
          # capture node and there is no combine-stream at all.
          #
          # ── WHY THE TWO-JACK VERSION IS GONE ──
          #
          # It existed because each RCA was in its own socket via an adapter,
          # giving one channel per jack. That arrangement was MEASURED to be the
          # fault, not the shape: the signal arrived ~45 dB low with almost no
          # bass, because an adapter was passing it on the ring with the tip
          # floating -- a capacitively coupled, unreferenced connection, which
          # attenuates, high-passes, and picks up buzz. Proven by taking the
          # deck straight into a Sonos line-in, where it sounded correct: the
          # deck, its preamp and its RIAA are all fine.
          #
          # So the TRS cable removes the two-ADC merge, the `0x18` retask and
          # its `.fw`, one of the two `Input Source` pins, and the level
          # mismatch between legs -- all of which were compensation for the
          # cable.
          #
          # `take` is BOTH channels and maps straight through: a stereo jack
          # needs no channel surgery, where each mono leg had to be pulled off
          # `FR` (the ring) and placed.
          captures = [
            {
              device = 0;
              take = [
                "FL"
                "FR"
              ];
              mapsTo = [
                "FL"
                "FR"
              ];
              description = "Turntable, stereo on the blue line-in (0x1a)";
            }
          ];

          # wireplumber's whole ACP device for this card, disabled because
          # `captures` above opens DEV=0 statically and an ALSA capture device
          # opens exclusively. Matched on the PCI address rather than a card
          # index, which is the one identifier facter does give us
          # (`sysfs_bus_id` `0000:00:1f.3`) and which no probe order can move.
          #
          # THE DEVICE AND NOT THE NODE, both halves measured on this box: a
          # node-level rule did clear the PCM contention (the `alsa_input.pci-*`
          # ports left `pw-link -o` and the static node got the card), but the
          # device survived it and kept restoring its saved MIXER state on every
          # pipewire start -- `Capture` back to +30 dB, clipping the deck into a
          # square wave, over the top of the `mixerControls` unit below. See
          # ../nixos/turntable.nix's `releaseDevices`.
          #
          # This costs the onboard mic/line-in as ordinary desktop inputs. That
          # is the trade this box is making; the USB webcam and the RODE AI-1
          # are separate cards and are untouched.
          releaseDevices = [ "~alsa_card\\.pci-0000_00_1f\\.3.*" ];

          # ── THE HUM, NOTCHED OUT ──
          #
          # 50 Hz and its odd harmonics, which is what this input measured: with
          # the cable OUT the floor is -92.19 dB, so the card is clean and every
          # bit of it arrives through the cable; with the cable in and the deck
          # idle it is -23.56 dB, of which the odd harmonics are 94% of the
          # power. ../../nixos/turntable.nix's `filtering` binding has the full
          # spectrum and the reasoning.
          #
          # ⚠ THIS IS WORTH 12 dB AND THE PHYSICAL FIX IS WORTH MORE. A -23.56 dB
          # noise floor against music at -23 to -31 dB is 0-7 dB of SNR, and
          # notching the comb leaves about -35 dB. The odd-harmonic signature
          # says magnetic induction from a saturating transformer core, which is
          # directional and falls off fast: rotating the deck or moving it away
          # from a power supply is routinely 10-20 dB. This filter is on top of
          # that, not instead of it.
          #
          # No `rumble`: subsonic measured -43.9 dB here, twenty dB below the
          # hum, so a high-pass would cost phase shift in the bass to remove
          # nothing.
          filter.mains = 50;

          # ⚠ BLOCKED DATA. Each ADC needs its `Input Source` pointed at its own
          # jack or both capture the same signal and the merge is silently mono.
          # The control name and the enum values have to be read off the card
          # after the retask is live:
          #
          #   amixer -c <card> contents
          #
          # then one entry per ADC, e.g.
          #
          # ── MEASURED, AND IT WAS THE WHOLE FAULT ──
          #
          # `amixer -c0 contents` on the live box, with a record playing and
          # nothing being captured:
          #
          #   numid=295 'Input Source'          items 0='Mic' 1='Line' 2='Line 1'  values=0
          #   numid=296 'Input Source' index=1  items 0='Mic' 1='Line' 2='Line 1'  values=0
          #
          # BOTH ADCs were selecting `Mic`. So neither capture device was
          # listening to a line jack at all, which is why a line-level signal
          # read as silence -- not a level problem, a routing one, and exactly
          # the failure ../nixos/turntable.nix's header predicts for an
          # unset `mixerControls`.
          #
          # Each ADC now takes a DIFFERENT line item, which is the half the
          # `.fw` cannot express: the patch makes a jack an input, this decides
          # which input each ADC hears. Both on one item would capture one jack
          # twice and merge it into itself -- audible as a dead stereo image on
          # material that plays perfectly well, which is the worst way for this
          # to be wrong.
          #
          # The item NAMES come from the enum the driver built, not from a pin
          # number: `Line` and `Line 1` are what ALSA calls the two line inputs.
          # ── ONE JACK, SO ONE SOURCE PIN AND ONE CAPTURE ──
          #
          # From a CLEAN codec state (no `user_pin_configs` overrides) the
          # factory enum is `Rear Mic` (0x18) / `Front Mic` (0x19) / `Line`
          # (0x1a) -- measured. `Line` is the blue jack, which is where the TRS
          # cable goes, so that is the whole routing decision.
          #
          # ── CAPTURE WAS MUTED, AND THAT WAS HALF THE SILENCE ──
          #
          # Measured with a record playing and the routing already correct:
          #
          #   'Capture',0  Front Left: Capture 0 [0%] [-17.25dB] [off]
          #
          # `[off]` is the capture switch. Both ADCs were pointed at the right
          # input and told not to listen -- two independent faults with one
          # symptom, which is why fixing the routing alone changed nothing.
          #
          # TWO ENTRIES PER CONTROL, not one: `value` is shell-escaped as a
          # SINGLE argument (see `script` in ../nixos/turntable.nix), so
          # `"80% cap"` would arrive as one word and be rejected.
          #
          # 80% of 0-63 rather than the top: a line-level source has headroom
          # to spare and the top of the range is where the ADC clips. `Line
          # Boost` is deliberately untouched -- it is a 0-3 step boost for weak
          # sources and on a properly referenced line feed it only adds noise.
          mixerControls = [
            {
              control = "Input Source";
              index = 0;
              value = "Line";
            }
            {
              control = "Capture";
              index = 0;
              value = "cap";
            }
            # ── THE LEVEL, IN RAW STEPS AND NOT PERCENT ──
            #
            # `23`, measured. This control's range STRADDLES ZERO -- `0` is
            # -17.25 dB and `63` is +30.00 dB, 64 steps of 0.75 dB -- so a
            # percentage is the wrong unit for it and `80%` was about +20.5 dB.
            # That is the "+20 dB clipping" this rig was debugged through: a
            # line-level deck into +20 dB of ADC gain is a square wave, and no
            # amount of adjusting anything downstream recovers it. `23` is
            # 0.00 dB, which is the right operating point for a line source.
            #
            # ── AND NOT A PER-CHANNEL TRIM, WHICH WAS TRIED AND REJECTED ──
            #
            # `amixer` takes `23,32` here, and that was measured as a fix for a
            # ~6 dB left-heavy imbalance on music. It is the wrong lever. With
            # the hum as a STEADY reference signal (music RMS is
            # program-dependent and three readings were confounded by the record
            # moving to a quieter passage), the capture path measures balanced
            # to within 1.8 dB, and the ADC gain responded to the trim exactly
            # as asked -- so the 6 dB is in the SOURCE, and the trim pushed the
            # path 5 dB wrong in the other direction.
            #
            # The imbalance also wanders -- 5.3, 6.0, 6.7, 6.9, 7.4, 8.0, 8.9 dB
            # across readings -- and that variance is itself the diagnosis: a
            # geometric error like azimuth is stable, a high-resistance contact
            # is not. Adjusting azimuth made it worse in both directions.
            # Trimming it here would bake one record's imbalance, and a moving
            # one at that, into the hardware configuration.
            {
              control = "Capture";
              index = 0;
              value = "23";
            }

            # The 0-3 input boost between the pin and the input mixer, AHEAD of
            # `Capture`. Stated although it is already 0 on this card, because
            # "correct by accident" stops being true the moment something
            # restores a saved mixer state -- which is exactly what the ACP
            # device was doing to `Capture` above. At step 2 or 3 a line-level
            # deck clips no matter what `Capture` says, and the signature is
            # distinctive: attenuating `Capture` scales the flat tops down
            # without removing them.
            {
              control = "Line Boost";
              index = 0;
              value = "0";
            }

            # The line input mixed back into the card's own OUTPUT path. Off,
            # because this jack is capture-only and on an onboard codec that
            # round trip is a well-known source of hum and hiss back into the
            # ADC. Separate from `Line,0`, which is the same monitoring idea as
            # a playback volume and is already muted.
            {
              control = "Loopback Mixing";
              index = 0;
              value = "off";
            }
          ];
        };

        programs.dconf.enable = true;
        programs.zsh.enable = true;
        programs.kdeconnect.enable = true;
        programs._1password.enable = true;
        programs._1password-gui = {
          enable = true;
          polkitPolicyOwners = [ "auscyber" ];
        };

        users.users.auscyber = {
          isNormalUser = true;
          description = "Ivy";
          extraGroups = [
            "video"
            "wheel"
            "input"
            "tty"
            "dialout"

            # ── WITH A SYSTEM-WIDE PIPEWIRE, ACCESS IS BY GROUP ──
            #
            # `services.pipewire.systemWide` above moves the daemon out of the
            # login session, and its own description says the socket is then
            # usable by "all users in the `pipewire` group". Without this the
            # desktop session would have no audio at all -- the daemon would be
            # running and this user could not reach it.
            "pipewire"

            # Direct ALSA access, which is what makes `arecord`/`amixer` work
            # from an SSH session with no seat -- measured, and the reason the
            # capture could be debugged by hand long before PipeWire could see
            # the card.
            "audio"
          ];
          openssh.authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"
          ];
        };
        fileSystems = {
          "/" = {
            device = "zpool/root";
            fsType = "zfs";
          };

          "/nix" = {
            device = "zpool/nix";
            fsType = "zfs";
          };

          "/var" = {
            device = "zpool/var";
            fsType = "zfs";
          };

          "/home" = {
            device = "zpool/home";
            fsType = "zfs";
          };

          "/boot" = {
            device = "/dev/disk/by-uuid/2BFB-8C7C";
            fsType = "vfat";
            options = [
              "fmask=0022"
              "dmask=0022"
            ];
          };
          "/mnt/hdd" = {
            device = "/dev/disk/by-uuid/112e9d72-3f90-4936-988b-521d50371d09";
            fsType = "ext4";
          };
        };

        # `zpool/swap` is a 16G zvol on the encrypted pool (see disko.devices
        # above). Like the `disko.devices` root/nix/var/home datasets, this
        # matches an already-provisioned dataset rather than something disko
        # creates on this live machine — see the NOTE at the top of the
        # `disko.devices` block. Create it once out-of-band before applying:
        #   zfs create -V 16G -b $(getconf PAGESIZE) \
        #     -o compression=zle -o logbias=throughput -o sync=always \
        #     -o primarycache=metadata -o secondarycache=none \
        #     -o com.sun:auto-snapshot=false zpool/swap
        #   mkswap -f /dev/zvol/zpool/swap
        #        swapDevices = [
        #          { device = "/dev/zvol/zpool/swap"; }
        #        ];
      };
  };

  den.aspects.auscyber = {
    # ── BOTH SERVERS, AS ONE CHAIN ──
    # Was `celler-use = { secondpc = [ "main" ]; celler2 = [ "main" ]; }` -- the
    # bare-list form, which ../base/celler/_lib.nix normalises to pull AND push.
    # Said as a take it is one declaration with no server named at all: `celler`
    # has `reach = "fleet"`, so this resolves to every celler the fleet has, and
    # adding a third needs no edit here.
    #
    # The two flows are separate requests with separate constraints, which the
    # flat form could not express: the 100MB body cap binds on the push and not
    # on the pull, so a carrier that caps bodies can carry one and not the other.
    takes.celler.gives.cache.main.gives = {
      pull = { };
      push = { };
    };
    includes = [
      den.aspects.fish
      den.aspects.lspmux
      den.aspects.claude
      (den.batteries.unfree [ "castlabs-electron" ])
    ];

    provides.auspc = {
      includes = [
        den.aspects.lix
        den.aspects.plasma
        den.batteries.primary-user
        #        den.aspects.zotero
        <browsers/helium>
        den.aspects.gui
        den.aspects.ghostty
        den.aspects.neovim
        den.aspects.gpg
        den.aspects.nushell
        den.aspects.dev
      ];

      provides.to-users.homeManager = { pkgs, ... }: {
        services.gpg-agent = {
          enable = true;
          enableSshSupport = true;
          pinentry.program = pkgs.pinentry-qt;
          extraConfig = "allow-loopback-pinentry";
        };

        stylix.targets.gnome.enable = false;

        home.packages = with pkgs; [
          obs-studio
          (heroic.override {
            extraPkgs =
              pkgs': with pkgs; [
                gamescope
                gamemode
              ];
          })
          shadps4
          tidal-hifi
          tmux
          pcmanfm
          vscode
          openjdk8
          rofi
          arandr
          libnotify
          stack
          xclip
          discord
          playerctl
          htop
          polychromatic
          fish
          maim
          gcc
          dunst
          lua
          unzip
          slack
          (python3.withPackages (p: with p; [ pynvim ]))
          nautilus
          zoom-us
          file
          mitscheme
          libreoffice
          thunderbird
        ];
      };
    };
  };
}
