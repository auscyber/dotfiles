{
  den,
  lib,
  houseWifi,
  homepageLib,
  ...
}:
let
  # On the fleet record as well as on the interface below: hosts on the same
  # switch reach this box here rather than through the wireguard tunnel.
  lanAddress = "192.168.0.26";
in
{
  den.hosts.x86_64-linux.secondpc = {
    # HOW THIS BOX IS ON THE HOUSE LAN: the address other hosts dial it at,
    # and the wire it hears them on. `br0` and not the NIC, because this host
    # bridges its ethernet and its access point together (below -- hostapd
    # puts `ap0` into the same bridge), so everything from the LAN arrives
    # over the bridge.
    #
    # Both on the ATTACHMENT rather than on the host, because they are one
    # fact about one wire: ../../base/firewall.nix opens a lan-exposed port on
    # exactly this interface, and reading it out of `networking.bridges` would
    # mean evaluating this host to find out.
    attachments.lan-home = {
      address = lanAddress;
      interface = "br0";
    };

    # Its own entity rather than an include, so it dials postgres from where
    # it runs (../../base/container.nix).
    # ../../services/homepage.nix: one page per member, each its own container.
    containers = {
      splitpro.runtime = "oci";
    }
    // homepageLib.containersFor den.households.ivymectin;

    # Serves the household; owned by nobody -- it is infrastructure, not a
    # personal machine, which is why `owner` is deliberately absent.
    # The household whose config this host produces: rooms, members, and who may
    # reach its services. See ../../base/household.nix.
    household = den.households.ivymectin;

    # WHAT THIS MACHINE RUNS, as service instances. The only place placement is
    # stated: ../../services/celler-service.nix declares what a celler IS and
    # names no host, because not all cellers are on secondpc -- this one is.
    #
    # One cache declared, four instances generated
    # (../../base/service-instances.nix): the daemon, `main`, and `main`'s
    # implied push and pull. `main` because that is the only cache there is --
    # ../../base/celler/celler-keys.json holds `secondpc.main` and nothing else,
    # and under this split the cache list and the key list come from this one
    # declaration rather than from two places that can disagree.
    # `main` is PUBLIC -- it is `cache.ivymect.in`, readable without a
    # credential, which is why it is the one cache the cloudflare tunnel
    # publishes. A cache is not public unless it says so
    # (../../services/celler-service.nix `defaults.public = false`).
    # THE CACHE THIS HOST SERVES is now an entry in the `includes` list below
    # (`den.aspects.celler-main`), not a `runs.celler.defines.cache` tree. Two
    # `includes` in one literal is a duplicate attribute, so it goes where the
    # list already is.

    # NO `runs.lan-home.address`, NO `runs.postgres`, NO `runs.nextcloud`.
    #
    # The address was the same value as `lanAddress` above, stated twice.
    # ../../network/segments.nix's `lan-home` giver reads `lanAddress` off the
    # host record, so the attachment IS the inclusion plus that one declared
    # fact -- and the prefix stays on the segment, where `192.168.0.0/16` is a
    # fact about the wire identical for everything on it.
    #
    # `runs.postgres` and `runs.nextcloud` declared the instances that edges
    # pointed at: a want with no provider in scope was a throw naming the scopes
    # it looked in. Inclusion says the same thing, and says it transitively --
    # `den.aspects.nextcloud` is in the list below and ITS own `includes` pulls
    # `den.aspects.postgres`, so this host gives postgres without naming it,
    # which a `runs` entry could not express.
    #
    # ../../base/capability.nix answers "who gives postgres here" from
    # `hasAspect`, which walks the whole inclusion closure and so cannot
    # disagree with reality the way a declaration can.

    # Which of the household's services this machine serves -- the manifest.
    # The services themselves are declared in ../../households/ivymectin.nix;
    # claiming a name it does not declare is an evaluation error
    # (../../base/service.nix `serves`).
    services = {
      postgres.enable = true;
      # The house's mail (../../services/mail.nix). Serving it is what makes
      # this box the one the MX points at and the one ddclient keeps the A
      # record pointing at.
      mail.enable = true;
      # S3 on loopback (../../services/garage.nix). No `access` block, same
      # reason as `nextcloud` below plus a real one: garage's permission model
      # is per (bucket, access key) and a kanidm group has nothing to map to.
      garage.enable = true;
      vikunja.enable = true;
      donetick.enable = true;
      splitpro.enable = true;
      # ../../services/grocy.nix. sqlite, and the gate is its login -- no
      # postgres role, no OIDC client and no secret of its own.
      grocy.enable = true;
      # ../../services/paperless.nix. OCR for scanned paper. A postgres role
      # over the socket and the gate for its login, so no secret of its own
      # either. See ../../docs/receipts-and-expenses.md.
      paperless.enable = true;
      # ../../services/tandoor-recipes.nix. A real kanidm OIDC client, like
      # vikunja and donetick, with its own postgres role over the socket.
      tandoor.enable = true;
      audiobookshelf.enable = true;
      music-assistant.enable = true;
      home-assistant.enable = true;
      # No `access` block on this service, deliberately: `gateway.services.<n>.upstream`
      # has no default and `enable` defaults to `services.<n>.enable`, so an access
      # block would make ../../services/access.nix create a gateway entry with an
      # undefined mandatory option the moment this claim exists. Nextcloud's only
      # role is its hardcoded `admin` group, so there is nothing for a level to mean.
      nextcloud.enable = true;

      # The `arr` group's members. Tagged as one group in the household, so a
      # single member grant opens all six.
      sonarr.enable = true;
      radarr.enable = true;
      lidarr.enable = true;
      prowlarr.enable = true;
      bazarr.enable = true;
      qbittorrent.enable = true;
    };
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICj7wlOxTp0NQJoUhRtj7k8gtDC0lCr5MJqLV5LxG9Yf root@kexec-minimal";

    # Advertise as a build machine — auspc/laptop including `den.aspects.builders`
    # will discover this entry and add it to their nix.buildMachines.
    builder = {
      ipAddress = "10.100.0.1";
      systems = [ "x86_64-linux" ];
      maxJobs = 2;
      speedFactor = 5;
      features = [
        "big-parallel"
        "cached-compilation"
        "kvm"
      ];
      sshUser = "builder";
    };

    users.auscyber = {
      # The human behind this account (../../persons/ivy.nix). The link
      # points account -> person, never the reverse; ../../base/person.nix says why.
      person = den.persons.ivy;
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA/+SE8omiIJ7VBQKgMLsgyADUVtY37o0kn0g0uwQVKi auscyber@secondpc";
    };
  };

  den.aspects.secondpc = {
    layers = [ "nixos" ];

    # `celler` used to be declared here as `github:blitz/celler/main` while
    # ../../base/caches.nix declares `github:auscyber/celler/main`. Two forks,
    # invisible to each other in separate partition fixpoints, so this host
    # built against a celler nothing else used. One merged environment turns
    # that into a hard conflict, which is how it was found. caches.nix is
    # canonical; this declaration is gone.

    inputs.nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
    inputs.nixos-mailserver.inputs.nixpkgs.follows = "nixpkgs";
    # NO `patch.enable` ANY MORE: upstream fixed this itself. arion's
    # container-systemd module used to set `services.journald.console`, which
    # nixpkgs removed, and its Haskell test suite evaluates that module -- so
    # ../../../patches/arion carried the replacement. At the locked rev arion
    # does it version-conditionally (`if options.services.journald ? settings`),
    # which is strictly better than our unconditional patch, so the patch and
    # the flag are both gone.
    inputs.arion.url = "github:hercules-ci/arion";
    inputs.arion.inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-flatpak.url = "github:gmodena/nix-flatpak/";

    includes = [
      # THE CACHE THIS HOST SERVES. `den.aspects.celler-main` carries the cache
      # record (../../services/celler-service.nix), and including it is what
      # makes this the host that serves it -- ../../base/celler/server.nix
      # derives its cache list from exactly this inclusion, so the list and the
      # signing keys in celler-keys.json cannot disagree.
      den.aspects.celler-main

      # Declared in ../../nixos/impermanence.nix now, so birdsarentreal can
      # have it too; den rejects an input declared twice.
      den.aspects.impermanence
      den.aspects.nginx
      den.aspects.nix
      # cache.ivymect.in (../../base/celler/server.nix). On the host aspect
      # rather than on `den.aspects.nix.provides.secondpc` with the ncps cache
      # next to it -- see the note there.
      den.aspects.celler
      den.aspects.local
      den.aspects.vpn-server
      # Alongside ../../network/vpn.nix's wireguard, not instead of it: the
      # wireguard hub is this box, so a client that cannot reach its fixed
      # endpoint has nothing, and the tailnet is what the laptop uses to reach
      # the celler here from outside the LAN.
      den.aspects.tailscale
      # PIA (../../network/pia.nix). The `arr` group egresses only through this
      # tunnel, and this box becomes a tailscale exit node over it -- so a
      # tailnet device can route through PIA by its own choice. Needs
      # secrets/static/pia_auth.age; until it holds a real login the tunnel
      # stays down and the `arr` group has no egress at all.
      den.aspects.pia
      # On the house LAN: ../../network/segments.nix. Inclusion IS the
      # attachment, and the address comes from this host's `lanAddress`.
      den.aspects.lan-home
      # One tunnel in front of this box's nginx, so every *.ivymect.in vhost is
      # reachable without a port forward. See ../../network/cloudflare.nix.
      # THE ZONE THIS HOST PUBLISHES UNDER. Including it is the whole
      # declaration: it carries `ivymect.in`, gives the public carrier with
      # Cloudflare's 100MB body cap, and sets `gateway.domain` from the one
      # place the zone is named. Pulls in `cloudflare-tunnel`, which is how the
      # zone is served.
      den.aspects.publish-ivymect-in
      den.aspects.builders
      den.aspects.builder-server
      den.aspects.disko
      den.aspects.facter
      den.aspects.secondpc-web
      den.aspects.searchix
      den.aspects.plex
      den.aspects.gateway
      # THE PROXY, AND THE GATE ON TOP OF IT. Without this nothing translates a
      # service's `takes.proxy`/`takes.gated` into a `gateway.services` entry,
      # so a converted service is declared and published nowhere -- which is
      # exactly what `attribute 'immich' missing` was.
      den.aspects.gated-oauth2-proxy
      den.aspects.kanidm
      den.aspects.service-accounts
      den.aspects.grafana
      den.aspects.tempo
      den.aspects.alloy
      den.aspects.homepage
      den.aspects.prowlarr
      den.aspects.audiobookshelf
      den.aspects.immich
      den.aspects.homeassistant
      den.aspects.mail
      den.aspects.nextcloud
      #      den.aspects.slskd
      den.aspects.sso
      # Each of these three is its own kanidm OIDC client rather than another
      # oauth2-proxy tenant, so they keep real per-user accounts. They share one
      # postgres through ../../services/postgres.nix's `postgres` quirk.
      # Who may reach which service, from ../../persons/. See ../../services/access.nix.
      den.aspects.access
      den.aspects.garage
      den.aspects.vikunja
      den.aspects.donetick
      den.aspects.grocy
      den.aspects.paperless
      den.aspects.tandoor
      den.aspects.samba
      # A 2.4GHz AP on the USB MT7925, bridged into br0 below rather than
      # routed. See ../../network/hostapd.nix -- `hostapd.ssid` is set in the
      # nixos block and must match the house router's SSID.
      den.aspects.hostapd
      # PXE on the house LAN (../../services/netbootxyz.nix). Beside hostapd
      # rather than in the household's `services`: the consumer is a NIC, not
      # a person, so there is nothing to grant. It needs no router change --
      # dnsmasq runs proxy-DHCP and allocates nothing.
      den.aspects.netbootxyz
      den.aspects.secondpc-fail2ban
      den.aspects.secondpc-otel-fail2ban
      (den.batteries.unfree [
        "intel-ocl"
        "plexmediaserver"
      ])
    ];

    # Render wg0 as a networkd netdev rather than a wg-quick unit. This host
    # bridges and routes for the LAN, and networkd is what its working
    # pre-dendritic config used; it also brings systemd-resolved back, without
    # which /etc/resolv.conf is left pointing at a stub resolver that nothing
    # is listening on.
    vpn.backend = "networkd";

    nixos =
      {
        config,
        pkgs,
        ...
      }:
      {
        # Host identity / boot
        networking.hostId = "4f6f802e";

        # The nix cache stays a direct A record. Cloudflare caps a proxied
        # request body at 100MB and `celler push` sends NARs far past it, so
        # tunnelling this one would break pushing while pulls kept working.
        # ── NOTHING HAND-EXCLUDED ANY MORE ──
        #
        # `cache.ivymect.in` was listed here because `celler push` sends NARs
        # far past Cloudflare's 100MB body cap. That exclusion is now DERIVED:
        # ../../base/celler/server.nix declares the cache's 15G limit and
        # ../../network/cloudflare.nix's `excluded` compares it against the
        # cap -- so the list cannot drift from the limits again, which is how
        # `cloud.ivymect.in` came to be missing from it while nextcloud
        # advertised 10G uploads.
        #
        # `exclude` remains for a name that has to stay a direct A record for a
        # reason OTHER than body size.
        services.cloudflared-tunnel.exclude = [ ];

        # Zone the gated service aspects publish under; they name only their
        # own subdomain, so this is the one place the domain appears.
        # `gateway.domain` is NOT set here: ../../network/cloudflare.nix's
        # `publish-ivymect-in` sets it, from the one place the zone is named.

        # Where the declared shares (../../services/shares.nix) are served out
        # of over SMB. Here and not in ./samba.nix because it is a fact about
        # this box's disks -- the data pool is `zpool` mounted at /mnt/hdd (see
        # `disko.devices` below) -- and not about what a share is. A share that
        # needs to live elsewhere sets its own `path`.
        samba.shareRoot = "/mnt/hdd/shares";
        # Everyone in this group reaches every gated service through a browser;
        # the per-service `svc-*` groups are for machine callers.
        gateway.humanGroups = [ "media-users" ];

        # Hardware detection (kernel modules, microcode, ...) comes from the
        # `facter` aspect reading this report instead of a hand-written
        # hardware-configuration.nix.
        hardware.facter.reportPath = ./facter.json;

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.loader.efi.efiSysMountPoint = "/boot/efi";

        boot.supportedFilesystems = [ "zfs" ];
        # `zroot` (root pool) is imported automatically because `/` lives on it;
        # only the data pool needs to be listed here.
        boot.zfs.extraPools = [
          "zpool"
          "zroot"
        ];

        boot.kernel.sysctl = {
          "net.ipv6.conf.all.accept_ra" = 2;
          "net.ipv6.conf.all.accept_redirects" = 1;
          "net.ipv6.conf.all.accept_source_route" = 1;
        };

        # 15G, no swap, and the service stack resident: builds get OOM-killed.
        zramSwap.enable = true;
        nix.settings = {
          max-jobs = 2;
          cores = 2;
        };

        # Disk layout (disko + disko-zfs). Two SATA disks:
        #   ssd  → ESP (/boot/efi) + zpool `zroot`  (root)
        #   hdd  → zpool `zpool`                    (/mnt/hdd data)
        # fileSystems for `/`, `/boot/efi` and `/mnt/hdd` are generated from
        # this layout — do not also declare them by hand.
        disko.devices = {
          disk = {
            ssd = {
              type = "disk";
              device = "/dev/disk/by-id/wwn-0x5707c1810016e7fa-part2";
              content = {
                type = "gpt";
                partitions = {
                  ESP = {
                    size = "512M";
                    type = "EF00";
                    content = {
                      type = "filesystem";
                      format = "vfat";
                      mountpoint = "/boot/efi";
                      mountOptions = [ "umask=0077" ];
                    };
                  };
                  zroot = {
                    size = "100%";
                    content = {
                      type = "zfs";
                      pool = "zroot";
                    };
                  };
                };
              };
            };
            hdd = {
              type = "disk";
              device = "/dev/disk/by-id/ata-ST4000VN006-3CW104_WW67DVN6-part1";
              content = {
                type = "gpt";
                partitions = {
                  zpool = {
                    size = "100%";
                    content = {
                      type = "zfs";
                      pool = "zpool";
                    };
                  };
                };
              };
            };
          };
          zpool = {
            zroot = {
              type = "zpool";
              options.ashift = "12";
              rootFsOptions = {
                compression = "zstd";
                acltype = "posixacl";
                xattr = "sa";
                "com.sun:auto-snapshot" = "false";
                mountpoint = "none";
              };
              # The installed pool has `mountpoint=legacy` on both datasets.
              # disko only omits `-o zfsutil` from the generated fileSystems
              # when it sees that property, and mounting a legacy dataset with
              # zfsutil fails outright — which under systemd stage 1 (on by
              # default now) surfaces as sysroot failing to mount.
              datasets.nixos = {
                type = "zfs_fs";
                mountpoint = "/";
                options.mountpoint = "legacy";
              };
            };
            zpool = {
              type = "zpool";
              options.ashift = "12";
              mountpoint = "/mnt/hdd";
              rootFsOptions = {
                compression = "zstd";

                mountpoint = "none";
              };
              datasets."root" = {
                type = "zfs_fs";
                options.mountpoint = "legacy";
                mountpoint = "/mnt/hdd"; # or wherever you want it mounted
              };
              datasets.s3 = {
                type = "zfs_fs";

                mountpoint = "/mnt/hdd/s3";
              };
            };
          };
        };

        # Networking: bridge + static ipv4, rendered by systemd-networkd.
        # `den.aspects.vpn-server` selects the networkd wireguard backend, which
        # turns on `networking.useNetworkd`; the `networking.*` options below are
        # translated into .network units by nixpkgs. Note that networkd asserts
        # `defaultGateway.interface` is set, so the plain-string form is not
        # usable here.
        networking.useDHCP = false;
        networking.enableIPv6 = true;
        # ap0 is NOT listed here. hostapd adds it to br0 itself once it has put
        # the radio into AP mode -- enslaving it from networkd first is what
        # makes that mode switch fail. See ../../network/hostapd.nix.
        networking.bridges.br0.interfaces = [ "enp2s0" ];
        networking.interfaces.br0.useDHCP = false;
        networking.interfaces.br0.ipv4.addresses = [
          {
            address = lanAddress;
            prefixLength = 16;
          }
        ];
        networking.defaultGateway = {
          address = "192.168.0.1";
          interface = "br0";
        };
        networking.nameservers = [ "1.1.1.1" ];

        # MUST BE SET BEFORE DEPLOYING. The house runs one SSID per location
        # ("Hallway Beehive", "Bedroom Beehive"), so pick the one this radio is
        # meant to extend -- the key in secrets/static/wifi_password.age has to be that
        # network's passphrase, or clients will see the SSID and fail to
        # associate. ../../network/hostapd.nix has the rest.
        # THE BEDROOM NETWORK, named rather than spelled: the string lives in
        # ../../network/house-wifi.nix and this says which of the house's
        # networks this radio extends.
        hostapd.ssid = houseWifi.ssids.bedroom;

        # A bridge whose only member has no carrier will otherwise stall
        # network-online.target for the full timeout on a headless box.
        systemd.network.wait-online.ignoredInterfaces = [ "br0" ];
        networking.defaultGateway6 = {
          address = "2403:5813:cd5c:0:120c:6bff:fe12:9ab5";
          interface = "br0";
        };

        # ── OPENED BY HAND ON EVERY WIRE, AND THAT IS THE DEFECT ──
        #
        # `networking.firewall.allowedTCPPorts`/`allowedUDPPorts` is nixpkgs'
        # pseudo-interface `default`: no `-i`, so each of these is reachable
        # from the lan, the tunnel and the access point alike. ../../base/
        # firewall.nix warns about exactly this list on every build, and
        # ../../base/firewall.nix's `socket-exclusions` cannot see any of them
        # -- a hand-written port is outside collision detection as well as
        # outside the carrier model.
        #
        # The replacement for each is `gives.<cap>.ports.<n>.expose =
        # [ "<carrier>" ]` on the aspect that SERVES it, which opens it on that
        # carrier's interface and nowhere else, and gives the port an owner.
        #
        # SIX ARE GONE ALREADY:
        #   25565/tcp, 19132/udp -- minecraft. ./minecraft.nix is entirely
        #     commented out and `services.minecraft-server.enable` renders
        #     `false`, so these were open for a service that does not run.
        #   67/udp, 69/udp -- proxy-DHCP and TFTP, now declared by
        #     ../../services/netbootxyz.nix and derived onto `br0` alone.
        #   8501/tcp -- the local nix cache, now `den.aspects.ncps`
        #     (../../base/caches.nix) on `br0` and `tailscale0`.
        #   51820/udp -- the wireguard hub's endpoint, now declared by
        #     ../../network/vpn-server.nix on `br0`, which is also this box's
        #     uplink.
        #
        # ⚠ WHAT REMAINS SPLITS IN TWO, AND ONLY THE SECOND HALF NEEDS A HUMAN.
        #
        # BLOCKED ON A MISSING CARRIER -- 53, 80, 443, 8080. The resolver, the
        # nginx frontend and its http port have owners in this tree
        # (../../network/roaming-resolver.nix, ../../network/cloudflare.nix,
        # `den.aspects.nginx`), so each COULD be declared. What stops it is
        # that this box is also the wireguard hub and clients browse these over
        # `wg0`: there is no carrier aspect for that tunnel
        # (../../network/vpn.nix declares no `gives`, no `prefix`, no
        # `preference`), so `expose` has no name for it and narrowing to
        # `lan-home`/`tailscale` would silently revoke the tunnel path. Same
        # blocker keeps `services.samba.openFirewall` on in ./samba.nix and
        # `services.openssh`'s 22 global.
        #
        # ⚠ NO OWNER ANYWHERE IN THIS TREE -- 853, 1080, 8081, 21115, 21116,
        # 21117, 21118, 21119 tcp and 68, 853, 21116 udp. Grepping every one
        # finds no other reference. Nothing can say what serves them, which is
        # why they cannot be migrated without someone who knows. They stay
        # open, wrongly, until each is attributed.
        networking.firewall.allowedTCPPorts = [
          53
          80
          443
          853
          1080
          8080
          8081
          21115
          21116
          21117
          21118
          21119
        ];
        networking.firewall.allowedUDPPorts = [
          53
          68
          853
          21116
        ];

        # User
        users.mutableUsers = true;
        users.users.auscyber = {
          isNormalUser = true;
          extraGroups = [
            "wheel"
            "libvirtd"
            "docker"
          ];
        };

        # Base programs
        programs.fish.enable = true;
        programs.nix-ld.enable = true;
        services.logrotate.checkConfig = false;
        services.accounts-daemon.enable = true;

        # Virtualisation
        virtualisation.docker.enable = true;
        virtualisation.libvirtd = {
          enable = true;
          qemu.swtpm.enable = true;
          qemu.runAsRoot = true;
          allowedBridges = [ "br0" ];
        };

        # Nix cache: ncps is configured in aspects/base/caches.nix via
        # `den.aspects.nix.provides.secondpc` (storage.local + the shared
        # `caches` upstream set), not inline here.

        # Extras
        environment.systemPackages = with pkgs; [
          jq
          rclone
          vscode-fhs
          wget
          bind
          ripgrep
          vim
          docker-compose
          python3
          kdePackages.breeze-grub
          qemu
          OVMF
        ];
      };
  };

  den.aspects.auscyber = {
    includes = [ den.aspects.agenix-rekey ];
    provides.secondpc.includes = [
      den.aspects.fish
      den.aspects.neovim
      den.aspects.gpg
      den.aspects.stylix
      den.batteries.primary-user
    ];
  };
}
