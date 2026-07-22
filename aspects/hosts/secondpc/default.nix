{
  den,
  lib,
  ...
}:
{
  flake-file.inputs = {
    celler.url = "github:blitz/celler/main";
    celler.inputs.nixpkgs.follows = "nixpkgs";
    nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
    arion.url = "github:hercules-ci/arion";
    impermanence.url = "github:nix-community/impermanence";
    nix-flatpak.url = "github:gmodena/nix-flatpak/";
  };

  den.hosts.x86_64-linux.secondpc = {
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICj7wlOxTp0NQJoUhRtj7k8gtDC0lCr5MJqLV5LxG9Yf root@kexec-minimal";

    # Advertise as a build machine — auspc/laptop including `den.aspects.builders`
    # will discover this entry and add it to their nix.buildMachines.
    builder = {
      ipAddress = "10.100.0.1";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUNqN3dsT3hUcDBOUUpvVWhSdGo3azhndERDMGxDcjVNSnFMVjVMeEc5WWY=";
      systems = [ "x86_64-linux" ];
      maxJobs = 5;
      speedFactor = 5;
      features = [
        "big-parallel"
        "cached-compilation"
        "kvm"
      ];
      sshUser = "builder";
    };

    users.auscyber = {
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA/+SE8omiIJ7VBQKgMLsgyADUVtY37o0kn0g0uwQVKi auscyber@secondpc";
    };
  };

  den.aspects.secondpc = {
    includes = [
      den.aspects.nginx
      den.aspects.nix
      den.aspects.local
      den.aspects.vpn-server
      den.aspects.builders
      den.aspects.builder-server
      den.aspects.disko
      den.aspects.facter
      den.aspects.secondpc-web
      den.aspects.searchix
      (den.batteries.unfree [ "intel-ocl" ])
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
        networking.bridges.br0.interfaces = [ "enp2s0" ];
        networking.interfaces.br0.useDHCP = false;
        networking.interfaces.br0.ipv4.addresses = [
          {
            address = "192.168.0.26";
            prefixLength = 24;
          }
        ];
        networking.defaultGateway = {
          address = "192.168.0.1";
          interface = "br0";
        };
        networking.nameservers = [ "1.1.1.1" ];

        # A bridge whose only member has no carrier will otherwise stall
        # network-online.target for the full timeout on a headless box.
        systemd.network.wait-online.ignoredInterfaces = [ "br0" ];
        networking.defaultGateway6 = {
          address = "2403:5813:cd5c:0:120c:6bff:fe12:9ab5";
          interface = "br0";
        };

        networking.firewall.allowedTCPPorts = [
          53
          80
          443
          853
          1080
          8080
          8081
          8096
          8501
          21115
          21116
          21117
          21118
          21119
          25565
        ];
        networking.firewall.allowedUDPPorts = [
          53
          67
          68
          69
          853
          1900
          7359
          19132
          21116
          51820
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

        # Time machine + zeroconf
        services.netatalk = {
          enable = true;
          settings = {
            Homes = {
              "basedir regex" = "/home";
              path = "netatalk";
            };
            time-machine = {
              path = "/timemachine";
              "valid users" = "ivypierlot";
              "time machine" = true;
            };
          };
        };
        services.avahi = {
          enable = true;
          nssmdns = true;
          publish = {
            enable = true;
            userServices = true;
          };
        };

        # Samba shares
        services.samba = {
          enable = true;
          securityType = "user";
          openFirewall = true;
          settings = {
            global = {
              "workgroup" = "WORKGROUP";
              "server string" = "smbnix";
              "netbios name" = "smbnix";
              "security" = "user";
              "hosts allow" = "192.168.0. 127.0.0.1 localhost 100.64.0.";
              "hosts deny" = "0.0.0.0/0";
              "guest account" = "nobody";
              "map to guest" = "bad user";
            };
            hdd = {
              "path" = "/mnt/hdd";
              "browseable" = "yes";
              "read only" = "no";
              "guest ok" = "no";
              "create mask" = "0644";
              "directory mask" = "0755";
              "force group" = "music";
            };
          };
        };

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
