{ den, lib, ... }:
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

    users.auscyber = { };
  };

  den.aspects.secondpc = {
    includes = [
      den.aspects.nginx
      den.aspects.nix
      den.aspects.local
      den.aspects.vpn-server
      den.aspects.builders
      den.aspects.builder-server
    ];

    nixos =
      { config, pkgs, ... }:
      {
        # Host identity / boot
        networking.hostId = "4f6f802e";

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.loader.efi.efiSysMountPoint = "/boot/efi";

        boot.supportedFilesystems = [ "zfs" ];
        boot.zfs.extraPools = [
          "zroot"
          "zpool"
        ];

        boot.kernel.sysctl = {
          "net.ipv6.conf.all.forwarding" = 1;
          "net.ipv6.conf.all.accept_ra" = 2;
          "net.ipv6.conf.all.accept_redirects" = 1;
          "net.ipv6.conf.all.accept_source_route" = 1;
        };

        fileSystems."/mnt/hdd" = {
          device = "zpool/root";
          fsType = "zfs";
        };

        # Networking: bridge + static ipv4
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
        networking.defaultGateway = "192.168.0.1";
        networking.nameservers = [ "1.1.1.1" ];
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
          shell = pkgs.fish;
        };

        # Base programs
        programs.fish.enable = true;
        programs.nix-ld.enable = true;
        services.logrotate.checkConfig = false;
        services.accounts-daemon.enable = true;

        # Stylix

        # OpenSSH (extends what openssh aspect already enables)

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

        # Hardware accel (intel)
        hardware.graphics = {
          enable = true;
          extraPackages = with pkgs; [
            intel-ocl
            intel-vaapi-driver
            libva-vdpau-driver
          ];
        };
        systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "i965";
        environment.sessionVariables.LIBVA_DRIVER_NAME = "i965";

        # Media + downloading
        services.jellyfin = {
          enable = true;
          openFirewall = true;
        };
        services.audiobookshelf.enable = true;
        users.groups.music = { };
        users.users.music = {
          isSystemUser = true;
          group = "music";
        };
        services.navidrome = {
          enable = true;
          group = "music";
          settings = {
            user = "music";
            MusicFolder = "/mnt/hdd/Music";
          };
        };
        services.lidarr = {
          enable = true;
          user = "music";
        };
        services.slskd = {
          enable = true;
          openFirewall = true;
          user = "music";
          settings = {
            shares.directories = [ "/mnt/hdd/Music" ];
            directories.downloads = "/mnt/hdd/Music/Downloads";
            web.ip_address = "0.0.0.0";
            web.logging = true;
          };
        };
        services.qbittorrent = {
          enable = true;
          webuiPort = 9090;
          openFirewall = true;
        };

        # Vaultwarden
        services.vaultwarden = {
          enable = true;
          backupDir = "/var/local/vaultwarden/backup";
          config = {
            DOMAIN = "https://bitwarden.ivymect.in";
            SIGNUPS_ALLOWED = false;
            ROCKET_ADDRESS = "127.0.0.1";
            ROCKET_PORT = 8222;
            ROCKET_LOG = "critical";
            SMTP_HOST = "127.0.0.1";
            SMTP_PORT = 25;
            SMTP_SSL = false;
            SMTP_FROM = "admin@bitwarden.ivymect.in";
            SMTP_FROM_NAME = "ivymect.in Bitwarden server";
          };
        };

        # Minecraft
        services.minecraft-server = {
          enable = true;
          eula = true;
          openFirewall = true;
          declarative = true;
          jvmOpts = "-Xmx4096M -Xms2048M";
          serverProperties = {
            server-port = 25565;
            max-players = 20;
            motd = "Super minecraft";
          };
        };

        # Jitsi-meet
        services.jitsi-meet = {
          enable = true;
          hostName = "meet.ivymect.in";
          nginx.enable = true;
          config = {
            enableWelcomePage = false;
            prejoinPageEnabled = true;
            defaultLang = "en";
          };
          interfaceConfig = {
            SHOW_JITSI_WATERMARK = false;
            SHOW_WATERMARK_FOR_GUESTS = false;
          };
        };
        services.jitsi-videobridge.openFirewall = true;

        # Nix cache: ncps for upstream caching
        services.ncps = {
          enable = true;
          cache = {
            hostName = "secondpc";
            dataPath = "/mnt/hdd/ncps";
            maxSize = "200G";
            lru.schedule = "0 2 * * *";
            allowPutVerb = true;
            allowDeleteVerb = true;
          };
          server.addr = "0.0.0.0:8501";
          upstream = {
            caches = [
              "https://nix-community.cachix.org"
              "https://iohk.cachix.org"
              "https://cache.nixos.org"
              "https://devenv.cachix.org"
              "https://auscyber.cachix.org"
            ];
            publicKeys = [
              "auscyber.cachix.org-1:RPlENxXc/irvLimM0Yz8Au3ntk/sxZ8bwXPwuXL3v5c="
              "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
              "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
              "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
              "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
            ];
          };
          prometheus.enable = true;
        };

        # Observability stack
        services.prometheus = {
          enable = true;
          port = 9091;
          extraFlags = [ "--web.enable-remote-write-receiver" ];
          exporters.node = {
            enable = true;
            port = 9000;
            enabledCollectors = [ "systemd" ];
          };
          globalConfig.scrape_interval = "10s";
          scrapeConfigs = [
            {
              job_name = "node";
              static_configs = [
                { targets = [ "localhost:${toString config.services.prometheus.exporters.node.port}" ]; }
              ];
            }
            {
              job_name = "ncps";
              static_configs = [
                { targets = [ "localhost:8501" ]; }
              ];
            }
          ];
        };
        services.loki = {
          enable = true;
          configFile = ../../packages/secondpc/loki.yaml;
        };

        # Extras
        environment.systemPackages = with pkgs; [
          jellyfin
          jellyfin-web
          jellyfin-ffmpeg
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

    secrets."bob".generator.script = "alnum";
  };
}
