{ den, __findFile, ... }:
{
  den.hosts.x86_64-linux.auspc = {
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFYM1mZ3fYfOjhyMhIiKbUOLYTQifG82P2NnGWHyIwHt root@nixos";
    gpu = "nvidia";
    roles = [
      "gui"
      "gaming"
      "dev"
    ];

    # auspc has 4 cores reserved for builds and is the fastest box.
    builder = {
      ipAddress = "10.100.0.2";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUZZTTFtWjNmWWZPamh5TWhJaUtiVU9MWVRRaWZHODJQMk5uR1dIeUl3SHQgcm9vdEBuaXhvcw==";
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
    users.auscyber = {
      wallpaper = ../../backgrounds/phoebebridgers-2.jpg;
      roles = [
        "gui"
        "gaming"
        "dev"
      ];
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMOJWDxkmOeVGAq7WcPI+BygJ2zbsn4J0UAq0R6B6ZVx auscyber@auspc";
    };
  };

  den.aspects.auspc = {
    includes = [
      den.aspects.vpn
      den.aspects.packages.alx-wol
      den.aspects.bootlogo
      den.aspects.builders
      den.aspects.builder-server
      den.aspects.secure-boot
      (den.batteries.unfree [ "castlabs-electron" ])
      # gaming content is delivered via the `gaming` role (see gaming.nix +
      # roles.nix); no explicit include needed.
    ];

    nixos =
      { pkgs, config, ... }:
      {

        boot.extraModulePackages = with config.boot.kernelPackages; [ alx-wol ];
        networking.hostName = "auspc";
        networking.hostId = "230c61e9";
        networking.networkmanager.enable = true;
        time.timeZone = "Australia/Melbourne";
        i18n.defaultLocale = "en_AU.UTF-8";

        boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
        boot.supportedFilesystems = [
          "zfs"
          "ntfs"
        ];
        nixpkgs.config.allowUnfree = true;
        boot.zfs.requestEncryptionCredentials = true;
        boot.zfs.forceImportRoot = true;

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
        };

        programs.dconf.enable = true;
        programs.zsh.enable = true;
        programs.kdeconnect.enable = true;
        programs._1password.enable = true;
        programs._1password-gui = {
          enable = true;
          polkitPolicyOwners = [ "auscyber" ];
        };

        services.desktopManager.plasma6.enable = true;
        services.displayManager.ly.enable = true;

        users.users.auscyber = {
          isNormalUser = true;
          description = "Ivy";
          extraGroups = [
            "video"
            "wheel"
            "input"
            "tty"
            "dialout"
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

        swapDevices = [ ];
      };
  };

  den.aspects.auscyber = {
    includes = [
      den.aspects.fish
      (den.batteries.unfree [ "castlabs-electron" ])
    ];

    provides.auspc = {
      includes = [
        den.batteries.primary-user
        den.aspects.zotero
        <browsers/helium>
        den.aspects.gui
        den.aspects.ghostty
        den.aspects.neovim
        den.aspects.gpg
        den.aspects.nushell
        den.aspects.dev
      ];

      provides.to-users.homeManager =
        { pkgs, ... }:
        {
          home.sessionVariables.SSH_AUTH_SOCK = "$HOME/.1password/agent.sock";

          services.gpg-agent = {
            enable = true;
            enableSshSupport = true;
            pinentry.program = pkgs.pinentry-qt;
            extraConfig = "allow-loopback-pinentry";
          };

          home.packages = with pkgs; [
            obs-studio
            heroic
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
            nitrogen
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
