# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }: {

  nix = {
# Binary Cache for Haskell.nix
binaryCachePublicKeys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    binaryCaches = [
        "https://hydra.iohk.io"
        "https://nix-community.cachix.org"
      ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  nixpkgs.config.allowUnfree = true;
  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # boot.loader.grub.useOSProber = true;
  networking.hostName = "auspc"; # Define your hostname.
  #  networking.wireless = { enable = true;  # Enables wireless support via wpa_supplicant.
  #       networks.NeddySB.pskRaw = "e9331ef6ad7d0a1d67e81afaba284e4544cedb73b33f840c9812fc1991562dcc";
  #  };
  # Set your time zone.
  time.timeZone = "Australia/Melbourne";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  hardware.openrazer.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
  networking.useDHCP = false;
  networking.interfaces.enp4s0.useDHCP = true;
  #  networking.interfaces.wlo1.useDHCP = true;
  networking.wireless.userControlled.enable = true;
  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_AU.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Configure keymap in X11
  # services.xserver.xkbOptions = "eurosign:e";
  fonts.fonts = with pkgs;
    [
      (nerdfonts.override {
        fonts = [ "FiraCode" "Inconsolata" "Hasklig" "RobotoMono" ];
      })
    ];


  # Enable CUPS to print documents.
  # services.printing.enable = true;
  # Enable sound.
  sound.enable = true;
#  hardware.pulseaudio = {
#    enable = true;
#
#    # NixOS allows either a lightweight build (default) or full build of PulseAudio to be installed.
#    # Only the full build has Bluetooth support, so it must be selected here.
#    package = pkgs.pulseaudioFull;
#  };
  #  services.jack = {
  #	jackd.enable = true;
  #
  #	alsa.enable = false;
  #	loopback = {
  #		enable = true;
  #	};
  #  };
  hardware.bluetooth.enable = true;
  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = true;
  users.users.auscyber = {
    isNormalUser = true;
    extraGroups = [
      "jackaudio"
      "audio"
      "libvirtd"
      "plugdev"
      "wheel"
      "docker"
      "video"
    ];
    shell = pkgs.zsh;
  };
  programs.sway = {
    enable = true;

  };
  programs.ssh = {
    startAgent = true;
    extraConfig = "AddKeysToAgent yes";
  };
  security.sudo = {
    enable = true;
    #	extraRules = {

    #	};
  };
  security.wrappers = {
    "1Password-KeyringHelper" = {
      source = "${pkgs._1password-gui.out}/share/1password/1Password-KeyringHelper";
      setuid = true;
      owner = "root";
      group = "nobody";
    };
  };
  programs.zsh.enable = true;
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [ mesa ];
    driSupport32Bit = true;
  };
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    gparted
    lxsession
    wget
    vim
    python3
    #Virtualisation
    breeze-grub
    qemu
    OVMF
    virtmanager
    _1password-gui

    dconf

    #Required for vscode
    nodejs-14_x
  ];
  programs.steam.enable = true;
  programs.dconf.enable = true;

  services = {
    printing = {
      drivers = with pkgs; [cnijfilter_2_80 cnijfilter2 cups-bjnp];
      enable = true;
    };
        mopidy = {
      enable = true;
      extensionPackages = with pkgs; [mopidy-mpd mopidy-spotify mopidy-iris];
      configuration = ''
      '' ;
      #++ builtins.readFile config.age.secrets.spotify_api.path;
    };
    accounts-daemon = {
      enable = true;
    };

    blueman.enable = true;
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse = {
        enable = true;
        # package = pkgs.pulseaudioFull;
      };
    };
    openssh = {
      enable = true;
      forwardX11 = true;
    };
    espanso.enable = true;
    xserver = {
      layout = "us";
      enable = true;
      config = ''

      Section "InputClass"
                  Identifier "My Mouse"
                  MatchIsPointer "yes"
                  Option "AccelerationProfile" "-1"
                  Option "AccelerationScheme" "none"
                  Option "AccelSpeed" "-1"
      EndSection
         '';
      videoDrivers = [ "nvidia" ];
      #   videoDrivers = [ "nouveau" ];
      displayManager.lightdm = {
        enable = true;
        greeter.enable = true;
      };
       desktopManager.plasma5.enable = true;
      #   displayManager.startx.enable = true;
      displayManager.defaultSession = "none+xmonad";
      #   displayManager.autoLogin = {
      #   	enable = true;
      #   	user = "auscyber";
      #   };
      windowManager.awesome = {
        enable = true;
        luaModules = with pkgs.luaPackages; [ luarocks ];
      };
      windowManager.xmonad = {
        enable = true;
        extraPackages = haskellPackages:
          with haskellPackages; [
            xmonad-contrib
            xmonad
          ];

      };
    };
  };
  #  services.cron = {
  #    enable = true;
  #    systemCronJobs = [
  #    "0 0 1-31/2 * *  auscyber . /etc/profile' ${pkgs.bash}/bin/bash /home/auscyber/dotfiles/backup2.sh"
  #   ];
  #  };

  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;
  environment.pathsToLink = [ "/share/agda" ];

  age.secrets.spotify_api.file = ../../secrets/spotify_api.age;
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.

  #services.espanso.enable = true;
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).

  system.stateVersion = "21.05"; # Did you read the comment?
  environment.etc."current-system-packages".text =
    let
      getPackagesNames = builtins.map (p: "${p.name}");
      sysPackages = config.environment.systemPackages;
      homePackages = config.home-manager.users.auscyber.home.packages;
      packages = getPackagesNames (homePackages ++ sysPackages);
      sortedUnique = builtins.sort builtins.lessThan (lib.unique packages);
      formatted = builtins.concatStringsSep "\n" sortedUnique;
    in
    formatted;
}

