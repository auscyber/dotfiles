# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }: {

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # boot.loader.grub.useOSProber = true;
  networking.hostName = "secondpc"; # Define your hostname.
  #  networking.wireless = { enable = true;  # Enables wireless support via wpa_supplicant.
  #       networks.NeddySB.pskRaw = "e9331ef6ad7d0a1d67e81afaba284e4544cedb73b33f840c9812fc1991562dcc";
  #  };
  # Set your time zone.
  time.timeZone = "Australia/Melbourne";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  hardware.openrazer.enable = true;
  networking.useDHCP = false;
  networking.bridges = {
    "br0" = {
      interfaces = [ "enp2s0" ];
    };
  };
  #
  networking.interfaces.br0.useDHCP = false;
  networking.interfaces.br0.ipv4.addresses = [{
    address = "192.168.0.26";
    prefixLength = 24;
  }];
  networking.defaultGateway = "192.168.0.1";
  networking.nameservers = [ "1.1.1.1" ];
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 25565 8080 21115 21118 21119 21116 21117 80 53 443 1080 8096 853 8081 ];
  networking.firewall.allowedUDPPorts = [ 51820 21116 53 67 1900 7359 853 69 68 ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;


  #  networking.interfaces.wlo1.useDHCP = true;
  networking.wireless.userControlled.enable = false;
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
  services.espanso.enable = false;
  #fonts.fonts = with pkgs;
  #  [
  #    (nerdfonts.override {
  #      fonts = [ "FiraCode" "Inconsolata" "Hasklig" "RobotoMono" ];
  #    })
  #  ];

  services.accounts-daemon = {
    enable = true;
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  #services.blueman.enable = true;
  # Enable sound.
  sound.enable = false;
  hardware.pulseaudio = {
    enable = false;

    # NixOS allows either a lightweight build (default) or full build of PulseAudio to be installed.
    # Only the full build has Bluetooth support, so it must be selected here.
    package = pkgs.pulseaudioFull;
  };
  #  services.jack = {
  #	jackd.enable = true;
  #
  #	alsa.enable = false;
  #	loopback = {
  #		enable = true;
  #	};
  #  };
  hardware.bluetooth.enable = false;
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
    enable = false;
  };
  security.sudo = {
    enable = true;
    #	extraRules = {

    #	};
  };
  programs.zsh.enable = true;
  hardware.opengl.enable = true;
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vscode-fhs
    wget
    wireshark
    bind
    ripgrep
    vim
    docker-compose
    python3
    #Virtualisation
    breeze-grub
    qemu
    OVMF
    virt-manager
    dconf

    #Required for vscode
    #nodejs-14_x
  ];
  programs.nix-ld.enable = true;
  programs.dconf.enable = false;

  #  services.xserver = {
  #    layout = "us";
  #    enable = true;
  #    config = ''
  #
  #      Section "InputClass"
  #                  Identifier "My Mouse"
  #                  MatchIsPointer "yes"
  #                  Option "AccelerationProfile" "-1"
  #                  Option "AccelerationScheme" "none"
  #                  Option "AccelSpeed" "-1"
  #      EndSection
  #         '';
  #    videoDrivers = [ "intel" ];
  #    #   videoDrivers = [ "nouveau" ];
  #    displayManager.lightdm = {
  #      enable = true;
  #      greeter.enable = true;
  #      background = "/usr/share/pixmaps/background1.jpg";
  #    };
  ##    desktopManager.plasma5.enable = true;
  #    #   displayManager.startx.enable = true;
  #    displayManager.defaultSession = "none+xmonad";
  #    #   displayManager.autoLogin = {
  #    #   	enable = true;
  #    #   	user = "auscyber";
  #    #   };
  #    windowManager.awesome = {
  #      enable = true;
  #      luaModules = with pkgs.luaPackages; [ luarocks ];
  #    };
  #    windowManager.xmonad = {
  #      enable = true;
  #      extraPackages = haskellPackages:
  #        with haskellPackages; [
  #          xmonad-contrib
  #          xmonad
  #        ];
  #
  #    };
  #  };
  hardware.opengl.driSupport32Bit = true;
  #  services.cron = {
  #    enable = true;
  #    systemCronJobs = [
  #    "0 0 1-31/2 * *  auscyber . /etc/profile' ${pkgs.bash}/bin/bash /home/auscyber/dotfiles/backup2.sh"
  #   ];
  #  };

  virtualisation.docker = {
    enable = true;
    liveRestore = false;
  };
  virtualisation.libvirtd = {
    qemu = {
      ovmf.enable = true;
      runAsRoot = false;
    };
    allowedBridges = [ "br0" ];
    enable = true;
  };
  environment.pathsToLink = [ "/share/agda" ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    forwardX11 = true;
  };
  #services.espanso.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
  environment.etc."current-system-packages".text =
    let
      packages = builtins.map (p: "${p.name}") config.environment.systemPackages;
      sortedUnique = builtins.sort builtins.lessThan (lib.unique packages);
      formatted = builtins.concatStringsSep "\n" sortedUnique;
    in
    formatted;
}

