# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];
  programs.fish.enable = true;
  auscybernix = {
    nixos.games.enable = true;
    bootlogo.enable = true;
    secrets.enable = true;
  };
  nix.settings.trusted-users = [
    "root"
    "auscyber"
  ];
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  stylix = {
    enable = true;
    image = ../../../backgrounds/phoebebridgers-2.jpg;
    polarity = "dark";
    #    base16Scheme = "${pkgs.base16-schemes}/share/themes/darcula.yaml";
  };
  #  services.tailscale.enable = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.kernelPackages = pkgs.linuxPackages_zen;
  #  boot.extraModulePackages = [ (config.boot.kernelPackages.callPackage ./alx-wol.nix { }) ];
  #  environment.persistence."/persistent" = {
  #    enable = true; # NB: Defaults to true, not needed
  #    hideMounts = true;
  #    files = [ "/etc/ssh/ssh_host_ed25519_key" ];
  #
  #  };

  services.udev.packages = with pkgs; [
    yubikey-personalization
    game-devices-udev-rules
  ];

  services.udev.extraRules = ''
        SUBSYSTEM=="tty", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="316d", GOTO="m1n1"
        GOTO="not_m1n1"
        LABEL="m1n1"
        SUBSYSTEM=="tty", ATTRS{bInterfaceNumber}=="00", KERNEL=="ttyACM*", SYMLINK+="m1n1"
        SUBSYSTEM=="tty", ATTRS{bInterfaceNumber}=="02", KERNEL=="ttyACM*", SYMLINK+="m1n1-sec"
        LABEL="not_m1n1"
    	SUBSYSTEM=="usb", ATTR{idVendor}=="057e", ATTR{idProduct}=="2069", MODE="0666"
  '';
  environment.etc."1password/custom_allowed_browsers" = {
    text = "zen";
    mode = "0755";
  };

  services.ollama = {
    host = "0.0.0.0";
    enable = true;
    openFirewall = true;
    #    acceleration = "cuda";
  };

  security.pam.services = {
    login.u2fAuth = true;
    sudo.u2fAuth = true;
  };

  services.pcscd.enable = true;
  #  home-manager.backupFileExtension = ".bak";
  fileSystems."/mnt/hdd" = {
    device = "/dev/sda3";
    fsType = "ext4";
    #    options = [
    #      "uid=1000"
    #      "gid=100"
    #      "rw"
    #      "user"
    #      "exec"
    #      "umask=000"
    #    ];
  };
  fileSystems."/mnt/ssd2" = {
    device = "/dev/disk/by-uuid/704821B848217DCA";
    fsType = "lowntfs-3g";
    options = [
      "uid=1000"
      "gid=100"
      "rw"
      "user"
      "exec"
      "umask=000"
    ];
  };
  #  programs.virt-manager.enable = true;

  #  users.groups.libvirtd.members = [ "auscyber" ];

  #  virtualisation.libvirtd.enable = true;

  #  virtualisation.spiceUSBRedirection.enable = true;

  boot.kernelModules = [ "kvm-intel" ];
  #boot.extraModulePackages = [ (config.boot.kernel.callPackage ./alx-wol.nix { }) ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = lib.mkForce false;
  #boot.loader.systemd-boot.enable = true;

  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/var/lib/sbctl";
  };
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot";
  };

  networking.hostName = "auspc"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.
  # Set your time zone.
  time.timeZone = "Australia/Melbourne";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_AU.UTF-8";
  console = {
    useXkbConfig = true; # use xkb.options in tty.
  };
  #
  # Enable the X11 windowing system.
  #  services.xserver.enable = true;
  networking.interfaces.enp6s0.wakeOnLan = {
    policy = [
      "magic"
      "broadcast"
      "multicast"
    ];
    enable = true;
  };

  #  networking.interfaces.enp6s0.useDHCP = true;

  # Configure keymap in X11
  #   services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  #   hardware.pulseaudio.enable = true;

  programs.dconf.enable = true;
  programs.zsh.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse = {
      enable = true;
      # package = pkgs.pulseaudioFull;
    };
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;
  #  security.wrappers = {
  #    "1Password-KeyringHelper" = {
  #      source = "${pkgs._1password-gui.out}/share/1password/1Password-KeyringHelper";
  #      setuid = true;
  #      owner = "root";
  #      group = "nobody";
  #    };
  #  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.auscyber = {
    isNormalUser = true;
    description = "Ivy";
    shell = pkgs.fish;
    extraGroups = [
      "video"
      "wheel"
      "input"
      "tty"
      "dialout"
    ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    google-chrome
    #input-leap
    vscode
    sbctl
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    git
  ];

  programs.sway.enable = true;
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  services.displayManager.ly.enable = true;
  #  services.displayManager.sddm.enable = true;
  #  services.displayManager.sddm.wayland.enable = true;
  #  services.displayManager.sddm.enableHidpi = true;
  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.X11Forwarding = true;

  #  services.displayManager.defaultSession = "hyprland";
  #services.displayManager.autoLogin = {
  #  enable = true;
  #  user = "auscyber";
  #};
  services.desktopManager.plasma6.enable = true;

  services.xserver = {
    xkb.layout = "us";
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
    #displayManager.lightdm = {
    #        enable = true;
    #        greeter.enable = true;
    #      };

    displayManager.startx.enable = true;
    videoDrivers = [ "nvidia" ];
    #   videoDrivers = [ "nouveau" ];

    windowManager.awesome = {
      enable = false;
      luaModules = with pkgs.luaPackages; [ luarocks ];
    };
    windowManager.xmonad = {
      enable = false;
      extraPackages =
        haskellPackages: with haskellPackages; [
          xmonad-contrib
          xmonad
        ];

    };
  };
  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    package = pkgs._1password-gui-beta;
    polkitPolicyOwners = [ "auscyber" ];
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 24800 ];
  #   networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  #  programs.hyprland.enable = true;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  #  system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "23.11"; # Did you read the comment?

}
