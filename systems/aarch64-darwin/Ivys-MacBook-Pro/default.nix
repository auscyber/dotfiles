# darwin.nix

{
  pkgs,
  config,
  inputs,
  ...
}:

{
  # List packages installed in system profile. To search by name, run:
  auscybernix.nix.caches = false;
  nix.settings = {
    substituters = [
      "https://cache.nixos.org"
      "http://secondpc.devices.imflo.pet:8501"
    ];
    trusted-public-keys = [
      "secondpc:cac96M9YXnt/U1UEQuu+g/Pfgblsqo+Q1ewcr3AuGr4="
    ];
  };

  documentation.enable = true;
  stylix.targets.jankyborders.enable = false;
  security.pam.services.sudo_local.touchIdAuth = true;
  fonts.packages =
    with pkgs;
    (with nerd-fonts; [
      hack
      roboto-mono
    ]);
  programs.zsh = {
    enable = true;
    loginShellInit = ''
      	  . /etc/zprofile
    '';
  };

  #  services.pia-wireguard = {
  #    enable = true;
  #    tokenFile = config.age.secrets.pia_password.path;
  #  };

  # $ nix-env -qaP | grep wget
  system.primaryUser = "ivypierlot";
  environment.systemPackages = with pkgs; [
    ollama
    #    bartender
    zen-browser
    ghostty
    #neovide
    obsidian
    hln
    vim
    nodejs
    discord
    vscode
    pandoc
    # _1password
    texliveFull
    #    wezterm
    zotero
    qemu
    gnupg
    #    prismlauncher
    #    zen-browser
    #    nixos-conf-editor
  ];
  stylix = {
    enable = true;
    polarity = "dark";
    image = ../../../backgrounds/phoebebridgers-2.jpg;
  };

  nix.channel.enable = false;
  nix.gc.automatic = true;
  # Auto upgrade nix package and the daemon service.
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.distributedBuilds = true;

  nix.linux-builder = {
    enable = true;
    systems = with inputs.flake-utils.lib.system; [
      aarch64-linux
    ];
    #config.nix.settings.extra-trusted-public-keys = [
    #  "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="

    #];
    #config.nix.settings.trusted-users = [ "builder" ];
    #    config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
  };

  nix.settings = {
    trusted-users = [
      "ivypierlot"
      "root"
    ];
    experimental-features = "nix-command flakes";
  };
  # Create /etc/zshrc that loads the nix-darwin environment.
  #  programs.zsh.enable = true; # default shell on catalina
  programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
  nix.enable = true;
  # The platform the configuration will be used on.
  services.karabiner-elements.enable = false;
  auscybernix = {
    sudo.agents.enable = true;
    keybinds.karabiner-driver-kit.enable = true;

    homebrew = {
      enable = true;

      brews = [ "nowplaying-cli" ];
      casks = [
        # "cinny"
        "mark-text"
        "craft"
        "raycast"
        #    "vivaldi"
        "calibre"
        #    "ollama"
        "todoist-app"

        "beeper"
        "amethyst"
        "steam"
        #    "notion"
        "google-drive"
        "tailscale"
        #    "arc"
        #      "zen@twilight"
        "affinity-designer"
        "affinity-publisher"
        #    "grammarly-desktop"
        "nitro-pdf-pro"
        #  "wezterm@nightly"
        #      "amethyst"
        "plover"
        "postman"
        #      "bartender"
        #    "skype"
        #      "1password"
        "tidal"
        #    "wezterm@nightly"
        "zoom"
        "microsoft-teams"
      ];
      masApps = {
        #    "1Password for Safari" = 1569813296;
        #    "Microsoft 365" = 1450038993;
        "Microsoft Word" = 462054704;
        Fantastical = 975937182;
        "Microsoft Outlook" = 985367838;
      };
    };
  };

  programs._1password-gui = {
    enable = true;
    package = pkgs._1password-gui-beta;
  };
  system.defaults.dock = {
    persistent-apps = [
      "/System/Applications/Apps.app"
      "/Applications/Nix Apps/Zen.app"
      "/Applications/TIDAL.app"
      "/Applications/Microsoft Outlook.app"
      "/Applications/Fantastical.app"
      #      "/System/Applications/App Store.app/"
      "/Applications/Microsoft Teams.app"
      "/Applications/Craft.app"
      "/Applications/Microsoft Word.app"
      "/System/Applications/Messages.app"
      "/Applications/Beeper Desktop.app"
      "/Applications/1Password.app"
      "/System/Applications/System Settings.app"
      #      "/System/Applications/Home.app"
      "/Applications/Nix Apps/Zed.App"
      "/Applications/Nix Apps/Ghostty.app"
      "/Applications/Nix Apps/Zotero.app"
      "/Applications/Todoist.app"
    ];
  };

  environment.shells = [
    pkgs.bash
    pkgs.zsh
    pkgs.fish
  ];
  users.users.ivypierlot = {
    name = "ivypierlot";
    home = "/Users/ivypierlot";
    shell = pkgs.fish;
  };
}
