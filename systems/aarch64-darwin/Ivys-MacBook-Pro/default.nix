# darwin.nix

{
  pkgs,
  config,
  inputs,
  flakeSelf,
  ...
}:

{
  # List packages installed in system profile. To search by name, run:
  auscybernix.nix.caches = true;
  auscybernix.meta.description = "M4 Macbook Pro 2024";

  nix.settings = {
    substituters = [
      "https://cache.nixos.org"
                  "http://10.100.0.1:8501"
      #"https://auscyber.cachix.org"
    ];
    trusted-public-keys = [
                  "secondpc:cac96M9YXnt/U1UEQuu+g/Pfgblsqo+Q1ewcr3AuGr4="
    ];
  };
  #  sops.age.plugins = with pkgs; [
  #    age-plugin-1p
  #    age-plugin-se
  #  ];

  documentation.enable = true;
  stylix.targets.jankyborders.enable = false;
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
    #ollama
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
    thunderbird-latest-bin-unwrapped
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
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICTsjq9lMzer6RPeDfXZ9eI1eiMf8b/fteSOb5XC5rBG";

  nix.channel.enable = false;
  nix.gc.automatic = true;
  # Auto upgrade nix package and the daemon service.
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.distributedBuilds = true;

  nix.linux-builder = {
    enable = true;
	ephemeral = true;
    systems = [
      "x86_64-linux"
      "aarch64-linux"
    ];
    config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
	config.users.users.builder.openssh.authorizedKeys.keyFiles = flakeSelf.auscybernix.builders.sshKeys;
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
    nix.flake = "/Users/ivypierlot/dotfiles";
    sudo.pam = {
      enable = true;
      touchIdAuth = true;

    };
    sudo.agents.enable = true;
    keybinds.karabiner-driver-kit = {
      enable = true;
      package = pkgs.kanata.darwinDriver;
    };
	vpn.enable = true;
	nix.builders = {
	enable = true;
	builderConfig = {
	enable = true;
	maxJobs = 4;

	  systems = [ "aarch64-darwin" ];

	};

	};

    homebrew = {
      enable = true;

      brews = [
        "nowplaying-cli"
        "mole"
        "speedtest"
      ];
      casks = [
        # "cinny"
        "mark-text"
        "craft"
        "raycast"
        #    "vivaldi"
        "calibre"
        #    "ollama"
		"sf-symbols" "font-sketchybar-app-font" "font-sf-pro"
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
        #        "nitro-pdf-pro"
        #  "wezterm@nightly"
        #      "amethyst"
        "helium-browser"
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
        #  "Microsoft Word" = 462054704;
        Fantastical = 975937182;
        # "Microsoft Outlook" = 985367838;
      };
    };
  };

  programs._1password-gui = {
    enable = true;
    package = pkgs._1password-gui-beta;
  };
  system.defaults.dock = {
    persistent-apps = [
      #      "/System/Applications/Apps.app"
      "/Applications/Nix Apps/Zen.app"
      #      "/Applications/TIDAL.app"
      #      "/Applications/Musly.app"
      "/Applications/Microsoft Outlook.app"
      "/Applications/Fantastical.app"
      #      "/System/Applications/App Store.app/"
      "/Applications/Microsoft Teams.app"
      "/Applications/Notion.app"
      "/Applications/Microsoft Word.app"
      "/System/Applications/Messages.app"
      "/Applications/Beeper Desktop.app"
      "/Applications/1Password.app"
      "/System/Applications/System Settings.app"
      #      "/System/Applications/Home.app"
      #      "/Applications/Nix Apps/Zed.App"
      "/Applications/Nix Apps/Visual Studio Code.app"
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
