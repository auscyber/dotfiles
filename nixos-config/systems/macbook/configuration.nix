# darwin.nix

{ pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  documentation.enable = true;
  security.pam.services.sudo_local.touchIdAuth = true;
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
    nodejs
    vscode
    pandoc
    texliveFull
    #    wezterm
    zotero
    gnupg
    prismlauncher
    virt-manager
  ];

  nix.channel.enable = false;
  # Auto upgrade nix package and the daemon service.
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.distributedBuilds = true;
  nix.buildMachines = [{
    hostName = "192.168.0.26";
    system = "x86_64-linux";
    protocol = "ssh";
    supportedFeatures = [ ];
  }];

  nix.settings = {

    experimental-features = "nix-command flakes";

  };
  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
  nix.enable = true;
  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
  homebrew.enable = true;
  homebrew.onActivation = {
    autoUpdate = true; # Fetch the newest stable branch of Homebrew's git repo
    upgrade = true; # Upgrade outdated casks, formulae, and App Store apps
    # 'zap': uninstalls all formulae(and related files) not listed in the generated Brewfile
    cleanup = "zap";
  };
  homebrew.brews = [ "nowplaying-cli" ];
  homebrew.casks = [
    "beeper"
    "amethyst"
    "steam"
    "notion"
    "google-drive"
    "spotify"
    "arc"
    "zen-browser"
    "discord"
    "affinity-designer"
    "affinity-publisher"
    "grammarly-desktop"
    "visual-studio-code"
    "nitro-pdf-pro"
    "amethyst"
    "1password"
    "plover"
    "postman"
    "bartender"
    "onedrive"
    "skype"
    "wezterm@nightly"
    "zoom"
  ];
  homebrew.masApps = {
    "1Password for Safari" = 1569813296;
    "Microsoft 365" = 1450038993;
    Fantastical = 975937182;
    "Microsoft Teams" = 1113153706;
    "Microsoft Outlook" = 985367838;
  };
  system.defaults.NSGlobalDomain = {
    AppleInterfaceStyle = "Dark"; # dark mode
    AppleShowAllFiles = true;
    ApplePressAndHoldEnabled = false; # enable press and hold

    # If you press and hold certain keyboard keys when in a text area, the key’s character begins to repeat.
    # This is very useful for vim users, they use `hjkl` to move cursor.
    # sets how long it takes before it starts repeating.
    InitialKeyRepeat = 10; # normal minimum is 15 (225 ms), maximum is 120 (1800 ms)
    # sets how fast it repeats once it starts.
    KeyRepeat = 3; # normal minimum is 2 (30 ms), maximum is 120 (1800 ms)
  };
  services.yabai = {
    enable = false;
    enableScriptingAddition = true;
    config = {
      focus_follows_mouse = "autoraise";
      mouse_follows_focus = "off";
      window_placement = "second_child";
      window_opacity = "off";
      top_padding = 36;
      bottom_padding = 10;
      left_padding = 10;
      right_padding = 10;
      window_gap = 10;
    };

  };
  system.defaults.dock = {
    persistent-apps = [
      "/System/Applications/Launchpad.app"
      "/Applications/Arc.app"
      "/Applications/Spotify.app"
      "/Applications/Notion.app"
      "/Applications/Microsoft Outlook.app"
      "/Applications/Fantastical.app"
      #      "/System/Applications/App Store.app/"
      "/Applications/Microsoft Teams.app"
      "/Applications/Microsoft Word.app"
      "/System/Applications/Messages.app"
      "/Applications/Beeper Desktop.app"
      "/Applications/1Password.app"
      "/System/Applications/System Settings.app"
      "/System/Applications/Home.app"
      "${pkgs.vscode}/Applications/Visual Studio Code.App"
      "/Applications/Wezterm.app"
      #      "${pkgs.wezterm}/Applications/Wezterm.app"
      "${pkgs.zotero}/Applications/Zotero.app"
    ];
  };

  users.users.ivypierlot = {
    name = "ivypierlot";
    home = "/Users/ivypierlot";
    shell = pkgs.zsh;
  };
}
