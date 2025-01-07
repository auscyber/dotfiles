# darwin.nix

{ pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  documentation.enable = true;
  security.pam.enableSudoTouchIdAuth = true;
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
    nodejs
    vscode
    wezterm
    zotero
    gnupg
    prismlauncher
  ];

  nix.channel.enable = false;
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
  homebrew.enable = true;
  homebrew.onActivation = {
    autoUpdate = true; # Fetch the newest stable branch of Homebrew's git repo
    upgrade = true; # Upgrade outdated casks, formulae, and App Store apps
    # 'zap': uninstalls all formulae(and related files) not listed in the generated Brewfile
    cleanup = "zap";
  };

  homebrew.casks = [
    "beeper"
    "amethyst"
    "steam"
    "notion"
    "google-drive"
    "spotify"
    "arc"
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
      "/System/Applications/App Store.app/"
      "/System/Applications/Messages.app"
      "/Applications/Beeper.app"
      "/Applications/1Password.app"
      "/System/Applications/System Settings.app"
      "/System/Applications/Home.app"
      "${pkgs.vscode}/Applications/Visual Studio Code.App"
      "${pkgs.wezterm}/Applications/Wezterm.App"
      "${pkgs.zotero}/Applications/Zotero.app"
    ];
  };

  users.users.ivypierlot = {
    name = "ivypierlot";
    home = "/Users/ivypierlot";
    shell = pkgs.zsh;
  };
}
