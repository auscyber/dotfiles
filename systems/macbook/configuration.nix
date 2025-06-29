# darwin.nix

{ pkgs, config, ... }:

{
  # List packages installed in system profile. To search by name, run:
  documentation.enable = true;
  security.pam.services.sudo_local.touchIdAuth = true;
  fonts.packages =
    with pkgs;
    (with nerd-fonts; [
      hack
      roboto-mono
    ]);
  # $ nix-env -qaP | grep wget
  system.primaryUser = "ivypierlot";
  environment.systemPackages = with pkgs; [
    vim
    nodejs
    vscode
    pandoc
    # _1password
    texliveFull
    #    wezterm
    zotero
    qemu
    utm
    gnupg
    prismlauncher
    virt-manager
    #    nixos-conf-editor
  ];
  stylix = {
    enable = false;
    image = ../../backgrounds/phoebebridgers-2.jpg;
  };

  nix.channel.enable = false;
  nix.gc.automatic = true;
  # Auto upgrade nix package and the daemon service.
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.distributedBuilds = true;

  nix.linux-builder.enable = true;
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
  "vivaldi"
  "calibre"
	"ollama"
#    "ollama"
    "beeper"
    "amethyst"
    "steam"
    "notion"
    "google-drive"
	"zen@twilight"
    "arc"
    # "zen-browser"
    "affinity-designer"
    "affinity-publisher"
    "grammarly-desktop"
    "nitro-pdf-pro"
    #  "wezterm@nightly"
    "amethyst"
    "plover"
    "postman"
    "bartender"
    "onedrive"
#    "skype"
    "1password"
    #    "wezterm@nightly"
    "zoom"
  ];
  homebrew.masApps = {
    #    "1Password for Safari" = 1569813296;
    "Microsoft 365" = 1450038993;
    Fantastical = 975937182;
    "Microsoft Teams" = 1113153706;
    "Microsoft Outlook" = 985367838;
  };

  services.yabai = {
    enable = false;
    enableScriptingAddition = true;
    config = {
      focus_follows_mouse = "off";
      mouse_follows_focus = "off";
      window_placement = "second_child";
      window_opacity = "off";
      layout = "bsp";
      top_padding = 10;
      bottom_padding = 10;
      left_padding = 10;
      right_padding = 10;
      window_gap = 10;
    };
    extraConfig = ''
      yabai -m signal --add app='^Ghostty$' event=window_created action='yabai -m space --layout bsp'
      yabai -m signal --add app='^Ghostty$' event=window_destroyed action='yabai -m space --layout bsp'
      	'';

  };
  system.defaults.dock = {
    persistent-apps = [
      "/System/Applications/Launchpad.app"
      "/Applications/Twilight.app"
      "${pkgs.spotify}/Applications/Spotify.app"
      "/Applications/Notion.app"
      "/Applications/Microsoft Outlook.app"
      "/Applications/Fantastical.app"
      "/System/Applications/App Store.app/"
      "/Applications/Microsoft Teams.app"
      "/Applications/Microsoft Word.app"
      "/System/Applications/Messages.app"
      "/Applications/Beeper Desktop.app"
      "/Applications/1Password.app"
      "/System/Applications/System Settings.app"
      "/System/Applications/Home.app"
      "${pkgs.vscode}/Applications/Visual Studio Code.App"
      "${pkgs.ghostty}/Applications/Ghostty.app"
      "${pkgs.zotero}/Applications/Zotero.app"
    ];
  };

  services.sketchybar.enable = true;
  services.sketchybar.extraPackages = with pkgs; [
    jq
    yabai
  ];

  users.users.ivypierlot = {
    name = "ivypierlot";
    home = "/Users/ivypierlot";
    shell = pkgs.zsh;
  };
}
