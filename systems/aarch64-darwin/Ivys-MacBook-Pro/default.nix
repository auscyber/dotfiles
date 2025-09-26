# darwin.nix

{
  pkgs,
  config,
  inputs,
  ...
}:

{
  # List packages installed in system profile. To search by name, run:

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

  services.jankyborders = {
    enable = true;
    active_color = "0xff${config.stylix.base16Scheme.base03}";
    inactive_color = "0xff${config.stylix.base16Scheme.base0D}";
    style = "round";
    blur_radius = 5.0;
    width = 6.0;
    ax_focus = true;
  };
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
    prismlauncher
    virt-manager
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
    enable = false;
    systems = with inputs.flake-utils.lib.system; [
      aarch64-linux
      x86_64-linux
    ];
    config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
  };
  nix.settings = {

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
    keybinds.karabiner-driver-kit.enable = true;

    homebrew = {
      enable = true;

      brews = [ "nowplaying-cli" ];
      casks = [
        "raycast"
        "anytype@alpha"
        #    "vivaldi"
        "calibre"
        "zoom"
        #    "ollama"
        "todoist-app"

        "beeper"
        "amethyst"
        "steam"
        #    "notion"
        "google-drive"
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

  services.yabai = {
    enable = true;
    enableScriptingAddition = true;
    config = {
      focus_follows_mouse = "off";
      mouse_follows_focus = "off";
      window_placement = "second_child";
      window_opacity = "off";
      external_bar = "all:40:0";
      layout = "bsp";
      top_padding = 10;
      bottom_padding = 6;
      left_padding = 10;
      right_padding = 10;
      window_gap = 10;
    };
    extraConfig = ''
            yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
            sudo yabai --load-sa
                              yabai -m rule --add app="^System Preferences$" manage=off
                              yabai -m signal --add app='^Ghostty$' event=window_created action='yabai -m space --layout bsp'
                              yabai -m signal --add app='^Ghostty$' event=window_destroyed action='yabai -m space --layout bsp'
      						yabai -m rule --add app="^1Password$"  scratchpad=1password grid=11:11:1:1:9:9
                              yabai -m rule --apply app="^1Password$"  scratchpad=1password grid=11:11:1:1:9:9
                              yabai -m rule --add app="^Discord$" title!="^Discord Updater$" scratchpad=discord grid=11:11:1:1:9:9
                              yabai -m rule --apply app="^Discord$" title!="^Discord Updater$" scratchpad=discord grid=11:11:1:1:9:9
                              yabai -m rule --add app="^Beeper$" title!="^Beeper Updater$" scratchpad=beeper grid=11:11:1:1:9:9
                              yabai -m rule --apply app="^Beeper$" title!="^Beeper Updater$" scratchpad=beeper grid=11:11:1:1:9:9

                                      	'';

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
      "/Applications/Nix Apps/Obsidian.app"
      "/Applications/Microsoft Outlook.app"
      "/Applications/Fantastical.app"
      #      "/System/Applications/App Store.app/"
      "/Applications/Microsoft Teams.app"
      "/Applications/Microsoft Word.app"
      "/System/Applications/Messages.app"
      "/Applications/Beeper Desktop.app"
      "/Applications/1Password.app"
      "/System/Applications/System Settings.app"
      #      "/System/Applications/Home.app"
      "/Applications/Nix Apps/Visual Studio Code.App"
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
