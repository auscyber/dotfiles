# darwin.nix

{ pkgs, config, ... }:

{
  # List packages installed in system profile. To search by name, run:
  documentation.enable = true;
  security.pam.services.sudo_local.touchIdAuth = true;
  fonts.packages = with pkgs; (with nerd-fonts; [ hack roboto-mono ]);
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
    image = ../../boygenius-performs-gq.jpg;
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

    "beeper"
    "amethyst"
    "steam"
    "notion"
    "google-drive"
    "arc"
    "zen-browser"
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
    "skype"
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
    enable = true;
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
      "/Applications/Arc.app"
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
      #      "${pkgs.wezterm}/Applications/Wezterm.app"
      "${pkgs.zotero}/Applications/Zotero.app"
    ];
  };
  services.skhd = {
    enable = true;
    skhdConfig =
      let
        input = {
          # focus window
          "alt - h" = " yabai -m window --focus west";

          # swap managed window
          "shift + alt - h " = "yabai -m window --swap north";

          # move managed window
          "shift + cmd - h " = "yabai -m window --warp east";

          # balance size of windows
          "shift + alt - 0 " = "yabai -m space --balance";

          # make floating window fill screen
          "shift + alt - up     " = "yabai -m window --grid 1:1:0:0:1:1";

          # make floating window fill left-half of screen
          "shift + alt - left   " = "yabai -m window --grid 1:2:0:0:1:1";

          # create desktop, move window and follow focus - uses jq for parsing json (brew install jq)
          "shift + cmd - n" = ''yabai -m space --create && \
                         index="$(yabai -m query --spaces --display | jq 'map(select(."is-native-fullscreen" == false))[-1].index')" && \
                         yabai -m window --space "''${index}" && \
                         yabai -m space --focus "''${index}"
						 '';

          # fast focus desktop
          "cmd + alt - x " = " yabai -m space --focus recent";
          "cmd + alt - 1 " = " yabai -m space --focus 1";

          # send window to desktop and follow focus
          # shift + cmd - z : yabai -m window --space next; yabai -m space --focus next
          # shift + cmd - 2 : yabai -m window --space  2; yabai -m space --focus 2

          # focus monitor
          "ctrl + alt - z  " = " yabai -m display --focus prev";
          "ctrl + alt - 3  " = " yabai -m display --focus 3";

          # send window to monitor and follow focus
          # ctrl + cmd - c  : yabai -m window --display next; yabai -m display --focus next
          # ctrl + cmd - 1  : yabai -m window --display 1; yabai -m display --focus 1

          # move floating window
          "shift + ctrl - a " = " yabai -m window --move rel:-20:0";
          "shift + ctrl - s " = " yabai -m window --move rel:0:20";

          # increase window size
          "shift + alt - a " = " yabai -m window --resize left:-20:0";
          "shift + alt - w " = " yabai -m window --resize top:0:-20";

          # decrease window size
          "shift + cmd - s " = " yabai -m window --resize bottom:0:-20";
          "shift + cmd - w " = " yabai -m window --resize top:0:20";

          # set insertion point in focused container
          "ctrl + alt - h " = " yabai -m window --insert west";

          # toggle window zoom
          "alt - d " = " yabai -m window --toggle zoom-parent";
          "alt - f " = " yabai -m window --toggle zoom-fullscreen";

          # toggle window split type
          "alt - e " = " yabai -m window --toggle split";

          # float / unfloat window and center on screen
          "alt - t " = " yabai -m window --toggle float --grid 4:4:1:1:2:2";

          # toggle sticky(+float), picture-in-picture
          "alt - p " = " yabai -m window --toggle sticky --toggle pip";
        };
        attibutes = builtins.attrNames input;
        rows = map (v: "${v} : ${builtins.getAttr v input}") attibutes;
      in
      builtins.concatStringsSep "\n" rows;

  };
  services.sketchybar.enable = true;
  services.sketchybar.extraPackages = with pkgs; [ jq yabai ];

  users.users.ivypierlot = {
    name = "ivypierlot";
    home = "/Users/ivypierlot";
    shell = pkgs.zsh;
  };
}
