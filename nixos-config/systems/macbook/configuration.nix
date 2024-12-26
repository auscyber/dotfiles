# darwin.nix

{ pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  documentation.enable = true;
  security.pam.enableSudoTouchIdAuth = true;
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [ vim nodejs vscode wezterm zotero gnupg ];

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

  homebrew.casks = [ "beeper" "steam" "notion" "google-drive" "spotify" "arc" "discord" "affinity-designer" "affinity-publisher" "grammarly-desktop" "nitro-pdf-pro" "amethyst" "1password" "plover" "postman" ];
  homebrew.masApps = {
    "1Password for Safari" = 1569813296;
    "Microsoft 365" = 1450038993;
    Fantastical = 975937182;
    "Microsoft Teams" = 1113153706;
  };

  users.users.ivypierlot = {
    name = "ivypierlot";
    home = "/Users/ivypierlot";
    shell = pkgs.zsh;
  };
}


