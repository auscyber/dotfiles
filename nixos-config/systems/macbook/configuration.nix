# darwin.nix

{ pkgs, ... }:

{
    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages =
    with pkgs; [ vim _1password-gui  ];

    # Auto upgrade nix package and the daemon service.
    services.nix-daemon.enable = true;
    services.karabiner-elements.enable = true;
    # nix.package = pkgs.nix;

    # Necessary for using flakes on this system.
    nix.settings.experimental-features = "nix-command flakes";

    # Create /etc/zshrc that loads the nix-darwin environment.
    programs.zsh.enable = true;  # default shell on catalina
    # programs.fish.enable = true;

    # Used for backwards compatibility, please read the changelog before changing.
    # $ darwin-rebuild changelog
    system.stateVersion = 4;

    # The platform the configuration will be used on.
    nixpkgs.hostPlatform = "aarch64-darwin";
    homebrew.enable = true;
    homebrew.casks = ["arc" "zotero" "beeper" "steam" "notion" "google-drive" "spotify" "discord" "affinity-designer" "affinity-publisher" "grammarly-desktop"];
    homeBrew.masApps = {
          "1Password for Safari" = 1569813296;
        "Microsoft 365" = 1450038993;
        Fantastical = 975937182;
        "Microsoft Teams" = 1113153706;
    };

    users.users.ivypierlot = {
        name = "ivypierlot";
        home = "/Users/ivypierlot";
    };
}