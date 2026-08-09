{ den, ... }: {
  den.default.gaming = {
    includes = [
      den.aspects.packages.proton-ge-bin
    ];

    # macOS/Apple Silicon side of the "gaming" role: nixpkgs has no
    # Game Porting Toolkit derivation (Apple ships it Homebrew-only, gated
    # behind an Apple Developer login), so this mirrors the NixOS Steam+Proton
    # block above with GPT's own brew tap/formula instead. Rosetta (needed to
    # run GPT's x86_64 wine64 under translation) is already turned on by
    # `nix-homebrew.enableRosetta` in aspects/darwin/homebrew.nix.
    # The formula's .dmg is gated behind an Apple Developer login, so `brew
    # install` can't fetch it itself -- download it from developer.apple.com
    # by hand once and drop it into Homebrew's download cache at the path/name
    # `brew install apple/apple/game-porting-toolkit` reports missing (or
    # `brew fetch --force apple/apple/game-porting-toolkit` to print it without
    # installing); onActivation's `brew bundle` picks it up from cache after
    # that, no re-download.

    nixos = { pkgs, ... }: {
      boot.kernelModules = [ "ntsync" ];
      programs = {
        gamescope = {
          enable = true;
          capSysNice = false;
        };
        gamemode.enable = true;
        steam = {
          proton-ge = {
            enable = true;
            settings = {
              PROTON_ENABLE_WAYLAND = "1";
              PROTON_NO_ESYNC = "0";
            };
          };
          enable = true;
          extraCompatPackages = with pkgs; [
            # gamescope
            vkd3d-proton
          ];
          package = pkgs.steam.override {
            extraPkgs =
              pkgs': with pkgs'; [
                libXcursor
                libXi
                libXinerama
                libXScrnSaver
                libpng
                libpulseaudio
                libvorbis
                mangohud
                stdenv.cc.cc.lib # Provides libstdc++.so.6
                gamescope
                gamemode
                gamescope-wsi
                libkrb5
                keyutils
                # Add other libraries as needed
              ];
          };
          protontricks.enable = true;
          gamescopeSession = {
            enable = true;
            args = [
              "--force-grab-cursor"
              "--mangoapp"
              "-f"
              "-r"
              "144"
              "-w"
              "2560"
              "-h"
              "1440"
              "--adaptive-sync"
            ];
          };
        };
      };
      environment.systemPackages = with pkgs; [
        gamescope-wsi # HDR
        mangohud
      ];
    };
  };
}
