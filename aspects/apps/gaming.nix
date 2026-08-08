{ den, ... }: {
  den.default.gaming = {
    includes = [
      den.aspects.packages.proton-ge-bin
      den.aspects.homebrew
    ];

    # macOS/Apple Silicon side of the "gaming" role: nixpkgs has no
    # Game Porting Toolkit derivation (Apple ships it Homebrew-only, gated
    # behind an Apple Developer login), so this mirrors the NixOS Steam+Proton
    # block above with GPT's own brew tap/formula instead. Rosetta (needed to
    # run GPT's x86_64 wine64 under translation) is already turned on by
    # `nix-homebrew.enableRosetta` in aspects/darwin/homebrew.nix.
    brew = {
      taps.apple = "apple/homebrew-apple";
      brews = [ "apple/apple/game-porting-toolkit" ];
    };

    darwin =
      { pkgs, ... }:
      {
        environment.systemPackages = [
          (pkgs.writeShellApplication {
            name = "gpt-init-prefix";
            text = ''
              # Bootstraps a Wine prefix for Apple's Game Porting Toolkit:
              # copies the runtime libraries out of the mounted GPT .dmg
              # (Finder must already have it open, per Apple's install steps)
              # and opens winecfg so you can pick Windows 10 as the prefix OS
              # -- that dialog is interactive, there's no headless flag for it.
              prefix="''${GPT_PREFIX:-$HOME/my-game-prefix}"
              gpt_root="$(brew --prefix game-porting-toolkit)"

              volume="$(find /Volumes -maxdepth 1 -iname 'Game Porting Toolkit*' -print -quit)"
              if [[ -z "$volume" ]]; then
                echo "Mount the Game Porting Toolkit .dmg (from developer.apple.com) first, then re-run." >&2
                exit 1
              fi

              echo "Copying GPT runtime libraries from '$volume' into '$gpt_root/lib' ..."
              ditto "$volume/lib/" "$gpt_root/lib/"

              echo "Opening winecfg for '$prefix' -- select 'Windows 10' as the OS, then close it."
              WINEPREFIX="$prefix" arch -x86_64 "$gpt_root/bin/wine64" winecfg

              echo "Prefix ready. Install a Windows app into it with:"
              echo "  gameportingtoolkit '$prefix' /path/to/installer.exe"
            '';
          })
        ];
      };

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
