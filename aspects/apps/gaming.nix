{ den, ... }:
{
  den.default.gaming = {
    includes = [ den.aspects.packages.proton-ge-bin ];
    nixos = { pkgs, ... }: {
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
            enable = false;
            args = [
              "--force-grab-cursor"
              "--mangoapp"
              "-f"
              "-r"
              "144"
              "-w"
              "1920"
              "-h"
              "1080"
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
