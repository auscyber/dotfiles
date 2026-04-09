{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.nixos.games;
in
{

  options.auscybernix.nixos.games = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable gaming related programs and settings.";
    };
  };
  config = lib.mkIf cfg.enable {
    programs.gamescope = {
      enable = true;
      capSysNice = false;
    };
    environment.systemPackages = with pkgs; [
      gamescope-wsi # HDR
      mangohud
    ];
    programs.gamemode.enable = true;
    programs.steam = {
      enable = true;
      extraCompatPackages = with pkgs; [
        # gamescope
        proton-ge-bin
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
          "1920"
          "-h"
          "1080"
          "--adaptive-sync"
        ];
      };

    };
  };

}
