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
            xorg.libXcursor
            xorg.libXi
            xorg.libXinerama
            xorg.libXScrnSaver
            libpng
            libpulseaudio
            libvorbis
            stdenv.cc.cc.lib # Provides libstdc++.so.6
            gamescope
            gamescope-wsi
            libkrb5
            keyutils
            # Add other libraries as needed
          ];
      };
      protontricks.enable = true;
      gamescopeSession.enable = true;

    };
  };

}
