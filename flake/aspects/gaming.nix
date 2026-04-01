{ den, ... }:
{
  # ── Gaming aspect ─────────────────────────────────────────────────────────────
  # NixOS: Steam, gamescope, gamemode, proton.
  # Home-manager: Prism Launcher (Minecraft).
  # Include in a host aspect (nixos) and/or a user aspect (homeManager).
  den.aspects.gaming = {
    nixos =
      { config, lib, pkgs, ... }:
      {
        options.auscybernix.nixos.games.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable gaming related programs and settings.";
        };

        config = lib.mkIf config.auscybernix.nixos.games.enable {
          programs.gamescope = {
            enable = true;
            capSysNice = false;
          };
          environment.systemPackages = with pkgs; [
            gamescope-wsi
            mangohud
          ];
          programs.gamemode.enable = true;
          programs.steam = {
            enable = true;
            extraCompatPackages = with pkgs; [
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
                  stdenv.cc.cc.lib
                  gamescope
                  gamemode
                  gamescope-wsi
                  libkrb5
                  keyutils
                ];
            };
            protontricks.enable = true;
            gamescopeSession.enable = true;
          };
        };
      };

    homeManager =
      { config, lib, pkgs, ... }:
      {
        options.auscybernix.programs.prismlauncher.enable =
          lib.mkEnableOption "Prism Launcher (Minecraft)";

        config = lib.mkIf config.auscybernix.programs.prismlauncher.enable {
          programs.prismlauncher.enable = true;
        };
      };
  };
}
