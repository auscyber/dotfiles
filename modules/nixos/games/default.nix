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
    programs.gamemode.enable = true;
    programs.steam = {
      enable = true;
      extraCompatPackages = with pkgs; [
        proton-ge-bin
        vkd3d-proton
      ];
      protontricks.enable = true;
      gamescopeSession.enable = true;

    };
  };

}
