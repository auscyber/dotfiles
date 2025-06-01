{
  config,
  lib,
  pkgs,
  ...
}:
{
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

}
