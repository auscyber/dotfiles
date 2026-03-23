{
  pkgs,
  options,
  config,
  lib,
  ...
}:
let
  inherit (lib) mkOption;

  cfg = config.auscybernix.browsers.helium;
in
{
  options.auscybernix.browsers.helium = {
    enable = lib.mkEnableOption "Helium browser";
  };
  config = lib.mkIf cfg.enable {
    programs.helium = {
      enable = true;
      extensions = [
        {
          id = "cdglnehniifkbagbbombnjghhcihifij";
        }
      ];
    };
  };

}
