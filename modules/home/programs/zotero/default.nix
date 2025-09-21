{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs.zotero;
in
{
  options.auscybernix.programs.zotero = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Zotero and configure it with my settings.";
    };
  };
  config = lib.mkIf cfg.enable {

    home.packages = with pkgs; [
      zotero
      tesseract
      poppler_utils
    ];
  };
}
