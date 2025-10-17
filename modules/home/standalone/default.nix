{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.standalone;

in
{

  options.auscybernix.standalone = {
    enable = lib.mkEnableOption "Enable AusCyberNix standalone configuration.";
  };
  config = lib.mkIf cfg.enable {
    stylix.image = ../../../backgrounds/phoebebridgers-2.jpg;
  };
}
