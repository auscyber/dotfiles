{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.bootlogo;

in
{

  options.auscybernix.bootlogo = {
    enable = lib.mkEnableOption "Enable AusCyberNix boot logo";
  };

  config = lib.mkIf cfg.enable {
    boot.plymouth = {
      enable = true;
    };
  };

}
