{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs.emacs;
in
{
  options.auscybernix.programs.emacs = {
    enable = lib.mkEnableOption "Enable Emacs with native-compilation and the Emacs daemon service.";
  };

  config = lib.mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = pkgs.emacsNativeComp;
    };
    services.emacs = {
      enable = true;
    };
  };
}
