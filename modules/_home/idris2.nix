{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs.idris2;
in
{
  options.auscybernix.programs.idris2 = {
    enable = lib.mkEnableOption "Enable Idris2 language support (LSP + formatter).";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs.idris2Pkgs;
      [
        lsp
        Prettier
      ]
      ++ [ pkgs.idris2 ];
  };
}
