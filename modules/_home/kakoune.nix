{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs.kakoune;
in
{
  options.auscybernix.programs.kakoune = {
    enable = lib.mkEnableOption "Enable Kakoune editor with curated plugins.";
  };

  config = lib.mkIf cfg.enable {
    programs.kakoune = {
      enable = true;
      plugins = with pkgs.kakounePlugins; [
        kak-lsp
        parinfer-rust
        kakoune-extra-filetypes
        powerline-kak
      ];
    };
  };
}
