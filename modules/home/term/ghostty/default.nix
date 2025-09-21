{
  config,
  nixpkgs,
  inputs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.terms.ghostty;
in
{
  options.auscybernix.terms.ghostty = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable ghostty terminal emulator.";
    };
  };
  config = lib.mkIf cfg.enable {
    home.file.".config/ghostty/shaders/cursor_shader.glsl".source =
      "${inputs.cursor_shader}/cursor_synesthaxia(tweened).glsl";
    stylix.targets.ghostty.enable = false;

    programs.ghostty = {

      enable = true;
      themes = {
        pink_ocean = {

          foreground = "d0d0d0";
          cursor-color = "eeeeee";
          selection-background = "005f5f";
          selection-foreground = "eeeeee";
          palette = [
            "0=#080808"
            "1=#ff5f5f"
            "2=#87d7af"
            "3=#d7d787"
            "4=#5fafd7"
            "5=#afafff"
            "6=#5fd7d7"
            "7=#dadada"
            "8=#8a8a8a"
            "9=#d75f5f"
            "10=#afd7af"
            "11=#d7d7af"
            "12=#87afd7"
            "13=#afafd7"
            "14=#87d7d7"
            "14=#dadada"
          ];
          background = "202020";

        };

      };
      settings = {
        custom-shader = [
          "./shaders/cursor_shader.glsl"
          #        "${inputs.ghostty-shaders}/cursor_blaze.glsl"
        ];
        cursor-style = "block";
        #      macos-icon = "paper";
        theme = "pink_ocean";
        #      background-opacity = 0.8;
        font-size = 15;
        font-family = "Hasklug Nerd Font";
      };
    };
  };
}
