{ den, ... }:
{
  # ── Terminal aspect ───────────────────────────────────────────────────────────
  # Home-manager: Ghostty terminal emulator.
  # Include in a user aspect to deploy Ghostty with custom theming and shaders.
  den.aspects.terminal = {
    homeManager =
      { config, lib, pkgs, ... }:
      {
        options.auscybernix.terms.ghostty.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Ghostty terminal emulator.";
        };

        config = lib.mkIf config.auscybernix.terms.ghostty.enable {
          stylix.targets.ghostty.enable = false;
          programs.ghostty = {
            enable = true;
            themes.pink_ocean = {
              foreground = "d0d0d0";
              cursor-color = "eeeeee";
              selection-background = "005f5f";
              selection-foreground = "eeeeee";
              palette = [
                "0=#080808" "1=#ff5f5f" "2=#87d7af" "3=#d7d787"
                "4=#5fafd7" "5=#afafff" "6=#5fd7d7" "7=#dadada"
                "8=#8a8a8a" "9=#d75f5f" "10=#afd7af" "11=#d7d7af"
                "12=#87afd7" "13=#afafd7" "14=#87d7d7" "14=#dadada"
              ];
              background = "202020";
            };
            installBatSyntax = false;
            settings = {
              shell-integration = "none";
              shell-integration-features = "no-path";
              custom-shader = [
                (builtins.toString ../../modules/home/term/ghostty/cursor_warp.glsl)
              ];
              cursor-style = "block";
              theme = "pink_ocean";
              font-size = 15;
              font-family = "Hasklug Nerd Font";
            };
          };
        };
      };
  };
}
