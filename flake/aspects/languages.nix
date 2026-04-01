{ den, ... }:
{
  # ── Languages aspect ──────────────────────────────────────────────────────────
  # Home-manager: Agda (with standard-library, cubical, agda-categories),
  #               Idris2 (with LSP and Prettier).
  # Include in a user aspect to enable formal-methods / functional language tools.
  den.aspects.languages = {
    homeManager =
      { config, lib, pkgs, ... }:
      {
        options.auscybernix.languages.agda.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Agda and useful libraries.";
        };
        options.auscybernix.languages.idris2.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Idris2 language tools.";
        };

        config = lib.mkMerge [
          (lib.mkIf config.auscybernix.languages.agda.enable {
            home.packages = with pkgs; [
              (agda.withPackages (p: with p; [
                standard-library
                cubical
                agda-categories
              ]))
            ];
          })

          (lib.mkIf config.auscybernix.languages.idris2.enable {
            home.packages =
              (with pkgs.idris2Pkgs; [ lsp Prettier ])
              ++ [ pkgs.idris2 ];
          })
        ];
      };
  };
}
