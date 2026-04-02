{ den, ... }:
{
  # ── Boot logo aspect ──────────────────────────────────────────────────────────
  # NixOS: Plymouth boot splash screen.
  # Include in a host aspect to enable a graphical boot screen.
  den.aspects.bootlogo = {
    nixos =
      { config, lib, ... }:
      {
        options.auscybernix.bootlogo.enable =
          lib.mkEnableOption "AusCyberNix plymouth boot logo";

        config = lib.mkIf config.auscybernix.bootlogo.enable {
          boot.plymouth.enable = true;
        };
      };
  };
}
