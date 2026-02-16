{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs.openclaw;
in
{
  options.auscybernix.programs.openclaw = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable OpenClaw, a reimplementation of Captain Claw (1997) platformer.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.openclaw
    ];
  };
}
