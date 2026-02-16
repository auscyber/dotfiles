{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  cfg = config.auscybernix.programs.openclaw;
in
{
  imports = [
    inputs.nix-openclaw.homeManagerModules.openclaw
  ];

  options.auscybernix.programs.openclaw = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable OpenClaw, a declarative OpenClaw packaging for AI assistant gateway.";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.openclaw = {
      enable = true;
    };
  };
}
