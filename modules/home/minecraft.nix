{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs.prismlauncher;
in
{

  options.auscybernix.programs.prismlauncher = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Prism Launcher (a Minecraft launcher).";
    };
  };
  config = lib.mkIf cfg.enable {

    programs.prismlauncher = {
      enable = true;
    };
  };

}
