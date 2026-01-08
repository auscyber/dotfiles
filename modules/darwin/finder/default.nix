{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.auscybernix.programs.finder;
in
with lib;
{
  options.auscybernix.programs.finder = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable Finder program configuration.";
    };
  };
  config = mkIf cfg.enable {
    system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;
    system.defaults.NSGlobalDomain.AppleTemperatureUnit = "Celsisus";
    system.defaults.finder = {
      ShowPathbar = true;

      AppleShowAllExtensions = true;
      FXPreferredViewStyle = "clmv";
      #	"FK_StandardViewOptions2"
      ShowStatusBar = true;

    };
  };

}
