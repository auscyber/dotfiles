{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.auscybernix.tools.updater;
  finalPackage = pkgs.writeShellApplication {
    name = "update-config";
    runtimeInputs = [
      cfg.nvFetcherPackage
    ];
    text = ''
      set -e
      PWD=${lib.escapeShellArg config.auscybernix.nix.flake}
      nix flake update
      nvfetcher -k ${lib.escapeShellArg config.age.templates."nvchecker.toml".path}
    '';
  };
in
{

  options.auscybernix.tools.updater = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable the updater tool.";
    };
    nvFetcherPackage = lib.mkPackageOption pkgs "nvfetcher" { };

    finalPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      internal = true;
      default = finalPackage;
      defaultText = "Automatically determined based on the value of nvFetcherPackage.";
      description = "The package to use for the updater tool.";
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.finalPackage ];
  };

}
