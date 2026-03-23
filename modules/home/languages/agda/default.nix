{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.auscybernix.languages.agda;
in

{
  options.auscybernix.languages.agda = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Agda and some useful libraries.";
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      (agda.withPackages (
        p: with p; [
          standard-library
          cubical
          agda-categories
        ]
      ))
    ];
  };
}
