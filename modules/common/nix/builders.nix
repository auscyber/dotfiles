{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.buildConfig;
  inherit (lib) mkOption types;

in

{

  options.auscybernix.buildConfig = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable distributed builds with Nix.";
    };

    system = mkOption {
      type = types.str;
      default = "x86_64-linux";
      description = "The system type for the build machines.";
    };

  };
  config = {

    nix.distributedBuilds = true;

  };

}
