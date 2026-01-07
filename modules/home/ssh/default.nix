{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs;
in

{
  options.auscybernix.programs.ssh = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };
  config = lib.mkIf cfg.enable {
    programs.ssh = {
      matchBlocks = {
        "faggot.sh" = {
          hostname = "faggot.sh";
          user = "ivy";
          forwardAgent = true;

        };
        "secondpc" = {
          hostname = "121.200.22.213";
          forwardAgent = true;
          user = "auscyber";
        };
        "imflo.pet" = {
          hostname = "imflo.pet";
          forwardAgent = true;
          user = "ivy";

        };

      };
    };
  };

}
