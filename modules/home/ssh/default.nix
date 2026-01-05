{ config, pkgs, ... }:
{
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

}
