{ config, pkgs, ... }:
let
  domain = "cache.ivymect.in";
in
{

  services.ncps = {
    enable = true;
    cache = {
      hostName = "secondpc";
      dataPath = "/mnt/hdd/ncps";
      maxSize = "200G";
      lru.schedule = "0 2 * * *"; # Clean up daily at 2 AM
      allowPutVerb = true;
      allowDeleteVerb = true;
    };
    server.addr = "0.0.0.0:8501";
    upstream = {
      caches = [
        "https://nix-community.cachix.org"
        "https://iohk.cachix.org"
        "https://cache.nixos.org"
        "https://devenv.cachix.org"
        "https://auscyber.cachix.org"
      ];
      publicKeys = [
        "auscyber.cachix.org-1:RPlENxXc/irvLimM0Yz8Au3ntk/sxZ8bwXPwuXL3v5c="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      ];
    };
    prometheus.enable = true;
  };
}
