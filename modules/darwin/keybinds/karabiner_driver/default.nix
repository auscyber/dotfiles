{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.auscybernix.keybinds.karabiner-driver-kit;
  parentAppDir = "/Applications/Nix Apps";

in
{

  options.auscybernix.keybinds.karabiner-driver-kit = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.karabiner-dk;
    };

  };

  config = lib.mkIf cfg.enable {
    services.karabiner-dk = {
      enable = true;
      package = cfg.package;
    };
  };
}
