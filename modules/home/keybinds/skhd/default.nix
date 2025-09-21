{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.keybinds.skhd;
in
with lib;
{
  options.auscybernix.keybinds.skhd = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
    binds = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
    };
  };
  config = lib.mkIf cfg.enable {
    services.skhd = {
      enable = true;
      config =
        let
          attibutes = builtins.attrNames cfg.binds;
          rows = map (v: "${v} : ${builtins.getAttr v cfg.binds}") attibutes;
        in
        builtins.concatStringsSep "\n" rows;
    };
    home.file."keybinds.json" = {
      text = builtins.toJSON cfg.binds;
    };

  };

}
