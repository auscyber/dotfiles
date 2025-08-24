{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
let
  skhdMap = config.skhd.configmap;
in
{

  options = {

    services.skhd.configMap = mkOption {
      type = attrs;
      default = { };
    };
  };

  config = {
    services.skhd.config =
      let
        attibutes = builtins.attrNames skhdMap;
        rows = map (v: "${v} : ${builtins.getAttr v skhdMap}") attibutes;
      in
      builtins.concatStringsSep "\n" rows;

    home.file."binds.txt" = ;
  };

}
