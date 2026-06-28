{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.plugins.jujutsu;
  possible-sources = {
    difftastic = "difftastic";
    diffview = "diffview";
    codediff = "codediff";
  };
in
{
  config = lib.mkIf cfg.enable {
    plugins = lib.mapAttrs' (source: name: {
      inherit name;
      value.enable = lib.mkIf (source == cfg.settings.diff_preset) true;
    }) possible-sources;

  };
}
