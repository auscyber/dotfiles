{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.flakeConfig;
in
{
  options.auscybernix.flakeConfig = {
    flakeFolder = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/dotfiles";
      description = "Folder where the flake.nix is located";
    };
  };
  config = {

    lib.file.getLocalPath =
      path:
      cfg.flakeFolder

      + (lib.strings.removePrefix (builtins.toString ../../..) (builtins.toString path));
    lib.file.linkLocalPath =
      path: config.lib.file.mkOutOfStoreSymlink (config.lib.file.getLocalPath path);
  };
}
