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
      description = "Folder where the flake.nix is located";
      defaultText = lib.literalExpression ''"''${config.home.homeDirectory}/dotfiles"'';
    };
  };
  config.auscybernix.flakeConfig.flakeFolder = lib.mkDefault "${config.home.homeDirectory}/dotfiles";
  config = {

    lib.file.getLocalPath =
      path:
      cfg.flakeFolder

      + (lib.strings.removePrefix (builtins.toString ../../..) (builtins.toString path));
    lib.file.linkLocalPath =
      path: config.lib.file.mkOutOfStoreSymlink (config.lib.file.getLocalPath path);
  };
}
