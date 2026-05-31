{
  config,
  inputs,
  lib,
  ...
}:
{

  options.auscybernix = {
    nix.flake = lib.mkOption {
      type = lib.types.str;
      default = "/home/auscyber/dotfiles";

    };
    reloadAlias = lib.mkOption {
      type = lib.types.str;
      default = "re";
    };
    reloadProgram = lib.mkOption {
      type = lib.types.str;
    };
    meta = {
      userFriendlyName = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
      description = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
    };

  };
  config = {

    nix.registry.dots.flake = inputs.self;
  };

}
