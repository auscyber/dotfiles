{
  config,
  inputs,
  lib,
  ...
}:
{

  options.auscybernix.nix = {
    flake = lib.mkOption {
      type = lib.types.str;

    };
    reloadProgram = lib.mkOption {
      type = lib.types.str;
      default = "re";
    };
  };
  config = {

    nix.registry.dots.flake = inputs.self;
  };

}
