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
  };
  config = {

    nix.registry.dots.flake = inputs.self;
  };

}
