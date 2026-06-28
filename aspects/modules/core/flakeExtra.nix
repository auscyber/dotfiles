{
  lib,
  den,
  inputs,
  ...
}@args:
let
  flakeModule = (den.lib.aspects.resolve "flake-mod" (den.lib.resolveEntity "flake-mod" { }));
  evaled = (
    lib.evalModules {
      modules = [
        flakeModule
        {
          freeformType = with lib.types; attrs;
        }
      ];
      class = "flake";
      specialArgs = {
        inputs = inputs;
      };
    }
  )

  ;

in
{

  options.flake-mod = lib.mkOption {
    type = lib.types.attrs;
    default = lib.types.any;
    description = "Configuration for the flake-mod aspect.";
  };
  config = rec {
    flake-mod = evaled.config;
    inherit (flake-mod) flake-file;
    flake.flake-mod = evaled.config;

    den.aspects.flake-mod = { };

    den.schema.flake-mod.isEntity = true;

    den.aspects.ivypierlot.inputs = {
      #  test.url = "github:auscyber/agenix";
    };
  };

}
