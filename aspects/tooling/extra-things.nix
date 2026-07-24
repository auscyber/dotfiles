# modules/inputs/_extras.nix
{ lib, ... }:
{
  options.flake-file = lib.mkOption {
    type = lib.extraSub [
      ({ options, ... }: {
        options = {
          inputs = lib.mkOption {
            apply = lib.mapAttrs (_: v: removeAttrs v [ "meta" ]);
          };
          inputsWithMeta = lib.mkOption {
            type = lib.types.lazyAttrsOf (
              lib.types.submodule { freeformType = lib.types.attrsOf lib.types.raw; }
            );
            internal = true;
            default = { };
          };
        };
        config.inputsWithMeta = lib.modules.mkAliasAndWrapDefsWithPriority lib.id options.inputs;
      })
    ];
  };
}
