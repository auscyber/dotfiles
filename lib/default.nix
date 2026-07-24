final: prev: {
  extraSub =
    modules:
    prev.types.submoduleWith {
      inherit modules;
      shorthandOnlyDefinesConfig = null;
    };
  inputMeta = opts: {
    options.flake-file =
      let
        inputsOptions = prev.mkOption {
          type = prev.types.lazyAttrsOf (final.extraSub [ { options.meta = opts; } ]);

        };
      in

      prev.mkOption {
        type = final.extraSub [
          {
            options.inputs = inputsOptions;
            options.inputsWithMeta = inputsOptions;
          }
        ];
      };
  };
}
