final: prev: {
  extraSub =
    modules:
    prev.types.submoduleWith {
      inherit modules;
      shorthandOnlyDefinesConfig = null;
    };

  inputMetaModules = modules: {
    options.flake-file =
      let
        inputsOptions = prev.mkOption {
          type = prev.types.lazyAttrsOf (final.extraSub modules);
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

  inputMetaWithArgs =
    opts:
    final.inputMetaModules [
      (args: {
        options.meta = prev.mkOption { type = prev.types.submodule { options = opts args; }; };

      })
    ];

  inputMeta = opts: final.inputMetaWithArgs (_: opts);
}
