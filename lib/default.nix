final: prev: {
  # Every aspect under `dir`, in one list. A file's layer comes from the
  # `layers` tag on the aspects it defines (aspects/framework/layers.nix), never
  # from its path.
  aspectFiles =
    dir:
    dir
    |> prev.fileset.fileFilter (file: file.hasExt "nix" && !prev.hasPrefix "_" file.name)
    |> prev.fileset.toList;

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
  flattenAttrset =
    set:
    let
      recurse =
        path:
        final.concatMapAttrs (
          name: value:
          if builtins.isAttrs value then
            recurse (path ++ [ name ]) value
          else
            { ${builtins.concatStringsSep "." (path ++ [ name ])} = value; }
        );
    in
    recurse [ ] set;
}
