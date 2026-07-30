final: prev: {
  # Split the aspect tree into the base flake-parts import list plus one list
  # per flake-parts partition, driven by ./partition-map.nix.
  #
  # A map entry is a path relative to `dir`; a directory entry claims every
  # aspect under it. Anything claimed by a partition is NOT imported by the base
  # evaluation -- that is what keeps the partition's `ff.*` declarations (and so
  # its inputs, and so flake.lock) out of the root flake. See
  # aspects/framework/partitions.nix.
  aspectPartitions =
    {
      dir,
      map,
    }:
    let
      files =
        dir
        |> prev.fileset.fileFilter (file: file.hasExt "nix" && !prev.hasPrefix "_" file.name)
        |> prev.fileset.toList;

      claimedBy =
        entries:
        let
          paths = builtins.map (p: toString (dir + "/${p}")) entries;
        in
        file:
        let
          s = toString file;
        in
        prev.any (p: s == p || prev.hasPrefix "${p}/" s) paths;

      parts = prev.mapAttrs (_: entries: prev.filter (claimedBy entries) files) map;
      claimed = prev.concatLists (prev.attrValues parts);
    in
    {
      inherit parts;
      # Every partitioned aspect, for the `all` partition (see partitions.nix).
      all = claimed;
      base = prev.filter (file: !(prev.elem file claimed)) files;
    };

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
