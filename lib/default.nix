final: prev: {
  # Split the aspect tree into the base flake-parts import list plus one list
  # per flake-parts partition, driven by ./partition-map.nix.
  #
  # A map entry is a path relative to `dir`; a directory entry claims every
  # aspect under it. Anything claimed by a partition is NOT imported by the base
  # evaluation -- that is what keeps the partition's `ff.*` declarations (and so
  # its inputs, and so flake.lock) out of the root flake. See
  # aspects/framework/partitions.nix.
  #
  # `deps` lets one bucket be evaluated with another bucket's aspects imported
  # too. Buckets are otherwise disjoint supersets of base -- `partitions.nixos`
  # sees base + `parts.nixos` and nothing else -- so a bucket every host needs
  # (./packages) has to be pulled in explicitly or the hosts' partitions would
  # only ever see the inert base stubs. The dependency is on the ASPECTS, not on
  # the sub-flake: each bucket still locks its own inputs, and partitions.nix
  # subtracts a dep's inputs from the dependent's generated flake.nix so the same
  # input is never declared (or locked) twice.
  aspectPartitions =
    {
      dir,
      map,
      deps ? { },
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

      # Transitive dependency closure, so a dep-of-a-dep still arrives. `seen`
      # turns what would otherwise be an infinite recursion (and an unreadable
      # stack overflow) into a named error.
      resolveDeps =
        seen: bucket:
        builtins.concatMap (
          d:
          if !(parts ? ${d}) then
            throw "aspectPartitions: partition '${bucket}' depends on unknown partition '${d}'"
          else if prev.elem d seen then
            throw "aspectPartitions: dependency cycle through partition '${d}'"
          else
            [ d ] ++ resolveDeps (seen ++ [ d ]) d
        ) (deps.${bucket} or [ ]);

      depsOf = prev.mapAttrs (bucket: _: prev.unique (resolveDeps [ bucket ] bucket)) parts;
    in
    {
      inherit parts;
      # bucket -> every bucket whose aspects it is evaluated with, transitively.
      deps = depsOf;
      # The full import list for a bucket's partition: its own aspects plus its
      # deps'. `parts.<b>` stays the bucket's OWN files, because that is what
      # decides which inputs its generated flake.nix declares.
      partsFor = bucket: parts.${bucket} ++ builtins.concatMap (d: parts.${d}) depsOf.${bucket};
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
