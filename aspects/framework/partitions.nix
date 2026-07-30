{
  inputs,
  realInputs,
  lib,
  config,
  rootPath,
  aspectPartitions,
  ...
}:
# flake-parts partitions: keep inputs that only serve a subset of the flake's
# outputs out of the root flake.nix (and therefore out of flake.lock).
#
# Nix already fetches an input lazily -- `nixosConfigurations.secondpc` evaluates
# fine with `--override-input homebrew-core path:/var/empty`, so a rebuild never
# fetches it. What is NOT lazy is the lock: every input in flake.nix is a node in
# flake.lock, and `nix flake update`, `nix flake archive`, CI and any downstream
# consumer walk all of them. A partition is the only way to move an input out of
# the root lock while still being able to use it.
#
# A partition is a SUPERSET re-evaluation: `partitions.<p>.module` is
# `extendModules` over the entire top-level module set, plus that partition's
# aspects, with `inputs = rootInputs // extraInputs`. So base keeps defining
# everything, and a partition only adds -- which is why host-iterating aspects
# (deploy.nodes, ciMatrix, the rekey app) do not need to move: at base they see
# no extra aspects and evaluate to whatever they evaluate to, and every attribute
# that actually matters is read from `all` via `partitionedAttrs`.
let
  buckets = builtins.attrNames aspectPartitions.parts;

  # The base evaluation's input names. Subtracted inside each partition so the
  # generated partitions/<bucket>/flake.nix holds only that bucket's own inputs
  # (a partition's flake-file sees base + bucket, being a superset eval).
  baseInputNames = builtins.attrNames config.flake-file.inputs;

  # `extraInputsFlake` would let the sub-flake win every name it shares with the
  # root (flake-parts merges `inputs // extraInputs`), so load the sub-flake
  # ourselves and drop everything the root already provides. A sub-flake only
  # ever redeclares a root input because a `follows` cannot reach the parent
  # flake, so its copy is always the wrong one -- most sharply for home-manager,
  # where the root's is patched (see lib/patched-inputs.nix).
  # A partition's inputs must go through the same patch pass the root's do.
  # lib/patched-inputs.nix is a plain function precisely so more than one caller
  # can share it (flake.nix and aspects/tooling/patch-inputs.nix already do);
  # `rootPath` is only used for the flake path and the lock it reads the
  # auto-unify closure from, so pointing it at the sub-flake is all that is
  # needed. Without this an input that moved into a bucket would be silently
  # UNPATCHED -- the sub-flake is loaded straight through flake-compat, which
  # knows nothing about patches.
  patchFor =
    bucket: raw:
    let
      inputsOnly = builtins.removeAttrs raw [ "self" ];
    in
    (import (rootPath + "/lib/patched-inputs.nix") {
      inherit lib;
      inputs = inputsOnly;
      rootPath = rootPath + "/partitions/${bucket}";
      # Only specs for inputs this bucket actually provides: a spec's `src`
      # defaults to `inputs.<name>.sourceInfo`, which would not resolve here.
      patchSpecs = lib.filterAttrs (name: _: inputsOnly ? ${name}) (
        import (rootPath + "/patched-inputs.nix")
      );
    }).newInputs;

  subInputs =
    bucket:
    builtins.removeAttrs (patchFor bucket
      (import inputs.flake-compat {
        src = rootPath + "/partitions/${bucket}";
        system = throw "flake-compat is loading a partition in pure mode; `system` must not be forced";
      }).outputs.inputs
    ) ([ "self" ] ++ baseInputNames);

  # flake-file's own serializer, so the top level can look at a bucket's inputs
  # in the same shape `preProcess` receives them.
  ffLib = import "${inputs.flake-file}/dev/modules/_lib" lib;

  # What each bucket declares that base does not. Forcing this forces that
  # partition's evaluation, so it is only ever reached from `preProcess` --
  # i.e. when a flake.nix is being written or checked, never during a host build.
  bucketOwn =
    bucket:
    builtins.removeAttrs (ffLib.inputsExpr
      config.partitions.${bucket}.module.flake-file.inputs
    ) baseInputNames;

  followsTargetsOf =
    specs:
    lib.unique (
      builtins.concatMap (
        spec:
        builtins.filter (f: f != null) (lib.mapAttrsToList (_: v: v.follows or null) (spec.inputs or { }))
      ) (builtins.attrValues specs)
    );

  # An input declared on an aspect cannot always live in a bucket. Two cases
  # force it back to the root flake, and both are silent corruption if missed:
  #
  #   * more than one bucket declares it -- the aspect is used on both platforms,
  #     so each sub-flake would lock its own copy and `subInputs` would pick one
  #     arbitrarily (crane, celler, stylix, ...);
  #   * a root input `follows` it -- the root lock then names an input it does
  #     not have, which nix rejects outright and which cannot be repaired by
  #     regenerating, because the flake no longer evaluates
  #     (`input 'age-plugin-gpg/crane' follows a non-existent input 'crane'`).
  #
  # Both are decided here rather than by hand-editing the aspect back.
  ownByBucket = lib.genAttrs buckets bucketOwn;
  allOwn = lib.mergeAttrsList (builtins.attrValues ownByBucket);
  declaredIn = name: builtins.filter (b: ownByBucket.${b} ? ${name}) buckets;

  baseSerialized = ffLib.inputsExpr config.flake-file.inputs;

  hoistedNames =
    let
      shared = builtins.filter (n: builtins.length (declaredIn n) > 1) (builtins.attrNames allOwn);
      followed = builtins.filter (n: allOwn ? ${n}) (followsTargetsOf baseSerialized);
    in
    lib.unique (shared ++ followed);

  # This aspect is re-imported inside every partition (superset eval), so a
  # partition declares partitions of its own. Those are never read -- but the
  # write-flake hook below is part of every evaluation, and letting a partition
  # keep it would make the hook's text reference its own nested output: infinite
  # recursion the moment the app is built. Nothing else about the nested
  # partitions is ever forced, so dropping the hook is the whole guard. (Testing
  # `partitionStack` instead would not work: reading a module argument to decide
  # what a module defines is itself an infinite recursion.)
  noNestedHooks.flake-file.write-hooks = lib.mkForce [ ];

  # Every partition also writes its own flake.nix, from the same `ff.*`
  # declarations that live next to the aspects using them.
  subFlakeModule = bucket: {
    imports = aspectPartitions.parts.${bucket} ++ [ noNestedHooks ];
    flake-file = {
      intoPath = "partitions/${bucket}";
      # mkForce: a partition re-imports aspects/framework/flake-file.nix (and
      # aspects/base/caches.nix), which define these for the ROOT flake.nix.
      # Nothing ever evaluates a sub-flake's outputs or honours its nixConfig.
      outputs = lib.mkForce "_: { }";
      nixConfig = lib.mkForce { };
      do-not-edit = lib.mkForce ''
        # This file is generated by `nix run .#write-flake`. To make changes, edit the
        # `ff.*` declarations in the aspects listed under `${bucket}` in ../../partition-map.nix.
        #
        # It exists only to hold inputs and their lock: the root flake reads
        # `.inputs` out of it (see aspects/framework/partitions.nix) and never calls
        # `outputs`. `nixpkgs` is pinned to the root flake's locked rev so the two locks
        # cannot drift; it is dropped again before the inputs are merged into the
        # partition, and is here purely so the inputs below can `follows` it -- a
        # `follows` cannot reach the parent flake.
      '';
      # mkForce: base defines its own preProcess (hoisting), and a partition
      # re-imports that definition.
      preProcess = lib.mkForce (
        serialized:
        let
          own = builtins.removeAttrs serialized (baseInputNames ++ hoistedNames);

          # A `follows` can only name an input of the flake it is written in, so
          # a base input that a bucket input follows has to be carried into the
          # sub-flake too -- nix rejects the lock outright otherwise ("follows a
          # non-existent input"). Carried entries keep only `url`/`flake`; their
          # own `follows` would dangle in turn, and `subInputs` drops them again
          # so the partition still sees the root's copy.
          followsOf =
            spec:
            builtins.filter (f: f != null) (lib.mapAttrsToList (_: v: v.follows or null) (spec.inputs or { }));
          carried = lib.unique (
            builtins.filter (name: serialized ? ${name} && !(own ? ${name}) && name != "nixpkgs") (
              builtins.concatMap followsOf (builtins.attrValues own)
            )
          );
        in
        {
          nixpkgs.url = "github:nixos/nixpkgs/${realInputs.nixpkgs.rev}";
        }
        // lib.genAttrs carried (
          name: lib.filterAttrs (k: _: k == "url" || k == "flake") serialized.${name}
        )
        // own
      );
    };
  };

  partitionedInputs = lib.mergeAttrsList (builtins.map subInputs buckets);

  # `all` is base + every partitioned aspect: the evaluation that can see the
  # whole tree, and therefore the one every cross-cutting output is taken from.
  # It writes no flake.nix of its own -- each bucket already owns one.
  allPartition = {
    extraInputs = partitionedInputs;
    module = {
      imports = aspectPartitions.all ++ [ noNestedHooks ];
      # `checks` is taken from this partition, and that includes flake-file's
      # own check-flake-file, which diffs the committed root flake.nix against a
      # freshly generated one. Here the aspect tree also carries the partitioned
      # `ff.*` declarations, so subtract them back out or the check reports every
      # partitioned input as missing from the root flake.
      flake-file.preProcess = lib.mkForce (
        serialized: builtins.removeAttrs serialized (builtins.attrNames partitionedInputs)
      );
    };
  };
in
{
  imports = [ inputs.flake-parts.flakeModules.partitions ];

  ff.flake-compat = {
    url = "github:nixos/flake-compat";
    flake = false;
  };

  # Reachability says which hosts USE an aspect, not which files NAME one: a
  # base file can still write `den.aspects.karabiner-driver` even though only
  # Darwin hosts pull it in. Stub every partitioned aspect so those references
  # resolve to something inert here; the real definition is merged back in by
  # whichever partition imports its file.
  den.aspects = lib.genAttrs aspectPartitions.stubs (_: { });

  partitions =
    lib.genAttrs buckets (bucket: {
      extraInputs = subInputs bucket;
      module = subFlakeModule bucket;
    })
    // {
      all = allPartition;
    };

  # The root flake.nix takes back anything the buckets could not keep. Without
  # this a hoisted input is declared nowhere at all: it left the base evaluation
  # when its `ff` moved onto an aspect, and the buckets just dropped it.
  flake-file.preProcess = serialized: serialized // lib.getAttrs hoistedNames allOwn;

  # Which flake outputs come from `all` instead of the base evaluation. Keep this
  # list to attributes that are genuinely dev-time or need the whole host set:
  # every entry here is another full den pipeline run when it is evaluated.
  #
  # `packages` deliberately stays at base -- `packages.write-flake` regenerates
  # the root flake.nix from `config.flake-file.inputs`, and taking it from the
  # superset partition would write the partitioned inputs straight back in.
  partitionedAttrs = {
    apps = "all";
    checks = "all";
    ciMatrix = "all";
    darwinConfigurations = "darwin";
    deploy = "all";
    homeConfigurations = "all";
    nixosConfigurations = "nixos";
    partitionMap = "all";
  };

  # `nix flake update` only touches the root lock, so bring the partitions along:
  # regenerate their flake.nix (the `nixpkgs` pin tracks the root's new locked
  # rev) and re-lock each one. Re-entering through `nix run` rather than a baked
  # derivation is deliberate -- this hook was built from the pre-update
  # evaluation, so the pin it would write is the old one.
  perSystem = {
    update-hooks.postFlake.update-partition-flakes = ''
      nix run .#write-flake
      ${lib.concatMapStringsSep "\n" (bucket: "nix flake update --flake ./partitions/${bucket}") buckets}
    '';
  };

  # `.#write-flake` writes the root flake.nix, then this hook writes each
  # partition's. Reading a partition's flake-file forces that partition's
  # evaluation, which is why this is a hook on the app rather than something the
  # base evaluation does unconditionally.
  flake-file.write-hooks = [
    {
      index = 20;
      program =
        pkgs:
        pkgs.writeShellApplication {
          name = "write-partition-flakes";
          meta.description = "Generate partitions/*/flake.nix";
          text = lib.concatMapStringsSep "\n" (
            bucket: lib.getExe (config.partitions.${bucket}.module.flake-file.apps.write-flake pkgs)
          ) buckets;
        };
    }
  ];
}
