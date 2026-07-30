{
  inputs,
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

  # Root's own lock, so a carried input can be pinned to the exact rev main
  # resolved rather than re-resolved to whatever is current when the sub-flake
  # is locked. `nixpkgs` was already pinned this way by hand; every shared
  # `follows` target now goes through the same path, so a sub-flake lock can
  # never disagree with main on a carried input.
  rootLocked =
    let
      lock = builtins.fromJSON (builtins.readFile (rootPath + "/flake.lock"));
    in
    lib.mapAttrs (_: node: lock.nodes.${node}.locked) lock.nodes.${lock.root}.inputs;

  # Render a locked node back to a rev-pinned flake ref. Falls back to the bare
  # url for input types that cannot carry a rev inline (a plain tarball/path),
  # which is the pre-existing behaviour for those.
  pinnedRef =
    name: fallback:
    let
      l = rootLocked.${name} or null;
      dir = l.dir or null;
      suffix = lib.optionalString (dir != null) "?dir=${dir}";
    in
    if l == null || !(l ? rev) then
      fallback
    else if l.type == "github" || l.type == "gitlab" || l.type == "sourcehut" then
      "${l.type}:${l.owner}/${l.repo}/${l.rev}${suffix}"
    else if l.type == "git" then
      "git+${l.url}?rev=${l.rev}" + lib.optionalString (l ? ref) "&ref=${l.ref}"
    else
      fallback;

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
  # Hashes recorded FOR THIS BUCKET, written by `nix run .#write-patched-inputs`
  # next to the sub-flake they belong to.
  bucketPatchFile = bucket: rootPath + "/partitions/${bucket}/patched-inputs.nix";

  patchContextFor =
    bucket: raw:
    let
      inputsOnly = builtins.removeAttrs raw [ "self" ];
      recorded =
        if builtins.pathExists (bucketPatchFile bucket) then import (bucketPatchFile bucket) else { };
    in
    import (rootPath + "/lib/patched-inputs.nix") {
      inherit lib;
      inputs = inputsOnly;
      rootPath = rootPath + "/partitions/${bucket}";
      # Only specs for inputs this bucket actually provides: a spec's `src`
      # defaults to `inputs.<name>.sourceInfo`, which would not resolve here.
      #
      # The patch LISTS are shared with the root -- same aspect declarations --
      # but the recorded HASH is not. A hash pins the patched tree built from
      # one particular source rev, and a bucket pins its own; reusing the root's
      # would fail the fixed-output build, and a failing FOD takes down the very
      # evaluation that runs the generator meant to fix it. So a bucket uses
      # only a hash recorded for that bucket, and otherwise builds locally --
      # unsubstitutable, but never wedged.
      patchSpecs = lib.mapAttrs (
        name: spec: builtins.removeAttrs spec [ "hash" ] // { hash = recorded.${name}.hash or null; }
      ) (lib.filterAttrs (name: _: inputsOnly ? ${name}) (import (rootPath + "/patched-inputs.nix")));
    };

  rawSubFlakeInputs =
    bucket:
    (import inputs.flake-compat {
      src = rootPath + "/partitions/${bucket}";
      system = throw "flake-compat is loading a partition in pure mode; `system` must not be forced";
    }).outputs.inputs;

  subInputs =
    bucket:
    builtins.removeAttrs (patchContextFor bucket (rawSubFlakeInputs bucket)).newInputs (
      [ "self" ] ++ baseInputNames
    );

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
        # `outputs`. `nixpkgs` and every shared `follows` target are pinned to the
        # root flake's locked rev so the two locks cannot drift; each is dropped
        # again before the inputs are merged into the partition, and is here purely
        # so the inputs below can `follows` it -- a `follows` cannot reach the
        # parent flake.
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
          # non-existent input"). Carried entries are pinned to main's locked
          # rev (see pinnedRef): a bare url would re-resolve to whatever is
          # current when the sub-flake is locked, so its `follows` could then
          # resolve against a rev main never used. Their own `follows` are
          # dropped -- they would dangle in turn -- and `subInputs` drops the
          # carried input itself, so the partition still sees the root's copy.
          followsOf =
            spec:
            builtins.filter (f: f != null) (lib.mapAttrsToList (_: v: v.follows or null) (spec.inputs or { }));
          carried = lib.unique (
            builtins.filter (name: serialized ? ${name} && !(own ? ${name}) && name != "nixpkgs") (
              builtins.concatMap followsOf (builtins.attrValues own)
            )
          );
          pin =
            name: fallbackSpec:
            {
              url = pinnedRef name (fallbackSpec.url or null);
            }
            // lib.optionalAttrs (fallbackSpec ? flake) { inherit (fallbackSpec) flake; };
        in
        {
          nixpkgs = pin "nixpkgs" (serialized.nixpkgs or { });
        }
        // lib.genAttrs carried (name: pin name serialized.${name})
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

  # The per-bucket patch context, so `nix run .#write-patched-inputs` can build
  # and hash each bucket's patched trees. Functions and derivations, never
  # serialised -- this is an internal handle, not a consumer-facing output.
  flake.partitionPatchContext = lib.genAttrs buckets (
    bucket: patchContextFor bucket (rawSubFlakeInputs bucket)
  );

  ff.flake-compat = {
    url = "github:nixos/flake-compat";
    flake = false;
  };

  # Reachability says which hosts USE an aspect, not which files NAME one: a
  # base file can still write `den.aspects.karabiner-driver` even though only
  # Darwin hosts pull it in. Stub every partitioned aspect so those references
  # resolve to something inert here; the real definition is merged back in by
  # whichever partition imports its file.
  #
  # A stub entry is a slash path, so a NESTED aspect a base file navigates into
  # (`den.aspects.browsers.zen`) rebuilds as `{ browsers.zen = { }; }` rather than
  # a flat `browsers = { }` that would lose the `.zen` key. The inert `{ }` leaf
  # merges cleanly with the real definition inside whichever partition imports it.
  den.aspects = lib.foldl' lib.recursiveUpdate { } (
    map (p: lib.setAttrByPath (lib.splitString "/" p) { }) aspectPartitions.stubs
  );

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
