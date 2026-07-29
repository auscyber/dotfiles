# Patched-input resolution, as a PLAIN function (no flake-parts).
#
# Extracted from aspects/tooling/patch-inputs.nix so that BOTH
#   * flake.nix, which needs the patched `inputs` before it can build the real
#     flake, and
#   * aspects/tooling/patch-inputs.nix, which exposes the options/apps/checks,
# run the exact same code. There is one implementation, not two.
#
# flake.nix used to obtain the patched inputs by running the whole flake-parts
# module system once (`(inputsFn inputs).newInputs`) and then running it a
# SECOND time with the result. Every aspect module was therefore evaluated
# twice, in two separate fixpoints that share nothing. Calling this file
# directly removes that first pass.
{
  inputs,
  lib,
  rootPath,
  # { <input> = { patches; prePatch ? ""; postPatch ? ""; isInput ? true; src ? <flake input>; }; }
  patchSpecs,
  noUnify ? [ ],
  # Which system's pkgs BUILDS the patched trees. With a recorded hash the
  # trees are fixed-output, so this does not affect the resulting store path.
  patchSystem ? (builtins.currentSystem or "x86_64-linux"),
}:
let
  inherit (lib) mapAttrs;
  realInputs = inputs;

  # Fill in the same defaults the `patchedInputModule` option declarations
  # provide, so a spec only has to carry what differs.
  patchedInputs = lib.mapAttrs (name: spec: {
    patches = spec.patches or [ ];
    prePatch = spec.prePatch or "";
    postPatch = spec.postPatch or "";
    isInput = spec.isInput or true;
    src = spec.src or realInputs.${name}.sourceInfo;
    # `PATCH_HASHES=ignore` must still defeat a recorded hash even when the
    # spec carries one inline (the generated ../patched-inputs.nix does).
    # That escape hatch is the only way out of the chicken-and-egg a stale
    # hash creates: the fixed-output build fails, which takes the whole
    # flake's evaluation down with it — including the app that would refresh
    # the hash. Reading it from the spec instead of hashes.json must not
    # quietly remove that recovery path.
    hash = if ignoreHashes then null else (spec.hash or (patchHashes.${name} or null));
  }) patchSpecs;

  # `getEnv` returns "" under pure eval, so this is inert everywhere else.
  ignoreHashes = builtins.getEnv "PATCH_HASHES" == "ignore";

  flipAttrs = lib.mapAttrs' (
    value: name: {
      inherit name value;
    }
  );

  # Every caller supplies `hash` in its spec — flake.nix from the generated
  # ./patched-inputs.nix, the aspects module from the `hash` option (whose
  # default reads that same generated file). There is no separate hashes.json
  # to consult, so an absent `hash` simply means "no recorded hash": build the
  # tree locally, unsubstitutably.
  patchHashes = { };
  mkResolvedNode =
    {
      lockFile,
      rootKey,
      passedInputs,
      flakePath,
      backupNode,
      patchSrc,
      allNodes,
    }:
    key: node:
    let
      isRelative = node.locked.type or null == "path" && builtins.substring 0 1 node.locked.path != "/";

      parentNode = allNodes.${getInputByPath lockFile.root node.parent};
      flipped = flipAttrs (lockFile.nodes.${lockFile.root}.inputs or { });

      sourceInfo = (
        if key == lockFile.root then
          flakePath
        else if isRelative then
          parentNode.sourceInfo
        else if flipped ? "${key}" && key != rootKey && passedInputs ? "${flipped.${key}}" then
          (passedInputs.${flipped.${key}})
        else
          fetchTree (node.info or { } // removeAttrs node.locked [ "dir" ])
      );

      subdir = if key == lockFile.root then "" else node.locked.dir or "";

      outPath = (
        if key == rootKey && lockFile.root != key then
          patchSrc
        else if isRelative then
          parentNode.outPath + (if node.locked.path == "" then "" else "/" + node.locked.path)
        else
          sourceInfo.outPath + (if subdir == "" then "" else "/" + subdir)
      );

      flake = import (outPath + "/flake.nix");
      backupInputs = if backupNode != null && key == rootKey then backupNode.result.inputs else { };

      newInputs =
        backupInputs
        // lib.mapAttrs (_inputName: inputSpec: allNodes.${resolveInput inputSpec}.result) (
          node.inputs or { }
        );

      # Resolve a input spec into a node name. An input spec is
      # either a node name, or a 'follows' path from the root
      # node.
      resolveInput =
        inputSpec: if builtins.isList inputSpec then getInputByPath lockFile.root inputSpec else inputSpec;

      # Follow an input path (e.g. ["dwarffs" "nixpkgs"]) from the
      # root node, returning the final node.

      outputs = flake.outputs (newInputs // { self = result; });
      getInputByPath =
        nodeName: path:
        if path == [ ] then
          nodeName
        else
          getInputByPath
            # Since this could be a 'follows' input, call resolveInput.
            (resolveInput lockFile.nodes.${nodeName}.inputs.${builtins.head path})
            (builtins.tail path);

      result =
        outputs
        // sourceInfo
        // {
          # This shadows the sourceInfo.outPath
          inherit outPath;

          inputs = newInputs;
          inherit outputs;
          inherit sourceInfo;
          _type = "flake";
        };
    in
    {
      result =
        if node.flake or true then
          assert builtins.isFunction flake.outputs;
          result
        else
          sourceInfo // { inherit sourceInfo outPath; };
      extraPathStuff =
        if isRelative then
          (if node.locked.path == "" then "" else "/" + node.locked.path)
        else
          (if subdir == "" then "" else "/" + subdir);

      inherit sourceInfo outPath;
    };

  nodesFn =
    {
      lockFile,
      rootKey,
      passedInputs ? { },
      flakePath,
      backupNode ? null,
      patchSrc ? null,
    }:
    let
      allNodes = mapAttrs (mkResolvedNode {
        inherit
          lockFile
          rootKey
          passedInputs
          flakePath
          backupNode
          patchSrc
          allNodes
          ;
      }) lockFile.nodes;
    in
    allNodes;

  # True when an entry actually rewrites its source tree. Entries that don't are
  # still meaningful: listing an input re-evaluates it against the *patched*
  # dependency graph (see `buildPatched`), even with zero diffs to apply.
  hasPatches = p: p.patches != [ ] || p.prePatch != "" || p.postPatch != "";

  # Produce a patched source tree. `applyPatches` FAILS the build if any patch
  # no longer applies — that's the "tell me when it breaks" guarantee.
  #
  # With `hash` set, the tree is fixed-output: its store path derives from the
  # content hash and name alone. That makes the path identical on every system
  # and across nixpkgs bumps, and — crucially — makes it *substitutable*, so
  # realising it (this is IFD; `patchFlake` imports the result) is a cache fetch
  # rather than a build. Without that, the evaluating machine must build the
  # tree itself, with a builder matching the derivation's `system`.
  #
  # `applyPatches` hardcodes `allowSubstitutes = false` / `preferLocalBuild =
  # true` in its `extendDrvArgs`, which merges OVER caller args — passing them as
  # arguments is silently dropped, so they can only be cleared via overrideAttrs.
  patchSource =
    {
      pkgs,
      src,
      patches ? [ ],
      name ? "patched-src",
      prePatch ? "",
      postPatch ? "",
      hash ? null,
    }:
    if !(hasPatches { inherit patches prePatch postPatch; }) then
      # Nothing to apply: hand back the pristine tree. No derivation, no IFD, no
      # hash to keep in sync.
      src
    else
      (pkgs.applyPatches {
        inherit
          name
          src
          patches
          prePatch
          postPatch
          ;
      }).overrideAttrs
        (
          _:
          lib.optionalAttrs (hash != null) {
            outputHash = hash;
            outputHashAlgo = "sha256";
            outputHashMode = "recursive";
            allowSubstitutes = true;
            preferLocalBuild = false;
          }
        );

  callLocklessFlake =
    flakeSrc:
    let
      flake = import (flakeSrc + "/flake.nix");
      outputs = flakeSrc // (flake.outputs { self = outputs; });
    in
    outputs;

  # Load a flake from a *patched* source tree.
  #
  # `builtins.getFlake` can't be used here: it refuses a store path that still
  # carries derivation string-context (the patched tree is IFD), erroring with
  # "not allowed to refer to a store path" / "path '…' does not exist".
  # `flake-compat` realises the patched tree and evaluates its `outputs` with
  # the flake's own lock — the getFlake-from-source behaviour we want, and it
  # works with jujutsu (unlike the git-branch input-branches trick).
  patchFlake =
    {
      pkgs,
      src,
      name,
      patches ? [ ],
      prePatch ? "",
      postPatch ? "",
      hash ? null,
      passedInputs,
      sharedTopNodes,
      ...
    }:
    let
      patchedSrc = patchSource {
        name = "${name}-patched";
        inherit
          pkgs
          src
          patches
          prePatch
          postPatch
          hash
          ;
      };
      lockFilePath = "${patchedSrc}/flake.lock";

      backupLockFile = builtins.fromJSON (builtins.readFile lockFilePath);

      backupNodes = nodesFn {
        rootKey = backupLockFile.root;
        lockFile = backupLockFile;
        flakePath = patchedSrc;
        passedInputs = passedInputs;
      };

      # Reuse the shared, rootKey-independent walk of the root lock graph
      # (`sharedTopNodes`, built once in `buildPatched`), overriding only this
      # entry's OWN node. Every other node resolves identically to
      # `sharedTopNodes` regardless of which entry is being patched — see
      # `sharedTopNodes`'s comment — so there's no need to re-derive the whole
      # ~223-node graph again for each of the handful of patched/auto-unified
      # inputs.
      allNodes = sharedTopNodes // {
        ${name} = mkResolvedNode {
          inherit lockFile passedInputs allNodes;
          rootKey = name;
          flakePath = "${rootPath}";
          backupNode = backupNodes.${backupLockFile.root};
          patchSrc = "${patchedSrc}";
        } name lockFile.nodes.${name};
      };

      flakeNode = allNodes.${name};

      curInputs = (
        (builtins.intersectAttrs backupLockFile.nodes.${backupLockFile.root}.inputs (
          lib.mergeAttrsList [
            realInputs
            backupNodes.${backupLockFile.root}.result.inputs
            passedInputs
          ]
        ))
        // {
          self = res;
        }
      );
      res =
        (lib.removeAttrs flakeNode.result [
          "outputs"
          "inputs"
        ])
        # Expose the unified inputs (the same `curInputs` the flake's outputs are
        # evaluated with, `self` included), so `inputs.<name>.inputs.<dep>` reflects
        # the *patched* dependency graph — not the flake's own pinned nodes that
        # nodesFn recorded.
        // {
          inputs = curInputs;
        }
        // ((import (patchedSrc + flakeNode.extraPathStuff + "/flake.nix")).outputs curInputs);
    in
    if !(builtins.pathExists lockFilePath) then (callLocklessFlake patchedSrc) else res;
  # Only entries that actually rewrite their tree produce a derivation; the rest
  # short-circuit to the pristine source in `patchSource`, which is not a
  # derivation and so must not reach `checks`/`patchedSources`.
  # `hashed = false` builds the very same tree as an ordinary (non-fixed-output)
  # derivation. `write-patched-inputs` hashes *that*, so a stale recorded hash can
  # never poison the value used to refresh itself.
  patchedDrvsWith =
    { hashed }:
    pkgs:
    lib.mapAttrs (
      n: p:
      patchSource {
        inherit pkgs;
        inherit (p)
          src
          patches
          prePatch
          postPatch
          ;
        hash = if hashed then p.hash else null;
        name = "${n}-patched";
      }
    ) (lib.filterAttrs (_: hasPatches) patchedInputs);

  patchedDrvs = patchedDrvsWith { hashed = true; };
  patchedDrvsUnhashed = patchedDrvsWith { hashed = false; };

  # ── Which inputs to re-evaluate ("input unification") ────────────────────────
  #
  # Re-evaluating a flake through `patchFlake` re-resolves its inputs against the
  # *patched* dependency graph. That's only meaningful for a flake that actually
  # (transitively) depends on a patched input — otherwise the re-eval reproduces
  # the identical outputs at needless cost. So we compute, purely from the top
  # `flake.lock` (no IFD), the set of flake inputs whose dependency closure reaches
  # a patched input, and re-evaluate exactly those (plus any explicitly listed in
  # `patchedInputs`, which may carry real diffs).
  lockFile = builtins.fromJSON (builtins.readFile "${rootPath}/flake.lock");
  rootInputs = lockFile.nodes.${lockFile.root}.inputs or { };

  # Resolve an input ref (a node-key string, or a `["a" "b"]` follows-path from the
  # root) to a node key — the same resolution nodesFn does with resolveInput/getInputByPath.
  resolveRef = ref: if builtins.isList ref then resolvePath lockFile.root ref else ref;
  resolvePath =
    node: path:
    if path == [ ] then
      node
    else
      resolvePath (resolveRef lockFile.nodes.${node}.inputs.${builtins.head path}) (builtins.tail path);

  isFlakeInput = name: lockFile.nodes.${resolveRef rootInputs.${name}}.flake or true;

  # Root inputs that become patched inputs (isInput), and their lock node keys.
  patchedNames = lib.attrNames (lib.filterAttrs (_: v: v.isInput) patchedInputs);
  patchedKeys = map (n: resolveRef rootInputs.${n}) patchedNames;

  # Reverse reachability: every node that can reach a patched node, via BFS over
  # predecessors. genericClosure memoises by `key`, so it terminates on cycles.
  allKeys = lib.attrNames lockFile.nodes;
  edgesOf = key: map resolveRef (lib.attrValues (lockFile.nodes.${key}.inputs or { }));
  reaching = map (i: i.key) (
    builtins.genericClosure {
      startSet = map (k: { key = k; }) patchedKeys;
      operator = item: map (k: { key = k; }) (lib.filter (k: lib.elem item.key (edgesOf k)) allKeys);
    }
  );

  # Root input names that (transitively) depend on a patched input — auto-unified,
  # minus any explicit `noUnify` opt-outs. (A non-flake input has no `inputs`, so it
  # never reaches a patched node; `isFlakeInput` is a belt-and-braces guard.)
  autoUnify = lib.filter (
    n: isFlakeInput n && !lib.elem n noUnify && lib.elem (resolveRef rootInputs.${n}) reaching
  ) (lib.attrNames rootInputs);

  # Everything to re-evaluate: explicit patchedInputs ∪ auto-unified. Explicit entries
  # are always built (they may carry patches) and cannot be dropped via `noUnify`.
  toBuild = lib.unique (patchedNames ++ autoUnify);

  # Effective per-input config, defaulting for inputs with no `patchedInputs` entry
  # (auto-unified inputs are re-evaluated with zero diffs against the patched graph).
  cfgFor =
    name:
    patchedInputs.${name} or {
      src = realInputs.${name}.sourceInfo;
      patches = [ ];
      prePatch = "";
      postPatch = "";
      hash = patchHashes.${name} or null;
      isInput = true;
    };

  buildPatched =
    pkgs:
    let
      # Shared across every patched/auto-unified entry: `passedInputs` only
      # depends on `realInputs`/`inputs` (constant) and `allInputs` itself (the
      # same self-referential fixpoint every entry already threads through via
      # cross-references), so it's one value, not one per entry.
      passedInputs = realInputs // inputs // allInputs;

      # The root lock graph, walked ONCE with `rootKey = null` — a value no
      # real node key ever equals, so every node takes the same branch it
      # would under any OTHER entry's call where it isn't THAT entry's own key
      # (see `mkResolvedNode`'s `key == rootKey` checks). `patchFlake` reuses
      # this unchanged for every key except the one it's patching, instead of
      # re-deriving the whole ~223-node graph per entry.
      sharedTopNodes = nodesFn {
        inherit passedInputs lockFile;
        rootKey = null;
        flakePath = "${rootPath}";
      };

      allInputs = lib.genAttrs toBuild (
        n:
        let
          p = cfgFor n;
        in
        patchFlake {
          inherit pkgs passedInputs sharedTopNodes;
          inherit (p)
            src
            patches
            prePatch
            postPatch
            hash
            ;
          name = "${n}";
        }
      );
    in
    allInputs;

  # `nix run .#patch-input -- <input> [patchName]`
  #  - copies the input's pristine source into a temp git repo
  #  - drops you into $SHELL to edit it
  #  - on exit, writes `git diff` to ./patches/<input>/<patchName>.patch
  # The patched `inputs` attrset: every input in `toBuild` replaced by its
  # re-evaluated (patched) self, everything else passed through untouched.
  # Lazy per input — reading one that is not in `toBuild` never forces `pkgs`,
  # `applyPatches`, or the flake-compat re-eval.
  newInputs =
    let
      pkgs = realInputs.nixpkgs.legacyPackages.${patchSystem};
      patched = buildPatched pkgs;
    in
    lib.mapAttrs (name: realInput: patched.${name} or realInput) realInputs;
in
{
  inherit
    newInputs
    buildPatched
    toBuild
    patchedInputs
    patchedDrvs
    patchedDrvsUnhashed
    hasPatches
    ;
}
