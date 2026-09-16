# The whole input pipeline, as one plain function: resolve the layer locks,
# merge them over the root's inputs, patch the result, hand it to mkFlake.
#
# flake.nix imports exactly this file. aspects/tooling/patch-inputs.nix imports
# it too, for the apps/checks, with the live specs instead of the generated
# ./patched-inputs.nix -- one implementation, not two.
#
# A layer is a real flake under ./partitions/<name> whose lock is read as DATA:
# its nodes never enter the root flake.lock, and each stays a `fetchTree` thunk
# only forced if something references it. Shared pins resolve through
# `passedInputs` to the root's already-resolved value, so a layer's copy of
# `nixpkgs` is materialised in its lock but never fetched a second time.
{
  inputs,
  lib,
  rootPath,
  # { <input> = { patches; prePatch ? ""; postPatch ? ""; isInput ? true; src ? <input>.sourceInfo; hash ? null; }; }
  patchSpecs,
  noUnify ? [ ],
  # Which system's pkgs BUILDS the patched trees. With a recorded hash the
  # trees are fixed-output, so this does not affect the resulting store path.
  patchSystem ? (builtins.currentSystem or "x86_64-linux"),
  # Layer directory names. Discovered by default: a layer exists because
  # ./partitions/<name>/flake.nix does, and nothing else records the list.
  layers ? (
    let
      dir = rootPath + "/partitions";
    in
    if !builtins.pathExists dir then
      [ ]
    else
      builtins.attrNames (
        lib.filterAttrs (
          # `root` is not a layer: those inputs live in the root flake.nix, and a
          # directory by that name would collide with the root source here.
          name: type:
          name != "root" && type == "directory" && builtins.pathExists (dir + "/${name}/flake.nix")
        ) (builtins.readDir dir)
      )
  ),
}:
let
  inherit (lib) mapAttrs;
  realInputs = inputs;

  # `TRACE_INPUTS=1` (with --impure) reports what an evaluation actually FORCES.
  # `getEnv` is "" under pure eval, so this is inert everywhere else -- same
  # arrangement as `PATCH_HASHES`.
  #
  # The point is to tell apart three things that get conflated:
  #   LOCK_READ  <layer>         the layer's flake.lock was parsed. Cheap, and
  #                              unavoidable: deciding who owns a name is lock
  #                              data. NOT the same as evaluating its inputs.
  #   INPUT_EVAL <name>          something read `inputs.<name>` off the merged
  #                              set.
  #   INPUT_EVAL <layer>/<name>  a layer's node was resolved -- this is the one
  #                              that fetches.
  #
  # So `TRACE_INPUTS=1 nix eval --impure .#darwinConfigurations.<host>...` should
  # print LOCK_READ for every layer and INPUT_EVAL for none of the ones that host
  # does not use.
  traceOn = builtins.getEnv "TRACE_INPUTS" == "1";
  traceAs = label: v: if traceOn then builtins.trace label v else v;

  readLock = path: traceAs "LOCK_READ ${toString path}" (builtins.fromJSON (builtins.readFile path));

  # ── Lock-graph resolution ────────────────────────────────────────────────────

  flipAttrs = lib.mapAttrs' (
    value: name: {
      inherit name value;
    }
  );

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

      # This node is a root input of THIS lock whose name an ancestor already
      # provides -- a CARRIED entry. Its value is the ancestor's, whole.
      inheritedName =
        if flipped ? "${key}" && key != rootKey && passedInputs ? "${flipped.${key}}" then
          flipped.${key}
        else
          null;

      # A stub that nothing replaced. Without this it falls through to the
      # non-flake branch and quietly yields the empty ./stubs directory, which
      # only fails much later and somewhere unrelated. A stub exists PURELY to
      # be substituted, so failing to substitute one is the error.
      isUnresolvedStub =
        inheritedName == null
        && !(node.flake or true)
        && (node.locked.type or "") == "path"
        && lib.hasSuffix "lib/stub" (node.locked.path or "");

      sourceInfo = (
        if key == lockFile.root then
          flakePath
        # BEFORE the `isRelative` branch: a carried entry is a `path:` stub, so
        # it IS relative, and that branch would chase a `parent` it has none of.
        else if inheritedName != null then
          passedInputs.${inheritedName}
        else if isRelative then
          parentNode.sourceInfo
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

      resolveInput =
        inputSpec: if builtins.isList inputSpec then getInputByPath lockFile.root inputSpec else inputSpec;

      outputs = flake.outputs (newInputs // { self = result; });
      getInputByPath =
        nodeName: path:
        if path == [ ] then
          nodeName
        else
          getInputByPath (resolveInput lockFile.nodes.${nodeName}.inputs.${builtins.head path}) (
            builtins.tail path
          );

      result =
        outputs
        // sourceInfo
        // {
          inherit outPath;
          inputs = newInputs;
          inherit outputs;
          inherit sourceInfo;
          _type = "flake";
        };
    in
    {
      result =
        # A carried entry is the ancestor's value VERBATIM -- not re-derived
        # from its path.
        #
        # Substituting only `sourceInfo` still left `outPath` feeding `import
        # (outPath + "/flake.nix")` and a fresh `flake.outputs` call, so every
        # layer carrying a name re-evaluated that flake: the tree was fetched
        # once, but `nur`'s (or nixpkgs') outputs were computed once per layer.
        # Returning the resolved value directly makes it one evaluation, shared.
        #
        # It is also what lets a carried entry be a non-flake placeholder: the
        # `node.flake` branch below would hand back a bare source tree, which
        # nothing following it could use as a flake.
        if inheritedName != null then
          passedInputs.${inheritedName}
        else if isUnresolvedStub then
          throw "lib/inputs.nix: stub '${key}' in ${toString flakePath} was never replaced -- no source provides that name. Run `nix run .#write-flake`."
        else if node.flake or true then
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

  # Resolve an input ref (a node key, or a `["a" "b"]` follows path from the
  # root) to a node key, within one lock.
  refIn =
    lock: ref:
    if builtins.isList ref then
      lib.foldl' (cur: seg: refIn lock lock.nodes.${cur}.inputs.${seg}) lock.root ref
    else
      ref;

  # ── Sources: the root flake plus every layer ─────────────────────────────────

  rootLock = readLock (rootPath + "/flake.lock");

  mkSource =
    {
      name,
      flakePath,
      lockFile,
      provides,
    }:
    {
      inherit
        name
        flakePath
        lockFile
        provides
        ;
      rootInputs = lockFile.nodes.${lockFile.root}.inputs or { };
    };

  rootSource = mkSource {
    name = "root";
    flakePath = rootPath;
    lockFile = rootLock;
    provides = realInputs;
  };

  layerSource =
    layer:
    let
      flakePath = rootPath + "/partitions/${layer}";
      lockPath = flakePath + "/flake.lock";
      lockFile =
        if builtins.pathExists lockPath then
          readLock lockPath
        else
          throw "lib/inputs.nix: partitions/${layer} has no flake.lock -- run `nix run .#write-flake`";
      # `passedInputs = mergedRaw`, not just the root's inputs: a shared name is
      # resolved from whichever source OWNS it, which need not be the root. A
      # layer carrying `nur` purely so its own `follows` has something to name
      # gets the real `nur` from the layer that owns it -- same tree, resolved
      # once, never fetched twice.
      #
      # Self-referential, and safe: deciding an owner reads only lock data
      # (`provides` is an attrset over the lock's root-node inputs, and
      # `ownsInLayer` a node lookup), so nothing here forces a node to decide
      # which source a name comes from.
      #
      # Minus the names this layer OWNS. Without that, the owner's own node
      # resolves through the merged set back to itself -- `mergedRaw.nur` is
      # this very node -- and the recursion has no base case. Only a layer's
      # CARRIED names may resolve elsewhere.
      inherited = builtins.removeAttrs mergedRaw (
        lib.filter (n: (ownerOf n).name == layer) (lib.attrNames rootInputs)
      );
      nodes = nodesFn {
        inherit lockFile flakePath;
        rootKey = lockFile.root;
        passedInputs = inherited;
      };
      rootInputs = lockFile.nodes.${lockFile.root}.inputs or { };
    in
    mkSource {
      name = layer;
      inherit flakePath lockFile;
      provides = mapAttrs (
        n: k: traceAs "INPUT_EVAL ${layer}/${n}" nodes.${refIn lockFile k}.result
      ) rootInputs;
    };

  layerSources = map layerSource layers;
  allSources = [ rootSource ] ++ layerSources;
  sourceByName = lib.listToAttrs (
    map (s: {
      inherit (s) name;
      value = s;
    }) allSources
  );

  # ── Ownership and merge ──────────────────────────────────────────────────────

  # A root input that exists only so a `follows` has a name to resolve against.
  # Its value is never used: whichever layer owns the real input wins. Derived
  # from the lock (a non-flake `path:` input under ./stubs), so there is no
  # second list to keep in step.
  isStub =
    name:
    let
      ref = rootSource.rootInputs.${name} or null;
      node = if ref == null then null else rootLock.nodes.${refIn rootLock ref};
    in
    node != null
    && !(node.flake or true)
    && (node.locked.type or "") == "path"
    && lib.hasSuffix "lib/stub" (node.locked.path or "");

  layersProviding = name: lib.filter (s: s.provides ? ${name}) layerSources;

  # A CARRIED entry -- one a layer declares only so its own `follows` has a name
  # to resolve against -- is generated pinned to the owner's locked rev with its
  # OWN `follows` stripped (see layerFlakeText). It still locks whatever its
  # upstream declares, so "has no inputs" does not tell the two apart; "has no
  # FOLLOWS" does. A follows shows up in the lock as a list-valued input ref (a
  # path from the lock root) rather than a node name.
  #
  # A flake whose spec genuinely carries no `follows` is indistinguishable, and
  # correctly so: two layers owning such a name really is ambiguous.
  ownsInLayer =
    s: name:
    let
      node = s.lockFile.nodes.${refIn s.lockFile s.rootInputs.${name}} or { };
    in
    lib.any builtins.isList (lib.attrValues (node.inputs or { }));

  # Root first. A shared pin is CARRIED into every layer that needs to `follows`
  # it -- `follows` cannot cross a flake boundary, so each layer must declare it
  # and `nix flake lock` materialises a node for it. Those copies are inert: the
  # root owns the name, so several layers carrying it is sharing, not conflict.
  # Two layers owning a name the root does NOT have is the real error, because
  # nothing then decides which one every host resolves.
  ownerOf =
    name:
    let
      ls = layersProviding name;
      rootOwns = realInputs ? ${name} && !(isStub name);
    in
    if rootOwns || ls == [ ] then
      rootSource
    else if lib.length ls == 1 then
      lib.head ls
    else
      let
        owners = lib.filter (s: ownsInLayer s name) ls;
      in
      if lib.length owners == 1 then
        lib.head owners
      else
        throw "lib/inputs.nix: '${name}' is claimed by ${
          lib.concatMapStringsSep ", " (s: s.name) ls
        } and the root does not provide it; give the shared name to the root instead";

  mergedNames = lib.unique (
    builtins.attrNames realInputs ++ lib.concatMap (s: builtins.attrNames s.provides) layerSources
  );

  # What mkFlake would get with no patching: one flat attrset, each name taken
  # from the single source that owns it.
  mergedRaw = lib.genAttrs mergedNames (name: (ownerOf name).provides.${name});

  # ── Patching ─────────────────────────────────────────────────────────────────

  # The registry is shared across the root and every layer, so a spec only
  # applies where its input exists. `isInput = false` entries patch non-input
  # trees and carry their own `src`, so they are kept regardless.
  applicableSpecs = lib.filterAttrs (
    name: spec: (spec.isInput or true) -> (mergedRaw ? ${name})
  ) patchSpecs;

  # `getEnv` returns "" under pure eval, so this is inert everywhere else.
  ignoreHashes = builtins.getEnv "PATCH_HASHES" == "ignore";

  patchedInputs = lib.mapAttrs (name: spec: {
    patches = spec.patches or [ ];
    prePatch = spec.prePatch or "";
    postPatch = spec.postPatch or "";
    isInput = spec.isInput or true;
    src = spec.src or mergedRaw.${name}.sourceInfo;
    # `PATCH_HASHES=ignore` must defeat a recorded hash even when the spec
    # carries one inline. A stale hash fails its fixed-output build, which takes
    # down the evaluation that runs the app meant to refresh it; this is the way
    # out.
    hash = if ignoreHashes then null else (spec.hash or null);
  }) applicableSpecs;

  hasPatches = p: p.patches != [ ] || p.prePatch != "" || p.postPatch != "";

  # `applyPatches` FAILS the build if a patch no longer applies -- the "tell me
  # when it breaks" guarantee.
  #
  # With `hash` set the tree is fixed-output: identical path on every system,
  # and substitutable, so realising it (this is IFD) is a fetch rather than a
  # build. `applyPatches` hardcodes `allowSubstitutes = false` /
  # `preferLocalBuild = true` in its `extendDrvArgs`, which merges OVER caller
  # args, so they can only be cleared via overrideAttrs.
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

  # Load a flake from a *patched* source tree, re-resolving its inputs against
  # the patched dependency graph.
  #
  # `builtins.getFlake` cannot be used: it refuses a store path carrying
  # derivation string-context (the patched tree is IFD).
  patchFlake =
    {
      pkgs,
      src,
      name,
      lockFile,
      flakePath,
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

      backupLockFile = readLock lockFilePath;

      backupNodes = nodesFn {
        rootKey = backupLockFile.root;
        lockFile = backupLockFile;
        flakePath = patchedSrc;
        inherit passedInputs;
      };

      # Reuse the shared, rootKey-independent walk of this source's lock graph,
      # overriding only this entry's OWN node. Every other node resolves
      # identically regardless of which entry is being patched, so there is no
      # need to re-derive the whole graph per entry.
      allNodes = sharedTopNodes // {
        ${name} = mkResolvedNode {
          inherit
            lockFile
            passedInputs
            allNodes
            flakePath
            ;
          rootKey = name;
          backupNode = backupNodes.${backupLockFile.root};
          patchSrc = "${patchedSrc}";
        } name lockFile.nodes.${name};
      };

      flakeNode = allNodes.${name};

      curInputs = (
        (builtins.intersectAttrs backupLockFile.nodes.${backupLockFile.root}.inputs (
          lib.mergeAttrsList [
            mergedRaw
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
        # Expose the unified inputs, so `inputs.<name>.inputs.<dep>` reflects the
        # patched graph rather than the flake's own pinned nodes.
        // {
          inputs = curInputs;
        }
        // ((import (patchedSrc + flakeNode.extraPathStuff + "/flake.nix")).outputs curInputs);
    in
    if !(builtins.pathExists lockFilePath) then (callLocklessFlake patchedSrc) else res;

  # Only entries that actually rewrite their tree produce a derivation; the rest
  # short-circuit to the pristine source in `patchSource`.
  #
  # `hashed = false` builds the very same tree as an ordinary derivation.
  # `write-patched-inputs` hashes THAT, so a stale recorded hash can never
  # poison the value used to refresh itself.
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
  # Re-evaluating a flake through `patchFlake` is only meaningful if it
  # transitively depends on a patched input; otherwise the re-eval reproduces
  # identical outputs at needless cost. Computed purely from lock data (no IFD),
  # per source, because an input's dependency graph lives in the lock that
  # declares it.

  patchedNames = lib.attrNames (lib.filterAttrs (_: v: v.isInput) patchedInputs);

  # Names this source owns, i.e. the ones whose graph is its to walk.
  ownedBy = s: lib.filter (n: (ownerOf n).name == s.name) (lib.attrNames s.rootInputs);

  autoUnifyIn =
    s:
    let
      lock = s.lockFile;
      # A patched name present in THIS lock, whether the source owns it or not:
      # a layer's carried `home-manager` still makes everything following it
      # reach a patched node.
      patchedKeys = map (n: refIn lock s.rootInputs.${n}) (
        lib.filter (n: s.rootInputs ? ${n}) patchedNames
      );
      allKeys = lib.attrNames lock.nodes;
      edgesOf = key: map (refIn lock) (lib.attrValues (lock.nodes.${key}.inputs or { }));
      # Reverse reachability by BFS over predecessors. genericClosure memoises
      # by `key`, so it terminates on cycles.
      reaching = map (i: i.key) (
        builtins.genericClosure {
          startSet = map (k: { key = k; }) patchedKeys;
          operator = item: map (k: { key = k; }) (lib.filter (k: lib.elem item.key (edgesOf k)) allKeys);
        }
      );
      isFlakeInput = name: lock.nodes.${refIn lock s.rootInputs.${name}}.flake or true;
    in
    lib.filter (
      n: isFlakeInput n && !lib.elem n noUnify && lib.elem (refIn lock s.rootInputs.${n}) reaching
    ) (ownedBy s);

  autoUnify = lib.concatMap autoUnifyIn allSources;

  # Explicit patchedInputs entries are always built (they may carry real diffs)
  # and cannot be dropped via `noUnify`.
  toBuild = lib.unique (patchedNames ++ autoUnify);

  cfgFor =
    name:
    patchedInputs.${name} or {
      src = mergedRaw.${name}.sourceInfo;
      patches = [ ];
      prePatch = "";
      postPatch = "";
      hash = null;
      isInput = true;
    };

  buildPatched =
    pkgs:
    let
      # Depends only on `mergedRaw` (constant) and `allInputs` itself -- the same
      # self-referential fixpoint every entry threads through -- so it is one
      # value, not one per entry.
      passedInputs = mergedRaw // allInputs;

      # Each source's lock graph walked ONCE with `rootKey = null`, a value no
      # real node key equals, so every node takes the branch it would under any
      # other entry's call where it is not that entry's own key.
      sharedTop = lib.genAttrs (map (s: s.name) allSources) (
        n:
        nodesFn {
          inherit passedInputs;
          inherit (sourceByName.${n}) lockFile flakePath;
          rootKey = null;
        }
      );

      allInputs = lib.genAttrs toBuild (
        n:
        let
          p = cfgFor n;
          s = ownerOf n;
        in
        patchFlake {
          inherit pkgs passedInputs;
          inherit (s) lockFile flakePath;
          sharedTopNodes = sharedTop.${s.name};
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

  # ── The gate ────────────────────────────────────────────────────────────────
  #
  # `LAYER_GATE_DENY="nixos"` (space- or comma-separated tags, needs --impure)
  # makes every input owned by a layer carrying one of those tags THROW when
  # forced. An evaluation that completes under the gate provably never touched
  # them -- which a trace cannot establish, since a trace only reports what was
  # forced on that run and stays silent about a path not taken.
  #
  # The denied tags are supplied by the caller rather than derived here, so the
  # layer vocabulary lives in exactly one place (../aspects/framework/layers.nix)
  # and is not duplicated into this file to rot.
  #
  # Inert without the variable: `getEnv` is "" under pure eval, so an ordinary
  # build is unaffected and pays nothing.
  gateDeny =
    let
      raw = builtins.getEnv "LAYER_GATE_DENY";
    in
    lib.filter (t: t != "") (lib.splitString " " (builtins.replaceStrings [ "," ] [ " " ] raw));

  # A layer name is its tags joined by "-", so membership is a tag test.
  layerDenied = layer: lib.any (t: lib.elem t (lib.splitString "-" layer)) gateDeny;

  gateInput =
    name: value:
    let
      owner = (ownerOf name).name;
    in
    if gateDeny != [ ] && owner != "root" && layerDenied owner then
      throw "lib/inputs.nix: input '${name}' comes from layer '${owner}', which this evaluation is gated against (LAYER_GATE_DENY=${lib.concatStringsSep "," gateDeny}). Something resolved for this host reaches an input belonging to another platform."
    else
      value;

  # The merged, patched `inputs` attrset mkFlake is handed. Lazy per input:
  # reading one not in `toBuild` never forces `pkgs`, `applyPatches`, or a
  # re-eval, and reading one the root owns never touches a layer lock.
  newInputs =
    let
      pkgs = realInputs.nixpkgs.legacyPackages.${patchSystem};
      patched = buildPatched pkgs;
    in
    lib.mapAttrs (
      name: raw: gateInput name (traceAs "INPUT_EVAL ${name}" (patched.${name} or raw))
    ) mergedRaw;
in
{
  inherit
    newInputs
    mergedRaw
    buildPatched
    toBuild
    patchedInputs
    patchedDrvs
    patchedDrvsUnhashed
    hasPatches
    nodesFn
    layers
    ;
}
