{
  inputs,
  lib,
  self,
  config,
  withSystem,
  rootPath,
  realInputs,
  ...
}:
let
  inherit (lib) mapAttrs;
  patchedInputModule =
    {
      config,
      name,
      ...
    }:
    {
      options = {
        src = lib.mkOption {
          type = lib.types.raw;
          default = realInputs.${name}.sourceInfo;
          description = "Source flake to patch (usually another `inputs.<x>`).";
        };
        autoIncludePatches = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = ''
            If true, automatically include any patch files found in
            `./patches/<name>/*.patch` (where `<name>` is the key of this input).
            This is convenient for local development, but may be undesirable if
            you want to control exactly which patches are applied.
          '';
        };
        isInput = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = ''
            If true, this patched flake is added to the `inputs` of the flake
            module. If false, it is built but not added to `inputs`.
          '';
        };
        hash = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = patchHashes.${name} or null;
          description = ''
            SRI hash of the *patched* source tree, making it a fixed-output
            derivation: the store path then depends only on the content, not
            on the system that built it, and it can be substituted from a
            binary cache instead of rebuilt on every machine.

            Defaults to the entry for this input in `./patches/hashes.json`.
            Refresh with `nix run .#update-patch-hashes` after changing a
            patch or bumping the upstream input — a stale hash fails the
            build with a hash mismatch.

            `null` opts out: the tree is built locally, unsubstitutable, and
            its path varies with the building system (the old behaviour).
          '';
        };
        patches = lib.mkOption {
          type = lib.types.listOf lib.types.path;
          default =
            if config.autoIncludePatches then
              with lib;
              ../../patches/${name}
              |> fileset.fileFilter (file: file.hasExt "patch" && !hasPrefix "_" file.name)
              |> fileset.toList
            else
              [ ];

          description = "Patch files (unified diffs) applied in order.";
        };
        prePatch = lib.mkOption {
          type = lib.types.lines;
          default = "";
        };
        postPatch = lib.mkOption {
          type = lib.types.lines;
          default = "";
        };
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = ''
            Enable this patched input. If false, the input is not built or
            added to `inputs`.
          '';
        };
      };
    };

  flipAttrs = lib.mapAttrs' (
    value: name: {
      inherit name value;
    }
  );

  # Recorded SRI hashes of the patched trees, feeding each entry's `hash`
  # default. A missing file (or a missing entry) yields `null`, which falls back
  # to a locally-built, unsubstitutable tree — so this is safe to bootstrap from
  # nothing: `nix run .#update-patch-hashes` writes it.
  patchHashes =
    let
      file = ../../patches/hashes.json;

      # Escape hatch. A recorded hash covers the patched *content*, so bumping a
      # patched input invalidates it — and because `flake.nix` computes
      # `newInputs` before anything else, a stale hash fails the fixed-output
      # build and takes the whole flake's evaluation down with it, including the
      # app that would refresh the hashes. Chicken, meet egg.
      #
      #   PATCH_HASHES=ignore nix run --impure .#update-patch-hashes
      #
      # drops back to the unhashed (non-fixed-output) path so the updater can
      # evaluate, rebuild the trees and rewrite this file. `nix run .#update`
      # does the same thing around a `nix flake update`.
      #
      # `getEnv` returns "" under pure eval, so this is inert everywhere else.
      ignore = builtins.getEnv "PATCH_HASHES" == "ignore";
    in
    if ignore || !builtins.pathExists file then { } else builtins.fromJSON (builtins.readFile file);

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
      allNodes = mapAttrs (
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
        }
      ) lockFile.nodes;
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
      extraInputs ? { },
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
      passedInputs = realInputs // inputs // extraInputs;
      lockFilePath = "${patchedSrc}/flake.lock";

      backupLockFile = builtins.fromJSON (builtins.readFile lockFilePath);

      backupNodes = nodesFn {
        rootKey = backupLockFile.root;
        lockFile = backupLockFile;
        flakePath = patchedSrc;
        passedInputs = passedInputs;
      };

      topLockFile = builtins.fromJSON (builtins.readFile "${rootPath}/flake.lock");

      allNodes = nodesFn {
        inherit passedInputs;
        lockFile = topLockFile;
        rootKey = name;
        backupNode = backupNodes.${backupLockFile.root};
        flakePath = "${rootPath}";
        patchSrc = "${patchedSrc}";
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
  # derivation. `update-patch-hashes` hashes *that*, so a stale recorded hash can
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
    ) (lib.filterAttrs (_: hasPatches) config.patchedInputs);

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
  patchedNames = lib.attrNames (lib.filterAttrs (_: v: v.isInput) config.patchedInputs);
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
    n: isFlakeInput n && !lib.elem n config.noUnify && lib.elem (resolveRef rootInputs.${n}) reaching
  ) (lib.attrNames rootInputs);

  # Everything to re-evaluate: explicit patchedInputs ∪ auto-unified. Explicit entries
  # are always built (they may carry patches) and cannot be dropped via `noUnify`.
  toBuild = lib.unique (patchedNames ++ autoUnify);

  # Effective per-input config, defaulting for inputs with no `patchedInputs` entry
  # (auto-unified inputs are re-evaluated with zero diffs against the patched graph).
  cfgFor =
    name:
    config.patchedInputs.${name} or {
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
      allInputs = lib.genAttrs toBuild (
        n:
        let
          p = cfgFor n;
        in
        patchFlake {
          inherit pkgs;
          extraInputs = allInputs;
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
  patchInputScript =
    pkgs:
    pkgs.writeShellApplication {
      name = "patch-input";
      runtimeInputs = [
        pkgs.git
        pkgs.coreutils
        pkgs.nix
      ];
      text = ''
        set -euo pipefail
        input="''${1:-}"
        patchName="''${2:-edit}"
        if [ -z "$input" ]; then
        	echo "usage: patch-input <input-name> [patch-name]" >&2
        	exit 2
        fi
        repo="$PWD"
        if [ ! -e "$repo/flake.nix" ]; then
        	echo "error: run from the flake root (no flake.nix in $repo)" >&2
        	exit 2
        fi

        echo "Resolving source of input '$input'…"
        src="$(nix eval --impure --raw --expr \
        	"builtins.toString (builtins.getFlake (toString $repo)).inputs.\"$input\"")"
        echo "  $src"

        work="$(mktemp -d -t "patch-$input.XXXXXX")"
        cp -R "$src/." "$work/"
        chmod -R u+w "$work"
        cd "$work"
        git init -q
        git add -A
        GIT_AUTHOR_NAME=patch GIT_AUTHOR_EMAIL=patch@local \
        	GIT_COMMITTER_NAME=patch GIT_COMMITTER_EMAIL=patch@local \
        	git commit -q -m pristine

        cat <<EOF

        Editing a copy of '$input' in:
          $work
        Make your changes, then exit this shell ('exit' / Ctrl-D) to capture the diff.
        Exit non-zero (e.g. 'exit 1') to abort without writing a patch.

        EOF

        out="$repo/patches/$input/$patchName.patch"
        if [ -e "$out" ]; then
        	git apply "$out"
        fi
        set +e
        "''${SHELL:-/bin/sh}"
        shellrc=$?
        set -e
        if [ "$shellrc" -ne 0 ]; then
        	echo "aborted (shell exited $shellrc); no patch written." >&2
        	exit "$shellrc"
        fi

        git add -A
        mkdir -p "$(dirname "$out")"
        # --no-ext-diff: bypass any external difftool (e.g. difftastic) from the
        # user's global git config, which would emit non-applicable output.
        # diff.noprefix=false: keep a/ b/ prefixes so `patch -p1` (applyPatches) works.
        git -c diff.noprefix=false diff --no-ext-diff --cached --binary HEAD >"$out"
        if [ ! -s "$out" ]; then
        	rm -f "$out"
        	echo "no changes detected; nothing written." >&2
        	exit 0
        fi
        echo "wrote $out"
        echo "Add it to patchedInputs.\"$input\".patches and rebuild."
      '';
    };

  # `nix run .#update-patch-hashes`
  #
  # Records the SRI hash of each patched tree in ./patches/hashes.json, which
  # feeds the `hash` option and so makes the trees fixed-output. Re-run after
  # changing a patch or bumping a patched input: the recorded hash covers the
  # patched *content*, so any change to either invalidates it and the build then
  # fails with a hash mismatch until refreshed.
  #
  # The unhashed derivations are interpolated below, so `nix run` builds them as
  # dependencies of this script — no flake output to plumb, and no dependency on
  # the (possibly stale) hashes already on disk.
  updateHashesScript =
    pkgs:
    let
      entries = lib.mapAttrsToList (n: drv: "${n} ${drv}") (patchedDrvsUnhashed pkgs);
    in
    pkgs.writeShellApplication {
      name = "update-patch-hashes";
      runtimeInputs = [
        pkgs.nix
        pkgs.jq
        pkgs.coreutils
      ];
      text = ''
        set -euo pipefail
        repo="$PWD"
        if [ ! -e "$repo/flake.nix" ]; then
        	echo "error: run from the flake root (no flake.nix in $repo)" >&2
        	exit 2
        fi

        out="$repo/patches/hashes.json"
        tmp="$(mktemp)"
        trap 'rm -f "$tmp" "$tmp.new"' EXIT
        echo '{}' >"$tmp"

        while read -r name path; do
        	[ -n "$name" ] || continue
        	hash="$(nix hash path --sri "$path")"
        	echo "  $name  $hash"
        	jq --arg n "$name" --arg h "$hash" '.[$n] = $h' "$tmp" >"$tmp.new"
        	mv "$tmp.new" "$tmp"
        done <<-'ENTRIES'
        	${lib.concatStringsSep "\n\t" entries}
        ENTRIES

        mkdir -p "$(dirname "$out")"
        jq -S . "$tmp" >"$out"
        echo "wrote $out"
      '';
    };
in
{
  options.patchSystem = lib.mkOption {
    type = lib.types.str;
    # No hardcoded system. Under impure eval `builtins.currentSystem` is the
    # host, so the trees build wherever they're evaluated. Under pure eval the
    # builtin is absent and we fall back to the flake's *own* first declared
    # system — derived, not a magic literal.
    #
    # For an input with a recorded `hash` the tree is fixed-output, so this
    # picks nothing about the resulting store path — every system produces the
    # identical output — it only decides which host can *build* it on a cold
    # cache (a warm cache substitutes it cross-system regardless). For an input
    # without a hash it still determines the path, as before.
    default = builtins.currentSystem or "x86_64-linux"; # or (builtins.head config.systems);
    defaultText = lib.literalExpression "builtins.currentSystem or (builtins.head config.systems)";
    description = ''
      System whose `pkgs` *builds* the patched source trees. See the comment in
      `patch-inputs.nix`; with a recorded `hash` this does not affect the output
      path, only which host can build on a cold cache.
    '';
  };

  options.noUnify = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = ''
      Input names to exclude from automatic input-unification re-eval — an escape
      hatch for a flake that does not survive `patchFlake`'s lock-resolver. Only
      removes AUTO-unified inputs (those pulled in because they transitively depend
      on a patched input); an input explicitly listed in `patchedInputs` is always
      built, since it may carry patches.
    '';
  };

  options.patchedInputs = lib.mkOption {
    default = { };
    description = ''
      Flake inputs to patch with local diffs. Each entry is applied with
      `pkgs.applyPatches` (which FAILS the build if a patch no longer applies)
      and re-loaded as a flake via flake-compat, then merged into the `inputs`
      argument seen by home-manager modules — reference it as `inputs.<name>`
      (e.g. `inputs.zen-browser-patched.homeModules.default`).

      Produce/refresh a patch with `nix run .#patch-input -- <name>`.

      Note: `inputs` at the flake-module level is a flake-parts specialArg bound
      to `self.inputs` and cannot be shadowed there; this injection happens in
      the home-manager scope (where patched flakes' modules are consumed and a
      per-system `pkgs` is available to build the patch).
    '';
    example = lib.literalExpression ''
      {
        zen-browser-patched = {
          src = inputs.zen-browser;
          patches = [ ../../patches/zen-browser/fix.patch ];
        };
      }
    '';
    type = lib.types.attrsOf (lib.types.submodule patchedInputModule);
  };

  imports = [
    (lib.inputMetaModules [
      (lib.mkAliasOptionModule [ "patch" ] [ "meta" "patch" ])
      ({ config, ... }: {
        options.meta = lib.mkOption {
          type = lib.types.submodule {
            options.patch = lib.mkOption {
              type = lib.types.submoduleWith {

                shorthandOnlyDefinesConfig = true;

                modules = [
                  patchedInputModule
                  {
                    config._module.args.name = lib.mkForce config._module.args.name;
                    config.hash = patchHashes.${config._module.args.name} or null;
                    config.src = realInputs.${config._module.args.name};
                    config.enable = lib.mkDefault false;
                  }
                ];
              };
            };
          };
        };

      })
    ])
  ];
  config = {
    #    flake-file.nixConfig.allowUnsupportedPlatform = true;
    patchedInputs =
      config.flake-file.inputsWithMeta
      |> lib.filterAttrs (_: v: v.meta.patch.enable)
      |> lib.mapAttrs (_: v: v.meta.patch);
    ff.flake-compat = {
      url = "github:nixos/flake-compat";
      flake = false;
    };
    flake.inputs = inputs;

    # Per-input overlay instead of `realInputs // buildPatched pkgs`. With `//`,
    # the patched value of EVERY re-evaluated input shares one attrset, and the
    # merge gives no syntactic hint that the rest are untouched. Building the
    # overlay with `mapAttrs` + a `patched.${name} or realInput` fallback keeps
    # evaluation lazy per input: reading an input not in `toBuild` (a non-flake,
    # or a flake that touches nothing patched) returns `realInputs` directly and
    # never forces `pkgs`, `applyPatches`, or the flake-compat re-eval — only the
    # names in `toBuild` enter the patch path. `buildPatched pkgs` stays a single
    # shared thunk, so re-evaluated inputs that reference each other still resolve
    # through the one `allInputs` fixpoint.
    #
    # Get pkgs directly from realInputs to avoid forcing perSystem evaluation:
    # using withSystem would trigger full perSystem module evaluation just to
    # compute newInputs, which adds significant eval overhead.
    flake.newInputs =
      let
        pkgs = realInputs.nixpkgs.legacyPackages.${config.patchSystem};
        patched = buildPatched pkgs;
      in
      lib.mapAttrs (name: realInput: patched.${name} or realInput) realInputs;

    perSystem = { pkgs, ... }: {
      apps.patch-input = {
        type = "app";
        program = lib.getExe (patchInputScript pkgs);
      };

      apps.update-patch-hashes = {
        type = "app";
        program = lib.getExe (updateHashesScript pkgs);
      };

      # `nix run .#update` — bump inputs, then refresh the patch hashes that
      # the bump just invalidated. Resolved and built *before* the lock moves,
      # so it still runs even though the flake won't evaluate in between.
      apps.update = {
        type = "app";
        program = lib.getExe (
          pkgs.writeShellApplication {
            name = "update";
            runtimeInputs = [ pkgs.nix ];
            text = ''
              set -euo pipefail
              nix flake update "$@"
              PATCH_HASHES=ignore nix run --impure .#update-patch-hashes
            '';
          }
        );
      };

      # Building these fails `nix flake check` if a declared patch is stale
      # (it no longer applies) or a recorded `hash` is stale (hash mismatch).
      # Entries with no patches are skipped: `patchSource` hands those back
      # as the pristine source path, which is not a derivation to build.
      checks =
        lib.mapAttrs' (n: _: {
          name = "patched-input-${n}";
          value = (patchedDrvs pkgs).${n};
        }) (lib.filterAttrs (_: hasPatches) config.patchedInputs)
        // {
          # Guard `buildPatched`'s fixpoint: every re-evaluated input — patched or
          # merely auto-unified — must resolve its own inputs to the *patched*
          # versions of them, not the pristine ones. E.g. `agenix` declares
          # `darwin` and `home-manager`, both patched, so severing the fixpoint
          # shows up here rather than as a mysteriously unpatched module at
          # rebuild time. Covers the whole `toBuild` set, not just declared patches.
          patched-inputs-intertwined =
            let
              patched = toBuild;
              mismatches = lib.concatMap (
                name:
                let
                  deps = lib.filter (d: lib.elem d patched) (lib.attrNames (inputs.${name}.inputs or { }));
                in
                lib.forEach (lib.filter (d: inputs.${name}.inputs.${d}.outPath != inputs.${d}.outPath) deps) (
                  d: "  ${name} sees ${d} = ${inputs.${name}.inputs.${d}.outPath}, want ${inputs.${d}.outPath}"
                )
              ) patched;
            in
            assert lib.assertMsg (mismatches == [ ]) ''
              patched inputs are not intertwined — a re-evaluated flake is seeing an
              unpatched dependency:
              ${lib.concatStringsSep "\n" mismatches}
            '';
            pkgs.emptyFile;
        };
    };
  };
}
