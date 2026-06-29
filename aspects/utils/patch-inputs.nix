{
  inputs,
  lib,
  config,
  withSystem,
  rootPath,
  realInputs,
  ...
}:
let
  # Produce a patched source tree. `applyPatches` FAILS the build if any patch
  # no longer applies — that's the "tell me when it breaks" guarantee.
  inherit (lib) mapAttrs;

  nodesFn =

    {
      lockFile,
      rootKey,
      passedInputs ? { },
      flakePath,
    }:
    let
      allNodes = mapAttrs (
        key: node:
        let
          isRelative = node.locked.type or null == "path" && builtins.substring 0 1 node.locked.path != "/";

          parentNode = allNodes.${getInputByPath lockFile.root node.parent};

          sourceInfo = (
            if key == lockFile.root then
              flakePath
            else if isRelative then
              parentNode.sourceInfo
            else if
              lockFile.nodes.${lockFile.root}.inputs ? "${key}" && key != rootKey && passedInputs ? "${key}"
            then
              passedInputs.${key}
            else
              fetchTree (node.info or { } // removeAttrs node.locked [ "dir" ])
          );

          subdir = if key == lockFile.root then "" else node.locked.dir or "";

          outPath =
            if isRelative then
              parentNode.outPath + (if node.locked.path == "" then "" else "/" + node.locked.path)
            else
              sourceInfo.outPath + (if subdir == "" then "" else "/" + subdir);

          flake = import (outPath + "/flake.nix");

          newInputs = lib.mapAttrs (_inputName: inputSpec: allNodes.${resolveInput inputSpec}.result) (
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

          inherit outPath sourceInfo;
        }
      ) lockFile.nodes;
    in
    allNodes;

  patchSource =
    {
      pkgs,
      src,
      patches ? [ ],
      name ? "patched-src",
      prePatch ? "",
      postPatch ? "",
    }:
    pkgs.applyPatches {
      inherit
        name
        src
        patches
        prePatch
        postPatch
        ;
    };

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
          ;
      };
      passedInputs = realInputs // inputs // extraInputs;

      backupLockFile = builtins.fromJSON (builtins.readFile "${patchedSrc}/flake.lock");

      backupNodes = nodesFn {
        rootKey = backupLockFile.root;
        lockFile = backupLockFile;
        flakePath = patchedSrc;
      };

      topLockFile = builtins.fromJSON (builtins.readFile "${rootPath}/flake.lock");

      allNodes = nodesFn {
        inherit passedInputs;
        lockFile = topLockFile;
        rootKey = name;
        flakePath = "${rootPath}";
      };

      flakeNode = allNodes.${name};

      curInputs = (
        builtins.intersectAttrs backupNodes.${backupLockFile.root}.result.inputs (
          passedInputs // backupNodes.${backupLockFile.root}.result.inputs // flakeNode.result.inputs
        )
        // {
          self = res // {
            inputs = curInputs;
          };
        }
      );
      res =
        flakeNode.result
        // ((import (patchedSrc + flakeNode.extraPathStuff + "/flake.nix")).outputs curInputs)
        // {
          outPath = "${patchedSrc}";
        };

    in
    res; # // { outPath = patchedSrc; };
  patchedDrvs =
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
        name = "${n}-patched";
      }
    ) config.patchedInputs;

  buildPatched =
    pkgs:
    let
      allInputs = lib.mapAttrs (
        n: p:
        patchFlake {
          inherit pkgs;
          extraInputs = allInputs;
          inherit (p)
            src
            patches
            prePatch
            postPatch
            ;
          name = "${n}";
        }
      ) (lib.filterAttrs (_: v: v.isInput) config.patchedInputs);
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
in
{
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
    type = lib.types.attrsOf (
      lib.types.submodule (
        { config, name, ... }: {
          options = {
            src = lib.mkOption {
              type = lib.types.raw;
              default = realInputs.${name}.outPath;
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

          };
        }
      )
    );
  };

  config = {
    flake-file.inputs.flake-compat = {
      url = "github:nixos/flake-compat";
      flake = false;
    };

    #    _module.args.patchFlake = patchFlake;
    #    flake.lib.patchFlake = lib.mkDefault patchFlake;

    # Per-key overlay instead of `realInputs // buildPatched pkgs`. With `//`,
    # the patched value of EVERY listed input shares one attrset, and the merge
    # gives no syntactic hint that un-patched inputs are untouched. Building the
    # overlay with `mapAttrs` + an explicit `if` keeps evaluation lazy per input:
    # reading an un-patched input (e.g. `inputs.nixpkgs`) returns `realInputs`
    # directly and never forces `pkgs`, `applyPatches`, or the flake-compat
    # re-eval — only the names actually in `patchedInputs` enter the patch path.
    # `buildPatched pkgs` stays a single shared thunk, so patched inputs that
    # reference each other still resolve through the one `allInputs` fixpoint.
    flake.age-plugin-gpg = inputs.age-plugin-gpg;
    # Get pkgs directly from realInputs to avoid forcing perSystem evaluation.
    # Using withSystem would trigger full perSystem module evaluation just to
    # compute newInputs, which adds significant eval overhead.
    flake.newInputs =
      let
        pkgs = realInputs.nixpkgs.legacyPackages.aarch64-darwin;
        patched = buildPatched pkgs;
      in
      lib.mapAttrs (
        name: realInput: if config.patchedInputs ? ${name} then patched.${name} else realInput
      ) realInputs;

    flake.inputs = inputs;

    perSystem =
      { pkgs, ... }:
      {
        # Patched inputs as a per-system module arg. Because flake-parts'
        options.patchedSources = lib.mkOption {
          type = lib.types.attrsOf lib.types.raw;
          default = { };
          description = ''
            Patched source trees for inputs listed in `patchedInputs`. Each entry
            is a flake-compat re-eval of the patched source tree, so it can be
            used as a flake input (e.g. `inputs.<name>`).
          '';
        };
        config = {
          patchedSources = patchedDrvs pkgs;
          # withSystem returns the full perSystem arg set, this is reachable both
          # in `perSystem = { patched, ... }: …` and
          # `withSystem system ({ patched, ... }: …)`.
          # Lazy + empty-safe (no IFD when `patchedInputs` is empty).

          apps.patch-input = {
            type = "app";
            program = lib.getExe (patchInputScript pkgs);
          };

          # Building these fails `nix flake check` if any declared patch is stale.
          checks = lib.mapAttrs' (n: _: {
            name = "patched-input-${n}";
            value = (patchedDrvs pkgs).${n};
          }) config.patchedInputs;
        };
      };
  };
}
