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

            Defaults to the entry for this input in the generated
            `./patched-inputs.nix`. Refresh with
            `nix run .#write-patched-inputs` after changing a patch or bumping
            the upstream input — that one command rebuilds each tree, hashes
            it and rewrites the file. A stale hash fails the build with a hash
            mismatch.

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

  # The patch implementation lives in ../../lib/patched-inputs.nix as a plain
  # function so flake.nix can call it WITHOUT running the module system (see
  # that file's header). This module and flake.nix therefore share one
  # implementation; the only thing that differs is where the specs come from —
  # here, the live `config.patchedInputs`; there, the generated
  # ../../patched-inputs.nix.
  impl = import ../../lib/patched-inputs.nix {
    inherit lib rootPath;
    inputs = realInputs;
    inherit (config) noUnify patchSystem;
    patchSpecs = lib.mapAttrs (_: v: {
      inherit (v)
        patches
        prePatch
        postPatch
        isInput
        src
        hash
        ;
    }) config.patchedInputs;
  };
  inherit (impl)
    toBuild
    patchedDrvs
    patchedDrvsUnhashed
    hasPatches
    ;

  # Manifest of patched inputs to republish as GitHub forks, shared by the
  # `patched-forks-manifest` / `push-patched-forks` apps below and the CI
  # workflow (which evaluates lib/patched-forks-manifest.nix directly). Pulls
  # the upstream github owner/repo/rev from ./flake.lock crossed with the
  # committed ./patched-inputs.nix — see that file's header.
  pushForksManifestJSON = builtins.toJSON (
    import ../../lib/patched-forks-manifest.nix {
      inherit (config.pushPatches) owner branch;
      root = rootPath;
    }
  );

  # The `<name>.patches = [...]` half of ./patched-inputs.nix. Paths are
  # emitted relative to the flake root so the generated file stays
  # source-relative (./patches/...) rather than baking in /nix/store paths.
  # The `<name>.hash` half is NOT produced here: it is computed by
  # `write-patched-inputs` at run time from the freshly built (unhashed) trees,
  # so generating the file and recording the hashes are one operation.
  generatedPatchLines =
    let
      relPath =
        p:
        let
          s = toString p;
          root = toString rootPath;
        in
        if lib.hasPrefix root s then "." + lib.removePrefix root s else s;
      entry =
        name: v:
        let
          ps = map relPath v.patches;
        in
        if ps == [ ] then
          "  ${name}.patches = [ ];"
        else
          "  ${name}.patches = [\n${lib.concatMapStringsSep "\n" (x: "    ${x}") ps}\n  ];";
    in
    lib.concatStringsSep "\n" (
      lib.mapAttrsToList entry (lib.filterAttrs (_: v: v.isInput) config.patchedInputs)
    );

  # Header of the generated file, kept next to the generator that emits it.
  header = ''
    # GENERATED by `nix run .#write-patched-inputs` — do not edit by hand.
    #
    # The patch configuration flake.nix needs BEFORE it can evaluate the
    # flake-parts module system: which inputs are patched, with which patches,
    # and the SRI hash of each resulting tree. Because the hash lives here,
    # flake.nix needs no second file to build the patched inputs.
    #
    # Everything else is still derived purely at eval time: `src` from the
    # flake input, and the auto-unified set (inputs that transitively depend
    # on a patched one) from ./flake.lock.
    #
    # `hash = null` means no recorded hash — that tree is built locally and
    # is not substitutable. `PATCH_HASHES=ignore` overrides every hash here,
    # which is the way out if one goes stale: a stale hash fails the
    # fixed-output build, which would otherwise take down the very evaluation
    # that runs the app to regenerate it.
    #
    # Regenerate with `nix run .#write-patched-inputs` after changing a patch
    # or bumping a patched input. `checks.patched-inputs-generated-current`
    # fails if the patch lists here drift from the aspects' declarations.
    {
  '';

  # Inputs whose tree is actually rewritten, paired with the UNHASHED build of
  # that tree — `write-patched-inputs` hashes these. Interpolating the
  # derivations makes them dependencies of the script, so `nix run` builds them
  # first. Deliberately the unhashed variant: hashing the hashed one would just
  # echo back whatever is already recorded, so a stale value could never be
  # corrected.
  hashEntries = pkgs: lib.mapAttrsToList (n: drv: "${n} ${drv}") (patchedDrvsUnhashed pkgs);

  # Inputs with no patches at all: nothing is built, so they get `hash = null`.
  nullHashNames = lib.attrNames (
    lib.filterAttrs (_: v: v.isInput && !(hasPatches v)) config.patchedInputs
  );

  # Recorded SRI hashes, feeding the `hash` option default. These now live in
  # the generated ./patched-inputs.nix rather than a separate hashes.json, so
  # `write-patched-inputs` records patches and hashes in one place.
  #
  # `getEnv` returns "" under pure eval, so the escape hatch is inert
  # everywhere else.
  patchHashes =
    let
      file = ../../patched-inputs.nix;
      ignore = builtins.getEnv "PATCH_HASHES" == "ignore";
    in
    if ignore || !builtins.pathExists file then
      { }
    else
      lib.mapAttrs (_: v: v.hash or null) (import file);

  patchInputScript =
    pkgs:
    pkgs.writeShellApplication {
      name = "patch-input";
      runtimeInputs = [
        pkgs.git
        pkgs.coreutils
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

  options.pushPatches = {
    owner = lib.mkOption {
      type = lib.types.str;
      default = "auscyber";
      description = ''
        GitHub account that owns the destination forks published by
        `nix run .#push-patched-forks` (and the push-patched-forks workflow).
      '';
    };
    branch = lib.mkOption {
      type = lib.types.str;
      default = "dendritic-patched";
      description = ''
        Branch force-pushed to each fork: `github:<owner>/<repo>/<branch>` is
        what a downstream flake then consumes as a patched input.
      '';
    };
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
    # Kept for anything reading `self.newInputs`, but flake.nix no longer uses
    # it: it calls ../../lib/patched-inputs.nix directly, which is what lets it
    # build the flake in ONE module-system pass instead of two. Lazy, so it
    # costs nothing unless something actually reads it.
    flake.newInputs = impl.newInputs;

    perSystem =
      args@{ pkgs, ... }:
      {
        apps.patch-input = {
          type = "app";
          program = lib.getExe (patchInputScript pkgs);
        };

        # Print the fork manifest (which patched input goes to which
        # github:<owner>/<repo>/<branch>). What CI feeds to the push script.
        apps.patched-forks-manifest = {
          type = "app";
          program = lib.getExe (
            pkgs.writeShellApplication {
              name = "patched-forks-manifest";
              runtimeInputs = [ pkgs.jq ];
              text = "jq .${pkgs.writeText "patched-forks-manifest.json" pushForksManifestJSON}";
            }
          );
        };

        # Republish every patched input as a fork under `pushPatches.owner`:
        # upstream-as-locked + its patches, force-pushed to the branch. Needs
        # `GH_TOKEN` with push access (or `--dry-run` to fetch+apply without
        # pushing). Same script the push-patched-forks workflow runs.
        apps.push-patched-forks = {
          type = "app";
          program = lib.getExe (
            pkgs.writeShellApplication {
              name = "push-patched-forks";
              runtimeInputs = [
                pkgs.git
                pkgs.jq
                pkgs.gh
                pkgs.gnupatch
                pkgs.coreutils
              ];
              text = ''
                exec bash ${../../scripts/push-patched-forks.sh} \
                  --manifest ${pkgs.writeText "patched-forks-manifest.json" pushForksManifestJSON} "$@"
              '';
            }
          );
        };

        # Regenerates ../../patched-inputs.nix from the live aspect declarations.
        # Run after adding/removing a patch or toggling `patch.enable`.
        # The ONE generator: builds each patched tree, hashes it, and writes
        # ./patched-inputs.nix with both the patch lists and the resulting
        # hashes. Previously this was two commands (`update-patch-hashes` wrote
        # patches/hashes.json, then `write-patched-inputs` copied the hashes
        # into the generated file), which could leave the two disagreeing.
        apps.write-patched-inputs = {
          type = "app";
          program = lib.getExe (
            pkgs.writeShellApplication {
              name = "write-patched-inputs";
              runtimeInputs = [ pkgs.coreutils ];
              text = ''
                set -euo pipefail
                repo="$PWD"
                if [ ! -e "$repo/flake.nix" ]; then
                	echo "error: run from the flake root (no flake.nix in $repo)" >&2
                	exit 2
                fi

                out="$repo/patched-inputs.nix"
                tmp="$(mktemp)"
                trap 'rm -f "$tmp"' EXIT

                cat >"$tmp" <<'HEADER'
                ${header}
                HEADER

                cat >>"$tmp" <<'PATCHES'
                ${generatedPatchLines}
                PATCHES
                printf '\n' >>"$tmp"

                while read -r name path; do
                	[ -n "$name" ] || continue
                	h="$(nix hash path --sri "$path")"
                	echo "  $name.hash = \"$h\";" >>"$tmp"
                	echo "  $name  $h"
                done <<-'ENTRIES'
                	${lib.concatStringsSep "\n\t" (hashEntries pkgs)}
                ENTRIES

                while read -r n; do
                	[ -n "$n" ] || continue
                	echo "  $n.hash = null;" >>"$tmp"
                	echo "  $n  (no patches — no hash)"
                done <<-'NULLHASHES'
                	${lib.concatStringsSep "\n\t" nullHashNames}
                NULLHASHES

                echo "}" >>"$tmp"
                mv "$tmp" "$out"
                trap - EXIT
                echo "wrote $out"
              '';
            }
          );
        };

        # `nix run .#update` — bump the inputs, then regenerate
        # ./patched-inputs.nix, which the bump just invalidated: a patched input
        # moving to a new rev changes the patched tree, and therefore its hash.
        #
        # `PATCH_HASHES=ignore` is what makes the second step possible at all.
        # Between the two commands the recorded hashes describe the OLD revs, so
        # a normal evaluation would fail the fixed-output build and take down the
        # very app that rewrites them. Ignoring them falls back to unhashed
        # builds, which evaluate fine and are what the generator hashes anyway.
        # `--impure` is required for `builtins.getEnv` to see the variable.

        update-hooks.flake = ''
          echo "Updating patched-inputs.nix…"
          PATCH_HASHES=ignore ${args.config.apps.write-patched-inputs.program}
        '';

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
            # The generated ../../patched-inputs.nix is what flake.nix reads to
            # build the patched inputs without running the module system. If it
            # drifts from what the aspects actually declare, flake.nix would
            # apply the wrong patch set — silently, since nothing else compares
            # them. Fail here instead. Refresh with
            # `nix run .#write-patched-inputs`.
            patched-inputs-generated-current =
              let
                generated = lib.mapAttrs (_: v: {
                  patches = map toString (v.patches or [ ]);
                  hash = v.hash or null;
                }) (import ../../patched-inputs.nix);
                live = lib.mapAttrs (_: v: {
                  patches = map toString v.patches;
                  inherit (v) hash;
                }) (lib.filterAttrs (_: v: v.isInput) config.patchedInputs);
                missing = lib.attrNames (removeAttrs live (lib.attrNames generated));
                extra = lib.attrNames (removeAttrs generated (lib.attrNames live));
                changed = lib.filter (n: (generated.${n} or null) != live.${n}) (
                  lib.attrNames (builtins.intersectAttrs generated live)
                );
              in
              assert lib.assertMsg (missing == [ ] && extra == [ ] && changed == [ ]) ''
                ./patched-inputs.nix is out of date — run `nix run .#write-patched-inputs`.
                  missing from the generated file: ${lib.concatStringsSep ", " missing}
                  no longer declared by aspects:   ${lib.concatStringsSep ", " extra}
                  different patches or hash:       ${lib.concatStringsSep ", " changed}
              '';
              pkgs.emptyFile;

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
