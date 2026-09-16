# `nix run .#write-flake` -- write partitions/<layer>/flake.nix, then the root.
#
# One eval, no subtraction. `flake.layerFlakeText` holds the rendered text per
# layer: ./layers.nix works out each input's placement from the `layers` tag on
# the aspect declaring it, so a layer's contents are known directly rather than
# recovered by diffing evaluations. Layer names come from that placement, so an
# intersection layer like `darwin-gui` appears the moment a host declares both
# tags -- its directory is created on demand, not enumerated anywhere.
#
# ORDER MATTERS, and it is why this is a wrapper rather than a write-hook.
# flake-file runs every `write-hooks` entry AFTER it has written flake.nix and
# locked it, so a hook cannot get the layers in place first. It has to: the root
# flake resolves `partitions/<layer>/flake.lock` to produce `inputs`, so moving
# an input into a layer that has not been written and locked yet leaves it
# declared nowhere and the flake stops evaluating -- which is not recoverable by
# regenerating, since regenerating needs it to evaluate. Layers first, then the
# root.
{
  lib,
  config,
  self,
  ...
}:
let
  topConfig = config;
in
{
  # The patch hashes, written by the SAME command as the layers and the root.
  #
  # A write-hook rather than a wrapper step, because unlike layer generation
  # this belongs after: hashing a patched tree needs the root lock that
  # `write-flake` has just written. Hooks run after flake.nix is written and
  # locked, which is precisely right here.
  #
  # Folded in because a separate `write-patched-inputs` drifts: `arion` had
  # patches and no recorded hash, so its patched tree was rebuilt locally on
  # every machine instead of substituted, and nothing said so.
  flake-file.write-hooks = [
    {
      index = 150;
      program = pkgs: config.allSystems.${pkgs.stdenv.hostPlatform.system}.packages.write-patched-inputs;
    }
  ];

  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    {
      # The rendered text as a STORE FILE, not a nested `nix eval`.
      #
      # Shelling out to `nix eval .#layerFlakeText` ran a second nix process with
      # its own fixpoint -- two LAYERS_MODULE_EVAL traces for one `write-flake`,
      # and the whole aspect tree walked twice. Built from the same evaluation
      # that produces flake.nix, it is walked once.
      packages.layer-flakes-json = pkgs.writeText "layer-flakes.json" (
        builtins.toJSON topConfig.flake.layerFlakeText
      );

      # Every host, with the attribute that evaluates it and the platform it is.
      # Built in the same evaluation so the checker never re-derives the fleet.
      packages.layer-isolation-hosts-json = pkgs.writeText "layer-isolation-hosts.json" (
        builtins.toJSON (
          lib.mapAttrsToList (name: _: {
            attr = "darwinConfigurations.${name}.system.drvPath";
            class = "darwin";
          }) (topConfig.flake.darwinConfigurations or { })
          ++ lib.mapAttrsToList (name: _: {
            attr = "nixosConfigurations.${name}.config.system.build.toplevel.drvPath";
            class = "nixos";
          }) (topConfig.flake.nixosConfigurations or { })
        )
      );

      packages.write-layer-flakes = pkgs.writeShellApplication {
        name = "write-layer-flakes";
        runtimeInputs = [
          pkgs.jq
          pkgs.coreutils
          # The same formatter flake-file runs over the root flake.nix. Without
          # it `nixCode` output is unindented, so every regeneration reads as a
          # whole-file rewrite and a real change is invisible in the diff.
          (topConfig.flake-file.formatter pkgs)
        ];
        text = ''
          repo="$(git rev-parse --show-toplevel)"
          cd "$repo"

          rendered=${config.packages.layer-flakes-json}

          count="$(jq -r 'length' "$rendered")"
          if [ "$count" -eq 0 ]; then
            echo "    no layers have inputs -- nothing to write" >&2
            exit 0
          fi

          changed=0
          written=()
          while IFS= read -r layer; do
            mkdir -p "partitions/$layer"
            target="partitions/$layer/flake.nix"
            jq -r --arg l "$layer" '.[$l]' "$rendered" > "$target.new"

            if [ -f "$target" ] && cmp -s "$target" "$target.new"; then
              rm -f "$target.new"
              echo "    $layer: unchanged" >&2
            else
              mv "$target.new" "$target"
              ${lib.getExe (topConfig.flake-file.formatter pkgs)} "$target"
              n="$(grep -cE '^      [a-zA-Z0-9_-]+ =' "$target" || true)"
              echo "    $layer: wrote $n input(s)" >&2
              written+=("$layer")
              changed=1
            fi
          done < <(jq -r 'keys[]' "$rendered")

          # Remove layer directories no aspect produces any more. A stale layer
          # is a lock nothing reads -- it cannot be checked against anything, so
          # it drifts silently, which is the failure this whole scheme exists to
          # prevent. Regenerating one costs a fetch; keeping a wrong one costs a
          # wrong build.
          #
          # `root` is never a directory here: those inputs live in the root
          # flake.nix, so it must not be treated as a missing layer.
          for d in partitions/*/; do
            [ -d "$d" ] || continue
            l="$(basename "$d")"
            [ "$l" = "root" ] && continue
            if ! jq -e --arg l "$l" 'has($l)' "$rendered" >/dev/null; then
              echo "    $l: no longer declared -- removing" >&2
              rm -rf "$d"
              changed=1
            fi
          done

          # Lock each layer we just wrote. A new sub-flake has no lock at all, so
          # this is the eager fetch -- unavoidable, and the reason it happens
          # here rather than lazily at build time.
          for layer in "''${written[@]}"; do
            echo "==> locking partitions/$layer" >&2
            # A BARE path, deliberately. Inside a git repo this resolves as
            # `git+file://<repo>?dir=partitions/<layer>`, so the store path is
            # the whole repo and the layer's `path:../../lib/stub` stays inside
            # it. Locking with a `path:` url copies only the layer directory and
            # nix then rejects the stub with "relative path points outside of
            # its parent's store path".
            nix flake lock "./partitions/$layer" \
              --no-accept-flake-config \
              --substituters "https://cache.nixos.org https://devenv.cachix.org https://nix-community.cachix.org"
          done

          if [ "$changed" -eq 1 ]; then
            echo "" >&2
            echo "Layer flakes and locks regenerated. Re-lock the root:  nix flake lock" >&2
          fi
        '';
      };

      # `nix run .#update` bumps the root lock; every layer lock has to move
      # with it or a shared pin drifts between them -- which is exactly how
      # partitions/nixos ended up six days behind the root on nixpkgs.
      update-hooks.postFlake.layer-locks = ''
        repo="$(git rev-parse --show-toplevel)"
        for d in "$repo"/partitions/*/; do
          [ -f "$d/flake.nix" ] || continue
          echo "Updating $(basename "$d") lock..."
          nix flake update --flake "$d"
        done
      '';
      # `nix run .#check-layer-isolation` -- assert a host never forces an input
      # from a layer belonging to another platform.
      #
      # Run on demand, NOT from `write-flake`: it is a full evaluation per host
      # on top of the one write-flake already does.
      #
      # The tag is an a priori claim about where an input belongs; this is the a
      # posteriori check that the claim held. It is the only thing that catches
      # declaration and USE diverging -- an input declared inside a nixos-only
      # provider but consumed by a fleet-wide collector reaches every host, and
      # nothing in the placement rules can see that. Both leaks found this way
      # (celler, nix-cachyos-kernel) were invisible to evaluation.
      #
      # A layer name is its tags joined by "-", so a layer is forbidden to a host
      # when it carries a PLATFORM tag the host is not. `gui-nixos` is forbidden
      # to darwin; plain `gui` or `dev` is not.
      packages.check-layer-isolation = pkgs.writeShellApplication {
        name = "check-layer-isolation";
        runtimeInputs = [
          pkgs.jq
          pkgs.coreutils
          pkgs.gnugrep
        ];
        text = ''
          # Flake to check: an explicit ref, else the working tree. The check
          # derivation passes a store path; a person running it by hand gets
          # their checkout.
          flakeref="''${1:-$(git rev-parse --show-toplevel)}"

          hosts=${config.packages.layer-isolation-hosts-json}
          rc=0


          while IFS=$'\t' read -r attr class; do
            [ -n "$attr" ] || continue

            # Deny every platform this host is NOT. The vocabulary lives in
            # ../framework/layers.nix; only the platform tags matter here, and
            # they are exactly the host classes.
            #
            # KNOWN LIMITATION -- nixos hosts are skipped.
            #
            # The gate denies an input for the WHOLE evaluation, and evaluating
            # `nixosConfigurations.<h>` was measured to force
            # `perSystem.aarch64-darwin.packages` (flake-parts' transposition
            # defines `flake.<attr>.<system>` for every system). That darwin
            # perSystem then legitimately applies darwin overlays and forces
            # darwin-layer inputs -- a false positive the gate cannot tell apart
            # from a real leak, because `lib/inputs.nix` sees only "this input
            # was forced", not who forced it.
            #
            # The darwin direction is sound and is NOT skipped: it caught a real
            # violation (`nixos-wsl`, forced from fleet resolution by attrset
            # class content) and passes now. The reverse is disabled rather than
            # left failing -- a check that cries wolf gets ignored, and this one
            # would fail on every run for a benign reason.
            #
            # SOLVED, and it is not an asymmetry in the config: every evaluation
            # of this flake builds exactly ONE perSystem `pkgs` -- the CURRENT
            # system's -- and applies every collected overlay to it. Measured by
            # instrumenting the pkgs construction: a nixos host eval and a darwin
            # host eval each build `aarch64-darwin` and nothing else, on this
            # Mac. So darwin-layer overlay inputs are ALWAYS forced and
            # nixos-layer ones NEVER are, whichever host you ask for.
            #
            # That makes the nixos direction structurally un-gateable here: it
            # would trip on the always-built current-system perSystem, not on
            # anything the host reaches. On a Linux machine the roles swap, and
            # the darwin direction would be the un-gateable one.
            if [ "$class" != "darwin" ]; then
              echo "==> $attr ($class) -- SKIPPED, see write-layer-flakes.nix" >&2
              continue
            fi

            deny=""
            for p in darwin nixos; do
              [ "$p" = "$class" ] || deny="$deny $p"
            done

            echo "==> $attr ($class, denying:$deny)" >&2
            log="$(mktemp)"

            # The GATE, not a trace. Every input owned by a denied layer throws
            # when forced, so an evaluation that COMPLETES proves it never
            # touched one. A trace could only report what happened to be forced
            # on that run, and stayed silent about a path not taken.
            if LAYER_GATE_DENY="$deny" nix eval --impure --raw "$flakeref#$attr" \
                 --no-accept-flake-config >/dev/null 2>"$log"; then
              echo "    ok -- no foreign-layer inputs reachable" >&2
            else
              echo "    FAILED" >&2
              # The WHOLE tail, not a grep for patterns I guessed in advance.
              # Filtering for `error:|is gated against` printed nothing at all
              # on the first real failure, which made the check useless exactly
              # when it mattered.
              tail -40 "$log" >&2
              rc=1
            fi
            rm -f "$log"
          done < <(jq -r '.[] | [.attr, .class] | @tsv' "$hosts")

          exit "$rc"
        '';
      };

      # `nix flake check` runs it.
      #
      # `__noChroot` because the check has to EVALUATE the fleet, and that needs
      # the nix daemon and the network -- a sandboxed builder has neither. The
      # invariant it tests is not visible to evaluation from the outside, so
      # there is no pure formulation: the only way to know what a host forces is
      # to force it. (`sandbox = relaxed` is what permits this.)
      checks.layer-isolation =
        pkgs.runCommand "layer-isolation"
          {
            __noChroot = true;
            nativeBuildInputs = [ config.packages.check-layer-isolation ];
          }
          ''
            check-layer-isolation ${self} >&2
            touch "$out"
          '';

      apps.write-layer-flakes = {
        type = "app";
        program = lib.getExe config.packages.write-layer-flakes;
      };

      # flake-file sets `packages.write-flake` from `flake-file.apps.write-flake`
      # (a pkgs -> package function). Replaced, not hooked: see the header. The
      # original is called through that same function, so this wraps it rather
      # than reimplementing it.
      packages.write-flake = lib.mkForce (
        pkgs.writeShellApplication {
          name = "write-flake";
          meta.description = "Regenerate the layer flakes and their locks, then flake.nix";
          text = ''
            ${lib.getExe config.packages.write-layer-flakes}
            exec ${lib.getExe (topConfig.flake-file.apps.write-flake pkgs)} "$@"
          '';
        }
      );
    };
}
