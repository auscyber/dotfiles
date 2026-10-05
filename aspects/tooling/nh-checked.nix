{ lib, ... }:
# `nix run .#nh-checked -- darwin switch` -- nh, but it runs this host's checks
# first and lands in the specialisation they select.
#
# An aspect declares what it needs with `requires` (../framework/requires.nix),
# a FAILING check means that aspect cannot work here, and a specialisation is
# already "this host minus these aspects". So this runs the checks, collects the
# aspects that failed, and picks the profile whose `excludes` COVER them.
#
# ── THE LINK IS THE EXCLUDE LIST, NOT A NAME ──
#
# Nothing says "no-celler is the celler profile". `celler-host` says it needs a
# celler; `no-celler` says it excludes `celler-host`; the wrapper matches the
# two. The smallest covering profile wins, so it drops least beyond what had to
# go. Writing the link out by hand would be the same fact in two places, able to
# disagree -- and the exclude list is the thing that actually removes the
# aspect, so it cannot be the one that is wrong.
#
# If nothing covers the failures it refuses and names them, rather than
# activating a profile that leaves a broken aspect in.
#
# ── IT ACTIVATES, IT DOES NOT BUILD A VARIANT ──
#
# Specialisations are built into the parent's `$out/specialisation/<name>` and
# selected at activation. nh takes `-s/--specialisation` on every subcommand
# that matters -- os/darwin switch and build, home switch -- and on darwin that
# flag is this repo's own patch (patches/nh/edit.patch), which reads
# /etc/specialisation.
#
# An earlier draft of this pointed `-H` at `<host>@<name>` instead, on the
# belief that it was a separate flake output. It is not:
# ../../lib/specialisations.nix parks that path inside a PRIVATE evalModules
# ("this evalModules is ours, not the repo's outputs, so nothing leaks"), and
# `nix eval .#darwinConfigurations` lists only the real hosts. That route
# resolved to nothing.
#
# Nor would it have helped if it had. The failure this exists for -- a dead
# substituter making a narinfo fetch fatal rather than a miss -- is a property
# of the machine DOING the build, not of the configuration being built:
# substituters come from nix.conf and the flake's own nixConfig, which is why
# ../../lib/without-celler.nix works by filtering NIX_CONFIG and why every
# agenix wrapper applies it unconditionally. Building a different attribute
# changes nothing about that. What a specialisation changes is the MACHINE: it
# comes up without the aspects whose peer is gone, instead of with units
# retrying against it.
#
# ── GOING BACK UP ──
#
# nh's patch does `self.specialisation.or(current_specialisation)`, so a plain
# `nh switch` STAYS in whatever specialisation is live. That is the right
# default for a human and the wrong one here: once the checks pass again you
# want to come back up. So when nothing is selected this passes `-S`, unless the
# caller named a specialisation themselves -- an explicit `-s`/`-S` always wins,
# and a conflict with a selected one is refused rather than silently resolved.
#
# `specialise base` (../tooling/specialise.nix) is the same move by hand, and
# `specialise --list` shows what the running system can reach.
#
# ── ONE PROFILE, NOT A COMBINATION ──
#
# This selects among specialisations that were DECLARED. Two aspects failing for
# unrelated reasons need a profile excluding both; exclude lists are not composed
# at build time, because that is the dynamic-variant design this deliberately
# does not have.
{
  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    let
      nhChecked = pkgs.writeShellApplication {
        name = "nh-checked";

        # ── NEITHER `nh` NOR `nix` IS BUNDLED, AND THAT IS LOAD-BEARING ──
        #
        # `writeShellApplication` PREPENDS runtimeInputs to PATH, so anything
        # listed here SHADOWS the tool the user actually runs.
        #
        # For `nh` that would shadow the patched build from ../base/nh.nix --
        # the very patch that provides `-s` on darwin.
        #
        # For `nix` it is worse, and was found by this script silently taking
        # the pass-through branch: this machine's nix is LIX, while `pkgs.nix`
        # is CppNix. Different implementations with different experimental
        # features -- ../../lib/default.nix uses `|>`, which Lix has and CppNix
        # gates behind `pipe-operators` -- so bundling it made every eval here
        # fail, and nh inherited the wrong nix from PATH too.
        #
        # So both are looked up and guarded, the same shape as the `xcrun --find
        # devicectl` guard in ../hosts/iphone.nix.
        #
        # curl/openssh/netcat ARE bundled, for the CHECK SCRIPTS rather than for
        # this one: a `requires` script is a string with nowhere to declare its
        # own dependencies, so the wrapper provides the handful a reachability
        # test needs and that set is part of the contract. Those scripts are the
        # one thing here that shellcheck never sees.
        runtimeInputs = [
          pkgs.coreutils
          pkgs.jq
          pkgs.bash
          pkgs.curl
          pkgs.openssh
          pkgs.netcat
        ];

        text = ''
          if [ ! -e flake.nix ]; then
            echo "nh-checked: run from the repo root (flake.nix not found)" >&2
            exit 1
          fi

          for tool in nh nix; do
            if ! command -v "$tool" >/dev/null 2>&1; then
              echo "nh-checked: $tool not found on PATH." >&2
              echo "  Neither is bundled on purpose -- bundling would shadow this" >&2
              echo "  machine's own (the patched nh, and Lix rather than CppNix)." >&2
              echo "  Enter the devshell." >&2
              exit 1
            fi
          done

          assume_yes=0
          user_chose_spec=0
          host=""
          args=()

          # `-H` is read, not rewritten: it says which host's checks to look up.
          # `-s`/`-S` are noted so an explicit choice is never overridden.
          while [ $# -gt 0 ]; do
            case "$1" in
              --yes)
                assume_yes=1
                ;;
              -H|--hostname)
                host="''${2:-}"
                args+=("$1")
                ;;
              -s|--specialisation|-S|--no-specialisation)
                user_chose_spec=1
                args+=("$1")
                ;;
              *)
                args+=("$1")
                ;;
            esac
            shift
          done

          if [ -z "$host" ]; then
            host="$(uname -n)"
            host="''${host%%.*}"
          fi

          # A host with no conditions declared is the normal case and means
          # "behave exactly like nh". An eval that FAILS is not that, and must
          # not be mistaken for it -- swallowing the error is what hid a
          # bundled-nix bug behind a silent pass-through.
          eval_err="$(mktemp)"
          if conditions="$(nix eval --json ".#buildConditions.\"$host\"" 2>"$eval_err")"; then
            rm -f "$eval_err"
          else
            if grep -q "does not provide attribute" "$eval_err"; then
              rm -f "$eval_err"
              conditions="{}"
            else
              echo "nh-checked: could not evaluate .#buildConditions.\"$host\"" >&2
              sed 's/^/  /' "$eval_err" >&2
              rm -f "$eval_err"
              exit 1
            fi
          fi

          # ── NOTHING TO CHECK, OR THE CALLER ALREADY CHOSE ──
          # `-S` so a pass comes back UP out of whatever is live; nh would
          # otherwise inherit the running specialisation and stay there.
          if [ "$conditions" = "{}" ] || [ -z "$conditions" ]; then
            if [ "$user_chose_spec" -eq 1 ]; then
              exec nh "''${args[@]}"
            fi
            exec nh "''${args[@]}" -S
          fi

          # ── RUN EVERY ASPECT'S CHECKS, COLLECT THE ASPECTS THAT FAILED ──
          # stderr is captured so the reason can be shown -- "Connection
          # refused" is the whole diagnostic, and swallowing it would make this
          # strictly worse than building and reading the error.
          failed=()
          reasons=""

          while IFS= read -r aspect; do
            while IFS= read -r check; do
              description="$(jq -r --arg a "$aspect" --arg c "$check" '.requires[$a][$c].description' <<<"$conditions")"
              script="$(jq -r --arg a "$aspect" --arg c "$check" '.requires[$a][$c].script' <<<"$conditions")"

              err="$(mktemp)"
              if bash -c "$script" >/dev/null 2>"$err"; then
                rm -f "$err"
                continue
              fi

              failed+=("$aspect")
              reasons+="  $aspect requires '$check' -- $description"$'\n'
              while IFS= read -r line; do
                [ -n "$line" ] && reasons+="      $line"$'\n'
              done <"$err"
              rm -f "$err"
              break
            done < <(jq -r --arg a "$aspect" '.requires[$a] | keys[]' <<<"$conditions")
          done < <(jq -r '.requires | keys[]' <<<"$conditions")

          # ── MATCH A PROFILE BY WHAT IT EXCLUDES ──
          # A specialisation covers this situation when its excludes are a
          # SUPERSET of the aspects that failed. Several may qualify, so the
          # smallest wins -- the one that drops least beyond what had to go.
          # Nothing is inferred from names: the link is the exclude list itself,
          # which is also the thing that actually takes the aspects out.
          selected=()
          if [ ''${#failed[@]} -gt 0 ]; then
            while IFS= read -r candidate; do
              selected+=("$candidate")
            done < <(
              jq -r --argjson f "$(printf '%s\n' "''${failed[@]}" | jq -R . | jq -s .)" '
                .profiles
                | to_entries
                | map(select(($f - .value) == []))
                | sort_by(.value | length)
                | .[0].key // empty
              ' <<<"$conditions"
            )
          fi

          if [ ''${#failed[@]} -gt 0 ] && [ ''${#selected[@]} -eq 0 ]; then
            echo "" >&2
            printf '%s' "$reasons" >&2
            echo "nh-checked: no specialisation excludes all of:" >&2
            printf '  %s\n' "''${failed[@]}" >&2
            echo "" >&2
            echo "  Declare one whose excludes cover them, or fix the requirement." >&2
            exit 1
          fi

          if [ ''${#selected[@]} -eq 0 ]; then
            if [ "$user_chose_spec" -eq 1 ]; then
              exec nh "''${args[@]}"
            fi
            echo "nh-checked: all checks passed for $host; going up to the base configuration" >&2
            exec nh "''${args[@]}" -S
          fi

          spec="''${selected[0]}"

          if [ "$user_chose_spec" -eq 1 ]; then
            echo "nh-checked: checks selected '$spec', but a specialisation was" >&2
            echo "  given on the command line. Refusing to override it." >&2
            printf '%s' "$reasons" >&2
            exit 1
          fi

          echo "" >&2
          printf '%s' "$reasons" >&2
          echo "nh-checked: selecting specialisation '$spec' for $host" >&2
          echo "  it excludes the aspects that check guards, and anything only" >&2
          echo "  they pulled in. \`specialise --list\` shows what is reachable;" >&2
          echo "  \`nh ... -S\` comes back up." >&2
          echo "" >&2

          if [ "$assume_yes" -eq 0 ]; then
            # The device node tests readable even with no controlling terminal,
            # so open it rather than `[ -t 0 ]` -- same probe as the one in
            # ../network/tailscale.nix.
            if ! { : </dev/tty; } 2>/dev/null; then
              echo "nh-checked: not interactive and --yes not given; refusing to" >&2
              echo "  silently activate a different configuration." >&2
              exit 1
            fi
            read -r -p "proceed? [y/N] " reply </dev/tty
            case "$reply" in
              [yY]*) ;;
              *)
                echo "nh-checked: aborted" >&2
                exit 1
                ;;
            esac
          fi

          exec nh "''${args[@]}" -s "$spec"
        '';
      };
    in
    {
      # Package as well as app: writeShellApplication runs shellcheck in its
      # builder, and an `apps.<x>.program` string cannot be built directly, so
      # app-only scripts are never linted. `nix build .#nh-checked` does.
      packages.nh-checked = nhChecked;
      devshells.default.packages = [ nhChecked ];
      apps.nh-checked = {
        type = "app";
        program = lib.getExe nhChecked;
      };
    };
}
