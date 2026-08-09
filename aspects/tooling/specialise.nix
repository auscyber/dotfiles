# `specialise` -- pick a specialisation and activate it, on whatever this is.
#
# Specialisations are built (aspects/framework/specialisations.nix) but activating
# one is platform trivia: NixOS wants
# `switch-to-configuration switch`, nix-darwin wants its own
# `sw/bin/darwin-rebuild activate`, home-manager wants a plain `activate`, and all
# three keep their profiles somewhere different. This wraps the lot in one
# command that works the same on the laptop and on a Linux box, and reads the
# LIVE generation rather than anything baked in -- so it lists exactly what the
# running system can switch to, including after a rollback.
#
# Usage:
#   specialise                 pick interactively (fzf in a terminal, dmenu/rofi/
#                              wofi/choose under a graphical session)
#   specialise study           activate by name
#   specialise base            back to the unspecialised configuration
#   specialise --list          print `scope<TAB>name<TAB>store path`
#   specialise --dry-run …     print the activation command instead of running it
#
# `--home`/`--system` disambiguate a name that exists in both scopes;
# `SPECIALISE_PICKER` overrides picker choice entirely.
{ den, ... }: {
  den.aspects.specialise = {
    os = { pkgs, ... }: {
      environment.systemPackages = [ pkgs.specialise ];
    };

    overlays.specialise = final: _prev: {
      specialise = final.writeShellApplication {
        name = "specialise";
        # fzf is the only picker that is always there: the graphical ones are
        # probed at runtime because whether any exists is a property of the
        # session, not of this closure.
        runtimeInputs = [
          final.fzf
          final.coreutils
        ];
        text = ''
          system_profile=''${SPECIALISE_SYSTEM:-/run/current-system}
          home_profile=''${SPECIALISE_HOME:-''${XDG_STATE_HOME:-$HOME/.local/state}/home-manager/gcroots/current-home}

          usage() {
            cat >&2 <<'EOF'
          usage: specialise [--list] [--dry-run] [--system|--home] [<name>]

            <no args>   pick a specialisation interactively
            <name>      activate that specialisation ("base" = the plain configuration)
            --list      print scope, name and store path, tab separated
            --dry-run   print the activation command instead of running it
            --system    only consider system (NixOS / nix-darwin) specialisations
            --home      only consider home-manager specialisations
          EOF
          }

          # scope<TAB>name<TAB>path, "base" first so the picker's default is the
          # way back out.
          entries() {
            local scope root p
            for scope in system home; do
              case "$scope" in
                system) root=$system_profile ;;
                home) root=$home_profile ;;
              esac
              [ -n "''${want_scope:-}" ] && [ "$want_scope" != "$scope" ] && continue
              [ -e "$root" ] || continue
              printf '%s\tbase\t%s\n' "$scope" "$root"
              [ -d "$root/specialisation" ] || continue
              for p in "$root"/specialisation/*; do
                [ -e "$p" ] || continue
                printf '%s\t%s\t%s\n' "$scope" "$(basename "$p")" "$p"
              done
            done
          }

          pick() {
            if [ -n "''${SPECIALISE_PICKER:-}" ]; then
              eval "$SPECIALISE_PICKER"
            elif [ -t 0 ] && [ -t 1 ]; then
              fzf --prompt='specialisation> ' --height=40% --reverse --with-nth=1,2
            elif command -v dmenu >/dev/null 2>&1; then
              dmenu -p specialisation
            elif command -v rofi >/dev/null 2>&1; then
              rofi -dmenu -p specialisation
            elif command -v wofi >/dev/null 2>&1; then
              wofi --dmenu -p specialisation
            elif command -v choose >/dev/null 2>&1; then
              choose
            else
              echo "specialise: no picker available (set SPECIALISE_PICKER, or install fzf/dmenu/rofi/wofi/choose)" >&2
              return 1
            fi
          }

          # NixOS and nix-darwin both leave their own switcher inside the
          # generation, so the target decides how it is activated -- no need to
          # ask what platform this is.
          activate() {
            local scope=$1 path=$2 cmd
            if [ "$scope" = home ]; then
              cmd=("$path/activate")
            elif [ -x "$path/sw/bin/darwin-rebuild" ]; then
              cmd=(sudo "$path/sw/bin/darwin-rebuild" activate)
            elif [ -x "$path/bin/switch-to-configuration" ]; then
              cmd=(sudo "$path/bin/switch-to-configuration" "''${SPECIALISE_ACTION:-switch}")
            elif [ -x "$path/activate" ]; then
              cmd=(sudo "$path/activate")
            else
              echo "specialise: $path has no activation script" >&2
              return 1
            fi
            if [ -n "''${dry_run:-}" ]; then
              printf '%s\n' "''${cmd[*]}"
            else
              printf 'specialise: activating %s %s\n' "$scope" "$path" >&2
              "''${cmd[@]}"
            fi
          }

          want_scope=""
          dry_run=""
          name=""
          while [ $# -gt 0 ]; do
            case "$1" in
              --list) list=1 ;;
              --dry-run) dry_run=1 ;;
              --system) want_scope=system ;;
              --home) want_scope=home ;;
              -h|--help) usage; exit 0 ;;
              -*) usage; exit 2 ;;
              *) name=$1 ;;
            esac
            shift
          done

          if [ -n "''${list:-}" ]; then
            entries
            exit 0
          fi

          if [ -z "$name" ]; then
            # `|| true`, then test for emptiness: a picker that stops reading as
            # soon as it has an answer makes `entries` take SIGPIPE, which under
            # `pipefail` would fail the pipeline even though the selection came
            # through. Cancelling (fzf ESC, dmenu C-c) yields nothing, which is
            # the case that should exit.
            selection=$(entries | pick) || true
            [ -n "$selection" ] || exit 1
          else
            # First match wins, so --system/--home is how a name present in both
            # scopes is disambiguated. awk keeps reading after the match rather
            # than `exit`ing: exiting closes the pipe, `entries` takes SIGPIPE,
            # and under `pipefail` + `set -e` the whole script dies right after
            # the assignment -- silently, for every name but the last one.
            selection=$(entries | awk -F'\t' -v n="$name" '$2 == n && !found { print; found = 1 }')
            if [ -z "$selection" ]; then
              echo "specialise: no specialisation named '$name'" >&2
              entries >&2
              exit 1
            fi
          fi

          scope=$(printf '%s' "$selection" | cut -f1)
          path=$(printf '%s' "$selection" | cut -f3)
          activate "$scope" "$path"
        '';
      };
    };
  };

  # Every host gets the command: it is a shell script, and a host without
  # specialisations still answers `specialise --list` with just `base` rather
  # than "command not found".
  den.schema.host.includes = [ den.aspects.specialise ];
}
