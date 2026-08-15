#!/usr/bin/env bash
# Record a CPU flamegraph for a Rust project, a running process, or any command.
#
#   flamegraph.sh                          # build and profile the cargo project here
#   flamegraph.sh -C ~/code/foo            # ...or the one over there
#   flamegraph.sh --bin server -o out.svg  # pick a binary, name the output
#   flamegraph.sh --attach 4213            # profile a process that is already running
#   flamegraph.sh --attach 4213 -d 20      # ...for twenty seconds, then stop by itself
#   flamegraph.sh --exec -- ./thing -v     # profile an arbitrary command
#
# Options that only matter to macOS GUI apps and daemons:
#   --sign [id]              codesign the built binary so its TCC grants survive rebuilds
#   --accessibility          settle the accessibility grant before recording, not during
#   --launchd <label>        stop that launch agent for the duration, restore on the way out
#   --env-from-launchd <l>   run with the environment that agent's wrapper sets
#
# Everything else:
#   -C, --path <dir>     project directory (default: the current one)
#   -b, --bin <name>     cargo target to build and profile
#   --profile <name>     cargo profile (default: one that carries debug symbols)
#   --features <list>    cargo features
#   -o, --output <file>  default <project>/flamegraph.svg
#   -d, --duration <s>   stop after this many seconds instead of waiting
#   --shell <how>        auto (default) | devenv | nix | none
#   --                   everything after this is passed to the profiled program
#
# Recording ends when the profiled program exits, when --duration runs out, or
# on Ctrl-C — and --attach has no exit of its own, so it needs one of the other
# two. macOS records through xctrace
# (needs Xcode, but no sudo and no SIP changes); Linux records through perf.
set -euo pipefail

ORIG_ARGS=("$@")

say()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33mnote:\033[0m %s\n' "$*" >&2; }
die()  { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }

PROJECT="$PWD"
BIN=""
CARGO_PROFILE=""
FEATURES=""
OUTPUT=""
SHELL_KIND="auto"
ATTACH_PID=""
DURATION=""
EXEC_MODE=0
SIGN=0
SIGN_ID=""
ACCESSIBILITY=0
LAUNCHD_LABEL=""
ENV_LAUNCHD_LABEL=""
PROGRAM_ARGS=()

while [ $# -gt 0 ]; do
    case "$1" in
        -C|--path)          PROJECT="$2"; shift 2 ;;
        -b|--bin)           BIN="$2"; shift 2 ;;
        --profile)          CARGO_PROFILE="$2"; shift 2 ;;
        --features)         FEATURES="$2"; shift 2 ;;
        -o|--output)        OUTPUT="$2"; shift 2 ;;
        --shell)            SHELL_KIND="$2"; shift 2 ;;
        --attach)           ATTACH_PID="$2"; shift 2 ;;
        -d|--duration)      DURATION="$2"; shift 2 ;;
        --exec)             EXEC_MODE=1; shift ;;
        # Optional argument, so it cannot use `$2` blindly: `--sign` on its own
        # means "find an identity", `--sign <id>` names one, and `--sign ""`
        # (or an empty PROFILE_CODESIGN_ID) deliberately keeps the ad-hoc one.
        --sign)             SIGN=1
                            if [ $# -ge 2 ] && [ "${2#-}" = "$2" ]; then SIGN_ID="$2"; shift; fi
                            shift ;;
        --accessibility)    ACCESSIBILITY=1; shift ;;
        --launchd)          LAUNCHD_LABEL="$2"; shift 2 ;;
        --env-from-launchd) ENV_LAUNCHD_LABEL="$2"; shift 2 ;;
        # The header comment, as far as it goes — no line range to keep in
        # step with edits above it.
        -h|--help)          awk 'NR > 1 { if ($0 !~ /^#/) exit; sub(/^# ?/, ""); print }' "$0"
                            exit 0 ;;
        --)                 shift; PROGRAM_ARGS=("$@"); break ;;
        *)                  die "unknown argument: $1" ;;
    esac
done

[ -d "$PROJECT" ] || die "no such directory: $PROJECT"
PROJECT="$(cd "$PROJECT" && pwd)"
OS="$(uname -s)"

# `--exec` profiles whatever follows `--`, so there is nothing to build; the
# same is true of `--attach`. Everything else needs a cargo project.
CARGO_MODE=1
if [ -n "$ATTACH_PID" ] || [ "$EXEC_MODE" -eq 1 ]; then
    CARGO_MODE=0
    [ -n "$ATTACH_PID" ] && [ "$EXEC_MODE" -eq 1 ] && die "--attach and --exec are alternatives"
fi
if [ "$CARGO_MODE" -eq 1 ] && [ ! -f "$PROJECT/Cargo.toml" ]; then
    die "no Cargo.toml in $PROJECT — use --exec or --attach to profile something else"
fi

# ------------------------------------------------------------------ dev shell
#
# A project's toolchain is usually not on the ambient PATH: this repo's own
# projects keep cargo, pkg-config and cargo-flamegraph inside a devenv or flake
# shell. Re-enter it once rather than making every caller remember to, and mark
# the re-entry so the child does not try again.
if [ -z "${FLAMEGRAPH_IN_SHELL:-}" ] && [ "$SHELL_KIND" != "none" ]; then
    kind="$SHELL_KIND"
    if [ "$kind" = "auto" ]; then
        # Already have a toolchain? Then this is very likely the project's own
        # shell already, and nesting another would only slow things down.
        if command -v cargo >/dev/null && command -v flamegraph >/dev/null; then
            kind="none"
        elif [ -f "$PROJECT/devenv.nix" ]; then
            kind="devenv"
        elif [ -f "$PROJECT/flake.nix" ]; then
            kind="nix"
        else
            kind="none"
        fi
    fi

    if [ "$kind" != "none" ]; then
        export FLAMEGRAPH_IN_SHELL=1
        # `--path` is re-stated first so it survives a relative -C, and the
        # original arguments still win if they named one.
        say "entering the $kind shell for $PROJECT"
        case "$kind" in
            devenv) command -v devenv >/dev/null || die "devenv not on PATH"
                    cd "$PROJECT"
                    exec devenv shell -- bash "$0" --path "$PROJECT" \
                        ${ORIG_ARGS[@]+"${ORIG_ARGS[@]}"} ;;
            nix)    command -v nix >/dev/null || die "nix not on PATH"
                    exec nix develop "$PROJECT" --command bash "$0" --path "$PROJECT" \
                        ${ORIG_ARGS[@]+"${ORIG_ARGS[@]}"} ;;
            *)      die "unknown --shell: $kind" ;;
        esac
    fi
fi

OUTPUT="${OUTPUT:-$PROJECT/flamegraph.svg}"

# --------------------------------------------------------------- the profiler
#
# On macOS this must be settled BEFORE anything else touches PATH: xcrun and
# xctrace have to be the system ones. A nix shell puts its own bin dirs first,
# and a nix xcrun cannot find Instruments' templates — which is what the
# "unable to run 'xctrace export'" failure actually means.
if [ "$OS" = "Darwin" ]; then
    export PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH"

    # xctrace is the recorder cargo-flamegraph drives here, and it lives inside
    # a full Xcode: the command line tools ship no Instruments. `/usr/bin/
    # xctrace` is only a shim that forwards to whichever developer directory is
    # active at the time, so rather than depend on that being right, find a
    # directory that genuinely contains the tool and name its binary outright.
    # That is what removes the whole "unable to run 'xctrace export'" family of
    # failures, which is the shim resolving somewhere without Instruments in it.
    #
    # `xcode-select -p` is consulted but never trusted: inside a nix shell it
    # can name an SDK in the store, and the CLT package is a legitimate answer
    # that also ships no Instruments. A candidate counts only if the tool is
    # actually in it — which is equally the test for an explicit DEVELOPER_DIR
    # inherited from the caller.
    unset SDKROOT   # a nix SDK here confuses the Instruments tooling

    XCTRACE=""
    xcode_dirs=()
    [ -n "${DEVELOPER_DIR:-}" ] && xcode_dirs+=("$DEVELOPER_DIR")
    selected="$(/usr/bin/xcode-select -p 2>/dev/null || true)"
    [ -n "$selected" ] && xcode_dirs+=("$selected")
    xcode_dirs+=(/Applications/Xcode.app/Contents/Developer \
                 /Applications/Xcode-beta.app/Contents/Developer)

    for candidate in ${xcode_dirs[@]+"${xcode_dirs[@]}"}; do
        [ -x "$candidate/usr/bin/xctrace" ] || continue
        # Present is not the same as working — a half-installed or
        # first-launch-pending Xcode has the binary and still cannot run.
        "$candidate/usr/bin/xctrace" version >/dev/null 2>&1 || continue
        export DEVELOPER_DIR="$candidate"
        XCTRACE="$candidate/usr/bin/xctrace"
        break
    done

    if [ -z "$XCTRACE" ]; then
        # Worth distinguishing: the tool being absent is an install problem,
        # the tool being present but unrunnable usually is not.
        if [ -x /usr/bin/xctrace ]; then
            die "found no working xctrace. /usr/bin/xctrace exists, but it is a shim and
  every developer directory it could forward to either lacks Instruments or
  fails to run. Try: sudo xcode-select -s /Applications/Xcode.app, then open
  Xcode once to let it finish installing components."
        fi
        die "xctrace not found. It ships with full Xcode, not the command line tools —
  install Xcode from the App Store, then run: sudo xcode-select -s /Applications/Xcode.app"
    fi

    # cargo-flamegraph spawns `xctrace` by name and honours $XCTRACE.
    export XCTRACE
    say "xctrace: $("$XCTRACE" version 2>&1 | head -n 1) at $XCTRACE"
else
    # perf needs either relaxed paranoia or root. Say so now rather than after
    # a build, and leave the choice alone — lowering it is a system change.
    paranoid="$(cat /proc/sys/kernel/perf_event_paranoid 2>/dev/null || echo unknown)"
    command -v perf >/dev/null || die "perf not found — install linux-perf / linux-tools"
    if [ "$paranoid" != "unknown" ] && [ "$paranoid" -gt 1 ] 2>/dev/null; then
        warn "perf_event_paranoid is $paranoid; recording will need --root (sudo).
  Lower it for this boot with: sudo sysctl kernel.perf_event_paranoid=1"
    fi
fi

if command -v flamegraph >/dev/null; then
    FLAMEGRAPH=(flamegraph)
elif command -v nix >/dev/null; then
    say "cargo-flamegraph not on PATH; running it from nixpkgs"
    FLAMEGRAPH=(nix shell nixpkgs#cargo-flamegraph -c flamegraph)
else
    die "cargo-flamegraph not found: cargo install flamegraph"
fi

# ------------------------------------------------------------- inherited env
#
# A daemon's launch agent usually wraps it in an environment it cannot start
# without — module search paths, helpers on PATH. Profiling it without that
# profiles a process whose config half failed to load, and the store paths
# involved change on every rebuild, so read them off the agent rather than
# pinning them anywhere.
if [ -n "$ENV_LAUNCHD_LABEL" ]; then
    [ "$OS" = "Darwin" ] || die "--env-from-launchd is macOS-only"
    plist="$HOME/Library/LaunchAgents/$ENV_LAUNCHD_LABEL.plist"
    [ -r "$plist" ] || die "no readable launch agent at $plist"

    # EnvironmentVariables is the plist's own; anything else lives in whatever
    # wrapper script ProgramArguments ends up exec'ing.
    while IFS= read -r line; do
        case "$line" in
            *" = "*)
                key="${line%% = *}"; value="${line#* = }"
                key="$(printf '%s' "$key" | tr -d '[:space:]')"
                [ -n "$key" ] && export "$key=$value" ;;
        esac
    done < <(/usr/libexec/PlistBuddy -c 'Print :EnvironmentVariables' "$plist" 2>/dev/null \
             | sed -n '/^Dict {/,/^}/p' | sed '1d;$d')

    args_count=0
    while /usr/libexec/PlistBuddy -c "Print :ProgramArguments:$args_count" "$plist" >/dev/null 2>&1; do
        args_count=$((args_count + 1))
    done
    wrapper=""
    if [ "$args_count" -gt 0 ]; then
        last="$(/usr/libexec/PlistBuddy -c "Print :ProgramArguments:$((args_count - 1))" "$plist" 2>/dev/null || true)"
        # Either the program itself, or a `sh -c '... && exec <wrapper>'` string.
        case "$last" in
            *exec\ *) wrapper="$(printf '%s' "$last" | sed -nE 's/.*exec[[:space:]]+([^[:space:]]+).*/\1/p')" ;;
            *)        wrapper="$(printf '%s' "$last" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//')" ;;
        esac
    fi

    if [ -n "$wrapper" ] && [ -r "$wrapper" ] && head -n 1 "$wrapper" | grep -qE '^#! */.*(ba)?sh'; then
        env_tmp="$(mktemp)"
        # Everything except a trailing `exec`, which would replace this script
        # with the daemon. `sed '$d'` and not `head -n -1`: BSD head has no
        # negative count, and /usr/bin is first on PATH by now.
        if tail -n 1 "$wrapper" | grep -qE '^exec[[:space:]]'; then
            sed '$d' "$wrapper" > "$env_tmp"
        else
            cp "$wrapper" "$env_tmp"
        fi
        # shellcheck source=/dev/null
        source "$env_tmp"
        rm -f "$env_tmp"
        say "took the environment from $ENV_LAUNCHD_LABEL's wrapper"
    else
        say "$ENV_LAUNCHD_LABEL execs a plain binary — only its plist environment applied"
    fi
fi

# ---------------------------------------------------------------------- build

BINARY=""
if [ "$CARGO_MODE" -eq 1 ]; then
    command -v cargo >/dev/null || die "cargo not found — is this the right --shell?"

    # A flamegraph without debug info is a wall of hex addresses, so the
    # profile has to keep symbols. Prefer one the project already defines for
    # this (any profile that inherits release and sets `debug`), because it
    # will also have been tuned for build time. Otherwise use release and turn
    # symbols on through --config, which needs no edit to Cargo.toml.
    CONFIG_ARGS=()
    if [ -z "$CARGO_PROFILE" ]; then
        CARGO_PROFILE="$(awk '
            /^\[profile\./ {
                name = $0; sub(/^\[profile\./, "", name); sub(/\].*$/, "", name)
                inherits = 0; dbg = 0; next
            }
            /^\[/ { name = "" }
            name != "" && /^[[:space:]]*inherits[[:space:]]*=[[:space:]]*"release"/ { inherits = 1 }
            name != "" && /^[[:space:]]*debug[[:space:]]*=/ && !/=[[:space:]]*(false|0|"none")/ { dbg = 1 }
            name != "" && inherits && dbg { print name; exit }
        ' "$PROJECT/Cargo.toml")"
        if [ -n "$CARGO_PROFILE" ]; then
            say "using the project's own '$CARGO_PROFILE' profile (release speed, symbols kept)"
        else
            CARGO_PROFILE="release"
            CONFIG_ARGS=(--config 'profile.release.debug=true' --config 'profile.release.strip=false')
            say "no symbol-carrying profile in Cargo.toml — using release with debug=true"
        fi
    fi

    cargo_args=(build --manifest-path "$PROJECT/Cargo.toml" --profile "$CARGO_PROFILE")
    [ -n "$BIN" ] && cargo_args+=(--bin "$BIN")
    [ -n "$FEATURES" ] && cargo_args+=(--features "$FEATURES")
    cargo_args+=(${CONFIG_ARGS[@]+"${CONFIG_ARGS[@]}"})

    say "building: cargo ${cargo_args[*]}"
    # json on stdout names the artifacts; render-diagnostics keeps warnings and
    # errors readable on stderr. This is how the binary is located without
    # guessing at a target directory or a package name.
    build_log="$(mktemp)"
    cargo "${cargo_args[@]}" --message-format=json-render-diagnostics > "$build_log"
    BINARY="$(grep -o '"executable":"[^"]\+"' "$build_log" | tail -1 | sed 's/^"executable":"//; s/"$//')"
    rm -f "$build_log"

    [ -n "$BINARY" ] || die "the build produced no executable — name one with --bin"
    [ -x "$BINARY" ] || die "expected a binary at $BINARY"
    say "binary: $BINARY"
fi

# -------------------------------------------------------------------- signing
#
# Cargo leaves a binary ad-hoc/linker-signed, and an ad-hoc signature's
# designated requirement degrades to a cdhash. TCC stores that requirement, so
# every rebuild changes the hash and macOS silently drops the grant — you
# re-grant on each build, and the recording stalls behind a dialog.
#
# A real identity gives an identity-based requirement instead, which carries no
# hash and so survives any rebuild of the same program at the same path.
if [ "$SIGN" -eq 1 ] && [ -n "$BINARY" ]; then
    [ "$OS" = "Darwin" ] || die "--sign is macOS-only"
    id="${SIGN_ID:-${FLAMEGRAPH_CODESIGN_ID-$(security find-identity -v -p codesigning 2>/dev/null \
        | awk '/Apple Development|Developer ID Application|Mac Developer/ {print $2; exit}')}}"

    if [ -n "$id" ]; then
        # Deliberately *not* --options runtime. The hardened runtime enforces
        # library validation, which refuses to map a nix-built dylib into a
        # process that now carries a Team ID ("mapping process and mapped file
        # (non-platform) have different Team IDs") — the binary then dies in
        # dyld before main. It also blocks the task_for_pid that xctrace needs.
        codesign --force --sign "$id" "$BINARY" 2>/dev/null \
            || die "codesign failed with identity $id"
        say "signed (the TCC grant for this path now survives rebuilds)"

        # Catch a binary that dies in dyld here rather than as a mysteriously
        # empty profile. --version is the cheapest thing that still maps every
        # dylib; a program without it just fails this check harmlessly.
        if probe="$("$BINARY" --version 2>&1)"; then
            say "runs: $(printf '%s' "$probe" | head -n 1)"
        else
            reason="$(printf '%s' "$probe" | grep -m1 -oE 'Library not loaded: [^ ]+|different Team IDs' || true)"
            if [ -n "$reason" ]; then
                die "the signed binary does not start — $reason
  Signing gave it a Team ID its dynamically linked libraries do not share.
  Re-run with FLAMEGRAPH_CODESIGN_ID= (empty) to keep the ad-hoc signature."
            fi
            warn "'$BINARY --version' failed; continuing (it may not accept --version)"
        fi
    else
        warn "no codesigning identity found — leaving the ad-hoc signature.
  macOS will drop this binary's TCC grants on every rebuild. Create a
  self-signed code-signing certificate in Keychain Access to fix that."
    fi
fi

# -------------------------------------------------------------- accessibility
#
# Apps that drive other apps need this, and granting it mid-recording turns the
# flamegraph into a picture of someone reading a dialog — or, for a program
# that blocks until it arrives, into one flat wait. TCC keys the grant on the
# binary's PATH, so a freshly built one needs its own; the installed copy's
# grant does not carry over.
#
# The grant lives in the *system* TCC database, not the per-user one, and
# reading it needs Full Disk Access for this terminal. Without that it is
# unobservable from here, so fall back to asking.
if [ "$ACCESSIBILITY" -eq 1 ] && [ -n "$BINARY" ]; then
    [ "$OS" = "Darwin" ] || die "--accessibility is macOS-only"
    AX_DB="/Library/Application Support/com.apple.TCC/TCC.db"
    granted=0
    if [ -r "$AX_DB" ] && command -v sqlite3 >/dev/null; then
        n="$(sqlite3 "file:$AX_DB?immutable=1" \
            "select count(*) from access
              where service='kTCCServiceAccessibility'
                and auth_value > 0
                and client = '$BINARY'" 2>/dev/null || echo 0)"
        [ "${n:-0}" -gt 0 ] && granted=1
    fi

    if [ "$granted" -eq 1 ]; then
        say "accessibility already granted for this binary"
    else
        printf '\n  This binary needs accessibility.\n'
        printf '  System Settings → Privacy & Security → Accessibility, then add:\n\n'
        printf '    %s\n\n' "$BINARY"
        open "x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility" 2>/dev/null || true
        open -R "$BINARY" 2>/dev/null || true
        if [ -t 0 ]; then
            read -r -p "  Press Enter once it is granted… " _
        else
            warn "not a terminal — continuing without waiting for the grant"
        fi
    fi
fi

# ------------------------------------------------------------------- launchd
#
# A daemon that holds a singleton resource — a Mach port, a socket, a lock —
# will refuse to start a second copy, so the installed one has to go first.
# Restoring it runs from an EXIT trap so that a Ctrl-C out of the recording, or
# any failure below, still gives the machine its daemon back.
SERVICE_STOPPED=0
restore_service() {
    [ "$SERVICE_STOPPED" -eq 1 ] || return 0
    SERVICE_STOPPED=0   # idempotent: the trap can fire more than once
    say "restarting $LAUNCHD_LABEL"
    /bin/launchctl kickstart "gui/$UID/$LAUNCHD_LABEL" >/dev/null 2>&1 \
        || /bin/launchctl bootstrap "gui/$UID" "$HOME/Library/LaunchAgents/$LAUNCHD_LABEL.plist" >/dev/null 2>&1 \
        || warn "could not restart $LAUNCHD_LABEL — start it yourself"
}

if [ -n "$LAUNCHD_LABEL" ]; then
    [ "$OS" = "Darwin" ] || die "--launchd is macOS-only"
    if /bin/launchctl print "gui/$UID/$LAUNCHD_LABEL" >/dev/null 2>&1; then
        say "stopping $LAUNCHD_LABEL for the duration"
        trap restore_service EXIT
        SERVICE_STOPPED=1
        # bootout, not kill: a KeepAlive job comes straight back and the
        # profiled copy could never take the resource.
        /bin/launchctl bootout "gui/$UID/$LAUNCHD_LABEL" >/dev/null 2>&1 || true
        # launchd tears a job down asynchronously; what it held is not free the
        # instant bootout returns.
        for _ in 1 2 3 4 5 6 7 8 9 10; do
            /bin/launchctl print "gui/$UID/$LAUNCHD_LABEL" >/dev/null 2>&1 || break
            sleep 0.5
        done
    else
        say "$LAUNCHD_LABEL is not loaded — nothing to stop"
    fi
fi

# ------------------------------------------------------------------ recording
#
# The recorder writes its raw trace as `cargo-flamegraph.trace` in the working
# directory, with no flag to place it elsewhere. A run that completes cleans it
# up; a run that was interrupted does not, and the next one then refuses to
# start at all ("Trace file already exists ... Specify append-run option"). One
# abandoned recording would otherwise poison every attempt after it.
#
# So: give the run a defined working directory, and move a leftover aside
# rather than deleting it — an interrupted recording is still a recording, and
# Instruments can open it.
if [ "$CARGO_MODE" -eq 1 ]; then cd "$PROJECT"; fi

TRACE="$PWD/cargo-flamegraph.trace"
if [ -e "$TRACE" ]; then
    say "an earlier recording left $TRACE behind; moving it to $TRACE.previous"
    rm -rf "$TRACE.previous"
    mv "$TRACE" "$TRACE.previous"
fi

record=("${FLAMEGRAPH[@]}" --output "$OUTPUT")

if [ -n "$ATTACH_PID" ]; then
    kill -0 "$ATTACH_PID" 2>/dev/null \
        || die "no process with pid $ATTACH_PID"
    record+=(--pid "$ATTACH_PID" --title "pid $ATTACH_PID ($(ps -o comm= -p "$ATTACH_PID" 2>/dev/null || echo "?"))")
    if [ -n "$DURATION" ]; then
        say "attached to pid $ATTACH_PID for ${DURATION}s"
    else
        # An attached recording has no end of its own, so an unattended run
        # here would sit forever. Worth saying rather than letting it hang.
        say "attached to pid $ATTACH_PID — Ctrl-C to finish (or use --duration)"
        [ -t 0 ] || warn "stdin is not a terminal: nothing can send Ctrl-C. Use --duration."
    fi
else
    if [ -n "$BINARY" ]; then
        target=("$BINARY")
        record+=(--title "$(basename "$BINARY") ($CARGO_PROFILE)")
    else
        [ ${#PROGRAM_ARGS[@]} -gt 0 ] || die "--exec needs a command after --"
        target=("${PROGRAM_ARGS[@]}")
        PROGRAM_ARGS=()
        record+=(--title "${target[0]##*/}")
    fi
    record+=(-- "${target[@]}" ${PROGRAM_ARGS[@]+"${PROGRAM_ARGS[@]}"})
    say "recording — exit the program (or Ctrl-C) to finish"
fi

# Ctrl-C reaches the whole foreground process group: the profiled program ends,
# and the profiler then folds the trace and writes the SVG. This shell must not
# die in the middle of that. A trap set to a *command* rather than the empty
# string keeps SIGINT from killing the shell while still leaving children the
# default disposition — `trap "" INT` would be inherited as SIG_IGN across exec
# and the program would never see the interrupt at all. Bash defers the handler
# until the foreground command returns, which is exactly the wait wanted here.
trap 'printf "\n"' INT

status=0
if [ -n "$DURATION" ]; then
    # Job control puts the recorder in its own process group, so the timeout
    # can signal the group — the same thing Ctrl-C does at a terminal, which
    # reaches the profiled program as well as the profiler. Signalling the
    # recorder alone would stop the recording but leave a spawned target
    # running.
    set -m
    "${record[@]}" &
    rec_pid=$!
    set +m
    ( sleep "$DURATION"; kill -INT -- -"$rec_pid" 2>/dev/null || kill -INT "$rec_pid" 2>/dev/null ) &
    timer_pid=$!
    wait "$rec_pid" || status=$?
    kill "$timer_pid" 2>/dev/null || true
    wait "$timer_pid" 2>/dev/null || true
else
    "${record[@]}" || status=$?
fi

trap - INT

# A recording ended with Ctrl-C exits nonzero and still wrote a perfectly good
# SVG, so the file is what to test, not the status.
[ -f "$OUTPUT" ] || die "no flamegraph written (the profiler exited $status)"

# A clean run removes its own trace, so this normally finds nothing. When it
# does find one, keeping it under a name tied to this run is better than
# leaving the fixed name to block the next recording.
if [ -e "$TRACE" ]; then
    kept="${OUTPUT%.svg}.trace"
    rm -rf "$kept"
    mv "$TRACE" "$kept"
    say "kept the raw trace at $kept (open it with Instruments)"
fi

say "wrote $OUTPUT"
if [ "$OS" = "Darwin" ] && command -v open >/dev/null; then open "$OUTPUT" || true; fi
