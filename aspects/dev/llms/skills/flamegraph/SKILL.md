---
name: flamegraph
description: >
  Record and read a CPU flamegraph — for a cargo project, a process that is
  already running, or any command — using cargo-flamegraph over xctrace on
  macOS and perf on Linux. Handles the parts that usually go wrong: keeping
  debug symbols, pinning the system Xcode tooling out of a nix shell,
  codesigning so TCC grants survive a rebuild, and stopping a launchd daemon
  that will not tolerate a second copy. Use when asked to profile something,
  find where time is going, or investigate a stutter, lag, high CPU or a slow
  frame. Triggers on "flamegraph", "profile this", "why is it slow", "hot
  path", "CPU usage", "perf".
---

# Flamegraphs

One script does the work:

```
bash ~/.claude/skills/flamegraph/scripts/flamegraph.sh [options]
```

`-h` prints its own help. It records for as long as the profiled program runs,
so **run it in the foreground of an interactive terminal** — it may need to
wait on a permission dialog, and it always waits on the program exiting. If you
cannot give it a terminal, print the command and let the user run it.

## The three modes

| | |
|---|---|
| `flamegraph.sh -C <dir>` | build the cargo project and profile it (default; `-C` defaults to the cwd) |
| `flamegraph.sh --attach <pid>` | profile a process that is already running |
| `flamegraph.sh --exec -- <cmd> …` | profile any command |

Recording ends when the profiled program exits, on Ctrl-C, or after
`-d/--duration <seconds>`. **`--attach` has no end of its own**, so give it one
of the other two — an attached run with no terminal to interrupt it will sit
there until something kills it.

Useful with the first mode: `--bin <name>` when a project builds several,
`--features`, `--profile <name>` to override the auto-picked cargo profile, and
`-o <file>` (default `<project>/flamegraph.svg`).

## What it settles before recording

These are the parts that fail, and why the flags exist:

1. **A toolchain.** Projects here keep cargo, `cargo-flamegraph` and their
   native dependencies inside a dev shell. The script detects `devenv.nix` or
   `flake.nix` and re-enters it once; `--shell devenv|nix|none` overrides.
2. **Debug symbols.** Without them every frame is a hex address. It prefers a
   profile the project already defines for this — any `[profile.*]` that
   inherits `release` and sets `debug` — and otherwise builds `release` with
   `--config profile.release.debug=true`, editing nothing.
3. **A working xctrace** (macOS). `/usr/bin/xctrace` is only a shim that
   forwards to whichever developer directory is active, so the script does not
   rely on it: it walks `DEVELOPER_DIR`, `xcode-select -p` and the standard
   Xcode locations, takes the first that *contains* xctrace and answers
   `xctrace version`, and exports that path outright. `xcode-select -p` is a
   candidate but never trusted — inside a nix shell it can name an SDK in the
   store, and the command line tools are a legitimate answer that ships no
   Instruments. `/usr/bin` also goes first on PATH and `SDKROOT` is unset, so a
   nix `xcrun` cannot win. Together that is the whole `unable to run 'xctrace
   export'` family. Needs full Xcode, but no sudo and no SIP changes.
4. **`--sign [id]`** (macOS). Cargo's ad-hoc signature makes the binary's TCC
   requirement a cdhash, so every rebuild silently drops its permissions and
   you re-grant forever. A certificate-based requirement carries no hash and
   survives. An identity is found automatically, or set
   `FLAMEGRAPH_CODESIGN_ID` (empty to keep ad-hoc). **Never add `--options
   runtime`**: library validation then refuses to map a nix-built dylib into a
   process carrying a Team ID, and it blocks the `task_for_pid` xctrace needs.
5. **`--accessibility`** (macOS). For apps that drive other apps. TCC keys the
   grant on the binary's *path*, so a freshly built one needs its own — the
   installed copy's grant does not carry over. Granting mid-recording ruins the
   profile, or, for a program that blocks until the grant arrives, produces one
   flat wait. Verifying it reads the *system* TCC database, which needs Full
   Disk Access for the terminal; without that the script asks instead.
6. **`--launchd <label>`** (macOS). A daemon holding a singleton — a Mach port,
   a socket, a lock — refuses to start a second copy, so the installed one is
   booted out for the duration and restored from an `EXIT` trap, including on
   Ctrl-C. `--env-from-launchd <label>` additionally runs with the environment
   that agent's wrapper sets, which is how a daemon whose config depends on
   generated module search paths gets profiled as it actually runs.

It also gives the run a defined working directory and rescues a
`cargo-flamegraph.trace` left there by an interrupted recording (moving it to
`.previous`). That matters because the recorder has no flag to place its raw
trace elsewhere and refuses to start when one already exists — so a single
abandoned run would otherwise block every attempt after it.

On Linux it checks for `perf` and warns when `perf_event_paranoid` is high
enough that recording will need `--root`; it never changes the setting.

## Worked examples

```sh
# a plain cargo project
flamegraph.sh -C ~/code/thing --bin thing

# rustcast: a GUI app needing accessibility, whose grant must outlive rebuilds
flamegraph.sh -C ~/code/rustcast --sign --accessibility

# paneru: a launchd window manager — stop the installed one, inherit its env
flamegraph.sh -C ~/code/paneru --sign --accessibility \
    --launchd com.github.karinushka.paneru \
    --env-from-launchd com.github.karinushka.paneru

# something already misbehaving, sampled for twenty seconds
flamegraph.sh --attach "$(pgrep -n thing)" -d 20
```

**rustcast has its own `scripts/profile.sh`**, which additionally sandboxes a
chosen set of example plugins under an isolated `HOME`. Prefer it whenever the
question involves plugins; use the generic script for a plain baseline.

Neither of those two apps can be profiled unattended — both need a person to
drive them while recording. Say so up front, and say what to exercise: for a
launcher, typing so providers stream and holding a key down so per-frame work
shows; for a window manager, focusing across displays, moving and resizing,
and switching workspaces.

## Reading the result

Width is time **on CPU**, not wall clock — anything blocked, sleeping or
waiting is simply absent, which matters most for exactly the programs that feel
slow. Height is stack depth and means nothing on its own.

- Look at **plateaus**, not peaks: a wide frame with little above it is the
  code actually burning CPU.
- Ignore the thin towers at either edge — startup and teardown.
- To compare before/after, record both with the **same interactions** and
  `-o before.svg` / `-o after.svg`; otherwise the two are not comparable.
- The SVG is interactive: click a frame to zoom, `Ctrl-F` to search (matches
  tint magenta and the matched percentage is shown). Re-render with `--reverse`
  to group by leaf instead of by root when a cost is spread over many callers.

## Failure modes

| Symptom | Cause |
|---|---|
| `unable to run 'xctrace export'` | a nix `xcrun` won the PATH race, or `SDKROOT`/`DEVELOPER_DIR` names a nix SDK |
| `/usr/bin/xctrace is missing` | only the command line tools are installed; xctrace ships with full Xcode |
| every frame is a hex address | built without debug info — pass `--profile` explicitly |
| the binary dies instantly after signing | the hardened runtime got applied; re-run with the identity empty |
| `the build produced no executable` | the project builds several targets, or none — name one with `--bin` |
| a daemon exits with "already running" / cannot bind | pass `--launchd <label>` so the installed copy is stood down |
| one wide frame that is just a wait | a permission never arrived — add `--accessibility` and re-record |
| `Trace file already exists ... specify append-run` | an earlier recording was interrupted; the script moves it aside, but a bare `cargo flamegraph` will not |
| an `--attach` run never returns | attaching has no natural end — pass `--duration` |
| perf: `Permission denied` on Linux | `perf_event_paranoid` too high; lower it or record with root |
