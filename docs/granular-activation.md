# Granular activation: which parts of a system are separable

> "can we produce like almost sub activateable and switchable composable parts
> that arent like full system closures? like different /etc/nix/nix.conf, have
> an explore so we can make it just switch out those parts or something like
> that instead of a full activate?"

and, sharpened:

> "i want things like changing the nix.conf such as changing maybe which window
> manager mode or something like that or taskbar as composable activation
> instead of full change everything that may be incredibly performance
> intensive"
>
> "activating homebrew for example is a lot"

Three examples, and they are **three different problems**. A config file nobody
holds open, a running process's config, and an activation script with side
effects are separated by different mechanisms, and a single mechanism that
claims all three is wrong. This document draws the line, prices each candidate,
and names the one piece that is implemented (`aspects/base/nix-conf-parts.nix`)
against the ones that stay on paper.

Everything labelled *measured* was run on this Mac in the session this was
written; everything else says what it is. There is a section at the end that is
nothing but the things that could not be checked from a darwin host.

## The answer first

A part of a configuration is separable when **both** of these hold:

1. **The reader test.** Something re-reads the part on its own schedule, without
   being told. nix re-reads `nix.conf` when a nix process starts. sshd on macOS
   re-reads `sshd_config.d/*` per connection. systemd does *not* re-read a unit
   drop-in — it has to be told, twice (`daemon-reload`, then restart the unit).
   If nothing re-reads it, "switching the part" means restarting something, and
   you are no longer doing less work than an activation — you are doing the same
   work with a worse audit trail.

2. **The declaration test.** Every alternative must produce the **same derived
   facts**. This tree derives firewall rules (`aspects/base/firewall.nix`), DNS
   records (`aspects/network/cloudflare.nix`), secret provisioning and
   reachability from declarations. A runtime choice the capability layer cannot
   see is a fact with no reader — the exact bug class this repo keeps fixing,
   and the reason `aspects/base/firewall.nix`'s header opens with "this is why
   every `expose` in the tree was inert". If swapping the part would change a
   bind address, a published name, a port or a secret, it is not a part. It is a
   configuration, and a configuration is what a specialisation is.

Both tests pass or it is not a part. The second one is the one that kills most
candidates, and it is the one that is easy to forget, because the first one is
the fun one.

### Where the line falls

| | separable | why |
|---|---|---|
| `nix.conf` settings | **yes** | `!include` re-read per nix process (measured); every alternative is invisible to the capability layer *by construction* — a substituter or builder a client dials is not a fact about this host that anything else reads |
| remote build machines | **yes** | same, and this is the implemented case |
| ssh **client** config | yes | `Include` re-read per invocation; `/etc/ssh/ssh_config` on this Mac already has `Include /etc/ssh/ssh_config.d/*` |
| sshd config, on darwin | yes | `ssh.plist` is `inetdCompatibility { Wait = false }` (measured) — a fresh `sshd` per connection, so a drop-in is live immediately |
| hand-written bar/WM config | **already is** | `aspects/base/file-local.nix`'s `linkLocalPath` puts it out of the store; `aspects/desktop/coolabah/coolabah.nix` says so in as many words |
| DNS answers | yes, with a daemon | `aspects/network/roaming-resolver.nix`. Not a file swap — a runtime API (`unbound-control local_data`) |
| ───── | ───── | ───── |
| **generated** bar config (`colors.lua`, `coolabahrc`) | no | store files, and stylix's theme is an eval-time fixpoint |
| which bar / which WM | no | different launchd jobs, different daemons, and a different Lua module set baked into each wrapper's search path |
| a codesigned binary's identity | **no** | code identity is re-derived from the final image at every exec (`aspects/darwin/wrapperd.nix`), so the signed binary is *copied* to a stable path — a symlink swap plants an unsigned path and loses the TCC grant |
| systemd units | no | drop-ins need `daemon-reload` + restart; and a unit is where `ExecStart`'s store path lives |
| users and groups | no | 139 `dscl` calls in this Mac's `activate` (measured); directory-service state, not a file |
| kernel, initrd, boot entry | no | not read again until reboot |
| anything that changes a port, a name, a secret or another host's view | **no** | fails the declaration test, whatever its reader does |

The interesting thing about that table is where `nix.conf` sits. It is not
separable because nix.conf is a simple format. It is separable because **the set
of things that read a substituter list or a builder list is exactly {nix}**, and
nix re-reads. The parts that are not separable are not hard to write a swapper
for; they are parts whose value some *other* reader already baked in.

## Case 1 — `nix.conf`: solved, and implemented

### `!include` is resolved per process, and that is already sanctioned here

`aspects/dev/nix-dev.nix:54-56` writes

```nix
nix.extraOptions = ''
  !include ${scoped.dev-nix.templates."extra-nix-conf".path}
'';
```

and that is live on this machine. Measured:

```
$ cat ~/.config/nix/nix.conf
extra-experimental-features = nix-command flakes
!include /Users/ivypierlot/Library/agenix/templates/dev-nix/extra-nix-conf
```

A store-path nix.conf naming a path that is not in the store. The pattern is
already load-bearing, so nothing below is novel — it is the same trick aimed at
a different setting.

Measured, with no activation and no rebuild between the lines:

```
$ echo 'cores = 7' > frag; NIX_USER_CONF_FILES=conf nix config show cores
7
$ echo 'cores = 3' > frag; NIX_USER_CONF_FILES=conf nix config show cores
3
$ rm frag;               NIX_USER_CONF_FILES=conf nix config show cores
0
```

Three properties fall out of that, and all three are load-bearing:

**The bang is not decoration.** `!include` on a missing file is a silent no-op —
the third line, where `cores` comes back to its default. A plain `include` on a
missing file is fatal:

```
error: file '/…/extra-probe' included from '/…/nixconf-probe2' not found
```

So with `!`, "no part selected" degrades to exactly the nix.conf the declaration
produced: there is no default to plant, no activation step to run, and no stale
symlink to clean up. Without `!`, a garbage-collected or hand-deleted part is a
machine that cannot run `nix` at all, which includes the `nix` you would use to
fix it.

**There is no glob.** Measured: `!include /…/partsdir/*.conf` with a matching
file in that directory left `cores` at `0`, while the explicit path gave `5`. So
every part has to be named in nix.conf at build time. That reads like a
limitation and is actually the property that makes this safe — **the set of
parts is declared; only the choice is runtime.** Nothing can introduce a new
switchable surface by dropping a file somewhere.

**Last key wins.** This Mac's `/etc/nix/nix.conf` has `builders-use-substitutes`
twice, and nix says so on every invocation:

```
warning: Found key 'builders-use-substitutes' twice while reading config from
'/etc/nix/nix.conf', discarding previously set values
```

*previously* set values are discarded, so the later line wins — and
`nix.extraOptions` is written at the *end* of the file by nix-darwin (measured:
`extra-sandbox-paths` and that duplicate sit below every `nix.settings` key). An
`!include` in `extraOptions` therefore overrides everything the declaration
wrote above it, which is what makes a part able to *change* a setting rather
than only add one.

### `builders` was already a runtime file indirection, by nix's own design

`nix config show builders` on this Mac returns `@/etc/nix/machines` — *verbatim*,
not expanded. The `@` is kept as a string and resolved later. Measured, same
command twice, no activation in between, the only change being the file:

```
$ nix build --max-jobs 0 --builders @…/machines-probe --expr '<a drv>'
error: unable to start any build; either set '--max-jobs' to a non-zero value
       or enable remote builds.                       # file did not exist

$ echo 'ssh-ng://nobody@definitely-not-a-real-host.invalid aarch64-darwin - 1 1 - - -' \
    > …/machines-probe
$ nix build --max-jobs 0 --builders @…/machines-probe --expr '<a drv>'
cannot build on 'ssh-ng://nobody@definitely-not-a-real-host.invalid': …
       ssh: Could not resolve hostname definitely-not-a-real-host.invalid
```

So nix's **default** for `builders` is already a mutable-file reference. The
whole mechanism this document is about is a thing nix does to itself.

Two more measurements make the implementation possible:

- An **inline** value works as well as `@file`, so a part can be one fragment
  with the whole line in it rather than a fragment pointing at a second file:
  `builders = ssh-ng://nobody@inline-bogus-host.invalid aarch64-darwin - 1 1 - - -`
  inside an `!include`d fragment was tried by nix.
- **Several specs separated by `;`** in one inline value are all tried
  (measured: two bogus hosts, both attempted).
- `builders =` with an **empty** value parses — `nix config show builders` comes
  back empty rather than erroring — which is how "no remote builders" is said.

### The thing this fixes is not a stale address

The brief arrived with "this Mac's `/etc/nix/nix.conf` reportedly still names a
stale builder `10.100.0.2` where auspc is now `192.168.1.11`". That is worth
correcting, because the correction *is* the design.

`/etc/nix/machines` on this Mac, measured:

```
ssh-ng://builder@10.100.0.2    x86_64-linux  …  10 20  big-parallel,cached-compilation,kvm
ssh-ng://ivy@faggot.sh         x86_64-linux  …   6  8  big-parallel
ssh-ng://builder@10.100.0.1    x86_64-linux  …   5  5  big-parallel,cached-compilation,kvm
ssh-ng://builder@linux-builder x86_64-linux,aarch64-linux … 1 1 kvm,benchmark,big-parallel
```

and the fleet, measured through the fast `.#debug.config` loop:

```json
"auspc":    { "dial": "10.100.0.2", "lan": "192.168.1.11" },
"secondpc": { "dial": "10.100.0.1", "lan": "192.168.0.26" },
"faggot.sh":{ "dial": "faggot.sh",  "lan": null }
```

Nothing is stale. `builder.ipAddress` is **deliberately** a wireguard address —
`aspects/hosts/fleet.nix`'s own option description says so: "`builder.ipAddress`
is a wireguard address, so it is the wrong thing to reach a box on the same
switch with". Both addresses are correct and they are different wires.

So there is no one-line fix, because there is no bug in the declaration. What
there is, is `aspects/base/builders.nix` having to choose a wire at **evaluation
time** for a laptop that is on a different wire half the week — which is
`docs/roaming-resolver.md`'s sentence verbatim:

> The route is not knowable at evaluation, which is exactly why it must not be
> ranked at evaluation.

`builders` is the one setting where that can be answered without a resolver, a
probe or a daemon, because the whole value is a line of text that nix re-reads
when it starts.

### What is implemented

`aspects/base/nix-conf-parts.nix`, on every host (`den.schema.host.includes`,
the same way `aspects/base/builders.nix` gives every host build machines), and
**inert until something writes the live link**.

- `nix.extraOptions` gains one static line, `!include /etc/nix/parts/builders.conf`.
  That line never changes, so it does not make nix.conf churn.
- The alternatives are rendered into `environment.etc."nix/parts-available/builders/<alt>.conf"`.
  Living in the system closure is what keeps them alive across a GC — a GC root
  per alternative, for free, with no hand-rolled `gcroots` entry — and it is also
  what makes them discoverable with `ls` instead of only through the flake.
- `nix-part` lists the parts, says which is live, and switches:

```
$ nix-part
builders	live: (none, so nix.conf's own value applies)
  all
  lan
  local
  only-auspc
  only-faggot-sh
  only-linux-builder
  only-secondpc

$ nix-part builders lan        # one symlink; no activation, no build
$ nix-part builders none       # back to the declared configuration
```

The per-machine alternatives are named after the **fleet entry**, joined on the
dialled address, falling back to the sanitised address for anything the fleet
does not model — so `only-auspc` rather than `only-10-100-0-2`.

The alternatives are:

- **`all`** — every entry of `nix.buildMachines` at its declared address, which
  is byte-equal to `/etc/nix/machines`. Present so that "go back" does not have
  to mean "delete the file".
- **`lan`** — the same list with every one of *our* boxes dialled on the home
  LAN where it has an address there: auspc at 192.168.1.11, secondpc at
  192.168.0.26. Same boxes, same host keys, same ssh key, one hop instead of a
  tunnel. `faggot.sh` (dialled by DNS, no LAN address) and nix-darwin's
  `linux-builder` are left exactly as declared.
- **`local`** — no remote builders at all. The cafe case. `aspects/base/caches.nix`'s
  `fallback = true` already turns an unreachable *substituter* into a miss, but
  an unreachable *builder* is still a connection timeout per build, and this is
  the only lever that removes it without a rebuild.
- **`only-<host>`** per machine, for pinning one.

Measured, on the real `darwinConfigurations.Ivys-MacBook-Pro` (one full host
eval, behind the lock):

```
nix/parts-available/builders/{all,lan,local,only-auspc,only-faggot-sh,
                              only-linux-builder,only-secondpc}.conf

nix.extraOptions:
  builders-use-substitutes = true
  !include /etc/nix/parts/builders.conf

all:  ssh-ng://builder@10.100.0.2 …  ; ssh-ng://ivy@faggot.sh …
      ; ssh-ng://builder@10.100.0.1 …  ; ssh-ng://builder@linux-builder …
lan:  ssh-ng://builder@192.168.1.11 … ; ssh-ng://ivy@faggot.sh …
      ; ssh-ng://builder@192.168.0.26 … ; ssh-ng://builder@linux-builder …
```

`all` reproduces `/etc/nix/machines` entry for entry **including
`linux-builder`**, which is the measurement that caught the `fleet`-derived
first draft; `lan` moves exactly auspc and secondpc and leaves `faggot.sh` and
`linux-builder` alone, which is the measurement that says the join is on the
right key.

**The source is `nix.buildMachines`, not `fleet`.** The first draft rebuilt the
list from `fleet` the way `builders.nix` does, and that is the same fact written
twice — able to disagree, and it did: `aspects/darwin/linux-builder.nix`
contributes an entry that is in `nix.buildMachines` and in no fleet record, so
an `all` derived from `fleet` silently dropped it. Rendering the evaluated
option cannot disagree with the file nix-darwin writes beside it. The `lan`
rewrite then joins on the dialled address, so an entry that is not one of ours
falls through untouched rather than being guessed at.

### Two honest caveats

**The daemon.** `builders` is consulted by whichever process performs the build.
On this Mac that is `org.nixos.nix-daemon`, a long-lived process that parsed
`/etc/nix/nix.conf` when it started — so a changed fragment does *not* reach a
running daemon by itself. What makes the probes above work is that the **client**
also reads `/etc/nix/nix.conf` on every invocation and, for a *trusted* user
(`trusted-users = root @admin`; this user is in `admin`), forwards its settings
to the daemon for that connection. So:

- for root and for a trusted user, a part takes effect on the next `nix`
  invocation. Measured explicitly against the daemon store, so this is not an
  artefact of a client-side build:

  ```
  $ nix build --store daemon --max-jobs 0 --expr '<a drv>'   # NIX_USER_CONF_FILES
  cannot build on 'ssh-ng://nobody@daemon-probe-host.invalid': …
  1 available machines:
  ```

  The part's machine is the only one the daemon considered, although
  `/etc/nix/machines` has four;
- for an **untrusted** user, it does not — and neither does anything else, since
  `builders` is a trusted-only setting in the first place.

The general form of this caveat is the one that matters when adding a second
part: **"nix re-reads nix.conf per process" is true of the nix you type; it is
not true of the daemon.** A part whose setting is only ever consulted by the
daemon (`build-users-group`, `sandbox`) needs a daemon reload, at which point it
is a service restart and belongs in the second category below. nix-darwin
already does exactly that reload, and gates it — see the next section.

**`post-build-hook` is a store path.** Any part whose value *names a store path*
re-introduces the GC-root problem the `environment.etc` placement solves for the
fragments themselves. The fragment is rooted; what the fragment mentions is not,
unless it is rooted some other way. `builders` mentions an agenix secret path
and a base64 host key, neither of which is a store path, which is part of why it
is the right first part.

## Case 2 — the bar and the window manager: half of this is already done

A bar or a WM is a **running process**, so this is a different problem from
nix.conf: there is a file, and there is something holding a decision made when
it last read that file.

### The hand-written half is already out of the store

`aspects/base/file-local.nix` declares

```nix
config.lib.file.linkLocalPath = path:
  config.lib.file.mkOutOfStoreSymlink (config.lib.file.getLocalPath path);
```

and `aspects/desktop/sketchybar/sketchybar.nix:77` links the whole `sketchybar/`
directory through it; `aspects/desktop/coolabah/coolabah.nix` links each
top-level entry individually, and its comment states the property outright:

> `linkLocalPath` keeps each one pointing at the working tree, so editing the
> bar config is still a reload rather than a rebuild

So **for the bar, the answer to the user's question already exists in this tree**
and it is not a new mechanism — it is `mkOutOfStoreSymlink`. Editing
`sketchybar/items/*.lua` changes the config the running bar will read, with no
build and no activation.

One caveat found by looking at the machine rather than the repo: this Mac's live
generation predates that change. Measured, `~/.config/coolabah/bar.lua` resolves
to `/nix/store/…-hm_bar.lua`, not to the working tree. The property is in the
repo and not yet on the box.

### The generated half is not, and cannot easily be

What is still a store file in `~/.config/coolabah/`: `colors.lua`,
`icon_map.lua`, `coolabahrc`, `paneru_bar.lua`, `wm.lua`, `paneru.so`. Those are
generated — `colors.lua` from `config.stylix.base16Scheme`, `coolabahrc` from
`programs.coolabah.config`, the Lua modules from `mkLuaFileModule` — and the
theme is an eval-time fixpoint that many other things read. A runtime theme swap
therefore fails the **declaration test**: stylix's palette also reaches GTK,
terminal colours, the wallpaper and the lock screen, so a bar that was switched
behind the flake's back is a machine whose bar does not match itself.

That is the honest shape of the answer for "taskbar as composable activation":
**the parts of the bar you write are already live; the parts that are derived
from the theme are not separable, because the theme is not a bar fact.**

### "which window manager mode" is two different questions

**Layout, gaps, keybinds** — the contents of `paneru.setup{…}` in
`~/.config/paneru/init.lua`, written from `services.paneru.config`. That is a
file plus a reload, and paneru has the reload: measured, `paneru --help` offers
`send-cmd`, `query`, `subscribe`, `state` and `restart` over a unix socket. So a
"mode" in this sense is implementable the same way the nix.conf part is — an
out-of-store `init.lua` plus `paneru send-cmd restart` — and it would pass both
tests, since nothing outside paneru reads its gaps.

**Which bar and which WM** — not separable, and for a reason specific to this
tree. `aspects/wms/paneru/default.nix` decides from *options* whether this user
runs sketchybar or coolabah, and that decision selects which Lua modules are
built into the daemon's module search path (`paneru_bar`, `wm`,
`paneru_coolabah_events`, …) and which launchd agent exists. Three different
things change together: a daemon, a module set baked into a wrapper, and a
launchd job. That is a configuration.

And it is worse than merely "three things", because of codesigning.
`aspects/darwin/wrapperd.nix` states the constraint:

> A setuid-style execve stub could not carry a signature — code identity is
> re-derived from the final image on every exec — which is why the signed binary
> is still COPIED to the stable path rather than symlinked or re-execed.

So **the generic symlink-swap mechanism (candidate 3) is unavailable for any
codesigned binary on darwin.** A swap has to be a copy, and the copy is what TCC
evaluates. There is already a daemon in this tree that does exactly that copy —
`wrapperd serve`/`wrapperd plant` — which is a sub-activation in everything but
name, and the right place to hang a WM swap if one is ever wanted. It is not a
file-swap mechanism and should not be made to look like one.

## Case 3 — homebrew: not a granularity problem

This is the one the user named as expensive, and the measurements say the
diagnosis in the brief is right: **this is an activation script with no change
detection, not a part that needs separating.**

### What it actually does, every single switch

The live `/run/current-system/activate`, lines 2991-2996, verbatim:

```bash
# Homebrew Bundle
echo >&2 "Homebrew bundle..."
if [ -f "/opt/homebrew/bin/brew" ]; then
  PATH="/opt/homebrew/bin:…/mas-7.0.0/bin:$PATH" sudo --preserve-env=PATH \
    --user=ivypierlot --set-home env brew bundle \
    --file='/nix/store/0bdk901qrrs9aghlrwbz8llf761yjwsq-Brewfile' --force-cleanup
else
  …
fi
```

No condition on the Brewfile having changed. No stamp. No comparison against
the previous generation's Brewfile — although the generation right above it does
exactly that for the nix daemon (see below). And `grep HOMEBREW /run/current-system/activate`
returns only `INSTALLING_HOMEBREW=1`: **no `HOMEBREW_NO_AUTO_UPDATE`, no
`--no-upgrade`.**

That follows from `aspects/darwin/homebrew.nix`:

```nix
onActivation = {
  autoUpdate = true;
  upgrade = true;
  cleanup = "uninstall";
};
```

against nix-darwin's `modules/homebrew.nix:188-197`, which only emits
`HOMEBREW_NO_AUTO_UPDATE=1` when `!autoUpdate` and only passes `--no-upgrade`
when `!upgrade`. So every `darwin-rebuild switch` on this machine asks Homebrew
to **git-fetch its taps and upgrade everything outdated**, as part of activating
a configuration that may not mention Homebrew at all.

### What that costs, measured

The Brewfile is 5 taps, 1 formula, 16 casks.

```
$ HOMEBREW_NO_AUTO_UPDATE=1 time brew bundle check --file=<Brewfile> --verbose
The Brewfile's dependencies are satisfied.
real 3.02

$ HOMEBREW_NO_AUTO_UPDATE=1 time brew outdated --greedy
craft
font-sf-pro
raycast
sf-symbols
zotero
real 20.74
```

So, as a floor, with auto-update suppressed and nothing to do: **3 seconds** to
decide the Brewfile is satisfied. The configured activation does not get that
floor — with `upgrade = true` it goes on to the `--greedy` version check, which
measured **20.7 seconds** on its own, and then **re-downloads and reinstalls
those five casks**, every switch, forever, because `--greedy` casks are
"outdated" by definition (they auto-update themselves and brew cannot tell).
Plus `brew update`'s git fetch on top, which was not measured because measuring
it means mutating tap state.

That is the user's "activating homebrew is a lot", and none of it is caused by
granularity. A granular mechanism that let you skip the homebrew step would be
solving the symptom.

### nix-darwin already shows what the fix looks like

One activation step up, `modules/nix/default.nix:935`:

```bash
if ! diff /etc/nix/nix.conf /run/current-system/etc/nix/nix.conf &> /dev/null \
   || ! diff /etc/nix/machines /run/current-system/etc/nix/machines &> /dev/null; then
    echo "reloading nix-daemon..." >&2
    launchctl kill HUP system/org.nixos.nix-daemon
fi
```

A side-effecting step, gated on an actual diff against the live generation. That
is the shape the homebrew step is missing, and it is ten lines, not a mechanism.

### So the recommendation, in order

1. **`onActivation.autoUpdate = false` and `upgrade = false`.** These make the
   step *deliberately* time-dependent: they are the reason it cannot be
   idempotent-by-input and therefore the reason it cannot be guarded. Upgrading
   casks is a thing to run on purpose, not a side effect of activating an
   unrelated change. This is the single highest-value edit in this document and
   it is one line each.
2. **Then guard the step on the Brewfile.** With `autoUpdate`/`upgrade` off,
   `brew bundle`'s effect is a function of the Brewfile alone, so a stamp
   comparing `/nix/store/…-Brewfile` against the last one applied makes it a
   no-op when nothing changed. That is a `nix-darwin` upstream change or a
   `postActivation` override; either way it is change detection, not separation.
3. Only then, if it is still slow, ask about granularity.

This is deliberately not implemented here. (1) changes what the machine does to
packages the user did not ask about in this session, and (2) depends on (1).

### And the same villain shows up in the store measurements

See candidate 4 below: of the three specialisations currently built into this
Mac's system, the entire marginal store cost of two of them is **one 39 MB
path**, `taps-env` — a nix-homebrew artefact. Homebrew is the most expensive
thing about activation *and* the most expensive thing about having
specialisations at all.

## The darwin finding: activation is already eighteen named parts

`nix-darwin`'s `modules/system/activation-scripts.nix` declares
`system.activationScripts` as an `attrsOf` named fragments and then **inlines
their `.text`** into one script:

```nix
${cfg.activationScripts.groups.text}
${cfg.activationScripts.users.text}
${cfg.activationScripts.etc.text}
${cfg.activationScripts.defaults.text}
${cfg.activationScripts.launchd.text}
…
${cfg.activationScripts.homebrew.text}
```

The live script, 3043 lines, carries them in that order (measured, by banner):

```
 963  groups            2655  restarting Dock
 987  users             2660  launchd services
2298  pam               2861  user launchd services
2315  patches           2925  networking
2381  /etc              2942  application firewall
2429  user defaults     2950  power
                        2965  fonts
                        2982  nvram
                        2987  Homebrew prefixes
                        2992  Homebrew bundle
                        3003  tmpfiles
                        3022  home-manager
```

And the part that matters: `modules/lib/write-text.nix` gives **every** fragment
a `source` that defaults to `pkgs.writeScript "activate-<name>" config.text`. A
standalone executable store path is already *defined* for each one; the combined
script just never references it, so it is never built.

That makes "run only the `/etc` step" or "run only the homebrew step" roughly a
five-line change to expose `$out/activate-parts/<name>`, with two real costs:

- **A fragment is not self-contained.** It relies on the preamble — `set -e`,
  `PATH`, `USER=root`, `HOME`, and `systemConfig=@out@`, which is substituted by
  the toplevel builder. A runner has to reconstruct that, which is a dozen lines
  and is exactly what `aspects/tooling/specialise.nix` already does for whole
  generations.
- **Order is a real dependency and running one part breaks the invariant that
  the others ran.** `homebrew` runs after `setup-homebrew`; `launchd` runs after
  `etc`; `wrapperd` deliberately uses `lib.mkAfter` in `postActivation` to land
  after `extraModules/darwin/wrappers`' `rm -rf`. Per-part activation is
  therefore safe for *leaf* steps and unsafe as a general mechanism, which is
  the same shape as the rest of this document: the enumeration is the
  deliverable, not the mechanism.

The honest version of candidate 5 for darwin is: **`/etc` is separately
activatable, and so is homebrew, and the rest are not worth separating.** `/etc`
is the one the user asked about.

## The candidates, priced

### 1. Runtime `include` indirection — free, where the format has it

What the formats actually do, with the evidence level marked:

| format | runtime include? | when re-read | evidence |
|---|---|---|---|
| `nix.conf` | `include` / `!include`, **no glob** | nix process start | measured here |
| `nix.conf` `builders` | `@file` | when a build needs a machine | measured here |
| ssh **client** | `Include`, globs | every invocation | `/etc/ssh/ssh_config` on this Mac already does it |
| sshd (darwin) | `Include` dir | **per connection** — `ssh.plist` is `inetdCompatibility { Wait = false }` | measured (plist) |
| sshd (NixOS) | `Include` dir | daemon start, or SIGHUP | not checked here; depends on `startWhenNeeded` |
| unbound | no useful include | — but `unbound-control local_data` is an atomic runtime API | `aspects/network/roaming-resolver.nix` relies on it |
| systemd units | drop-ins are **build-time** | `daemon-reload` + restart the unit | certain, by design |
| samba | `include =` | smbd rechecks `smb.conf` periodically | documented upstream, **not measured** |
| nginx | `include` | config load / reload only | certain |

The pattern: a format has a *useful* runtime include when the reader is
short-lived (nix, the ssh client, per-connection sshd). A long-lived daemon's
include is read once, so for daemons the lever is a **control socket**, not a
file — which is precisely why `roaming-resolver.nix` chose unbound over dnsmasq.

### 2. `systemd-confext` / `systemd-sysext` — designed for this, unavailable here

- The locked nixpkgs is `1cabc318c112…`, systemd **258.2**. Upstream systemd has
  supported confext since 253, so the tooling exists in principle.
- **nixpkgs has nothing.** Measured: `grep -ril 'sysext\|confext'` over the
  whole of `nixos/` in the locked tree returns **zero hits** — no module, no
  option, no test, no documentation. There is nothing to enable.
- The interaction is the real problem even with a hand-rolled module. NixOS's
  `/etc` is a tree of symlinks into `/etc/static`, managed by the `etc`
  activation script; `systemd-confext` works by mounting an **overlayfs over
  `/etc`** from an extension image. Those two are not designed to coexist, and
  which one wins depends on mount ordering that nothing in NixOS sequences.
  `system.etc.overlay.enable` exists (`nixos/modules/system/etc/etc.nix:82`) and
  is marked experimental with "only enable this option if you're confident that
  you can recover your system if it breaks" — nothing in this tree sets it.
- And the cost even if it worked: an image per extension, `/etc` becomes an
  overlay, and the extension's contents are invisible to `environment.etc`,
  which means invisible to the flake, which means it fails the declaration test
  for anything derived.

**Verdict: not available, and it would be the wrong shape if it were.** It
overlays *files*; the things that are hard here are daemons and side effects.

### 3. Symlink swap among pre-built store paths — this is what is implemented

The roaming-resolver pattern generalised, and the mechanism behind
`nix-conf-parts`. Two questions the brief asked:

- **What keeps the alternatives alive?** Being in the system closure.
  `environment.etc."nix/parts-available/builders/<alt>.conf"` is a reference of
  the `etc` derivation, which is a reference of the toplevel, which is
  `/run/current-system`, which is a GC root. No per-alternative `gcroots` entry,
  and no way for the set of alternatives to drift from the set the flake
  declares.
- **How does a human find out which is live?** `nix-part` with no arguments
  reads it off the filesystem, which is the only place it exists. There is
  deliberately nothing baked in to compare against — the same discipline
  `aspects/tooling/specialise.nix` applies ("reads the LIVE generation rather
  than anything baked in").

The limit, restated because it is the one that bites: **not for a codesigned
binary on darwin.** See `wrapperd.nix` above.

### 4. Specialisation with `inheritParentConfig` — measured, and the objection is not cost

This Mac has three specialisations built in right now. The marginal closure of
each, computed as "paths reachable from this specialisation that the base
configuration does not otherwise have" — the base closure taken as the closure
of the parent's direct references minus the three specialisation symlinks *and*
minus the parent's own self-reference, which is the subtlety that makes the
naive version of this measurement report zero:

```
parent closure                           3081 paths
base closure without specialisations     3002 paths

assessment    closure 1308 paths   marginal  35 paths,      1,465,296 B
life-project  closure 1374 paths   marginal  29 paths,     40,521,080 B
study         closure 1271 paths   marginal  36 paths,     40,562,160 B
```

Largest marginal paths:

```
life-project / study:  taps-env             39,068,480   <- nix-homebrew
all three:             home-manager-path       ~520,000
                       system-path             ~292,000
                       fish completions         198,240
                       man-cache                144,312
```

Against a 25.3 GB system closure. So **a specialisation costs about 1.5 MB of
store, plus 39 MB if homebrew is in it.** And a specialisation that changed
*only* `/etc/nix/nix.conf` would be a subset of that same ~30-path tail — the
self-NAR sizes are `nix.conf` 2,288 B, `etc` 10,072 B, `etc-machines` 1,064 B,
`darwin-system` 125,792 B, so on the order of **250 KB**.

**That changes the answer to the user's question.** The objection cannot be
store cost; it is a rounding error. What a specialisation actually costs is:

- **a second full evaluation.** `lib/specialisations.nix` parks each one inside a
  private `evalModules` over a one-entity fleet, so the fixpoint is paid again
  per profile. On this tree that is the several-GB eval `CLAUDE.md` warns about,
  once more per specialisation.
- **a full activation to switch into.** `specialise study` runs
  `darwin-rebuild activate` on the target generation — every one of the eighteen
  steps above, including the 20-second homebrew step, to change a config file.
- **a declared combination.** `aspects/tooling/nh-checked.nix` is explicit:
  "exclude lists are not composed at build time, because that is the
  dynamic-variant design this deliberately does not have." Two parts switching
  independently need 2ⁿ declared profiles.

That last point is where the user's request and `nh-checked.nix`'s note meet
head-on, and they do not actually conflict — because they are about different
things. `nh-checked` refuses to compose **configurations**, and it is right to:
a composed exclude list is a configuration nobody declared, nobody evaluated and
nobody can name. What this document proposes composes **parts**, and a part is
by definition something whose alternatives change no derived fact. Two parts
compose freely precisely *because* neither one is a configuration. The admission
test in "The answer first" is what keeps them on the right side of that line —
and the day a proposed part fails it, `nh-checked`'s answer is the correct one
and the part should become a specialisation.

So the fix for the user's complaint is three separate things, none of which is a
general mechanism:

1. the parts that genuinely are parts, switched by file (implemented, for
   `builders`);
2. the activation steps that are slow because they have no change detection,
   guarded (homebrew);
3. and for the rest, better *ergonomics* on specialisations rather than a new
   mechanism — which is what `specialise` and `nh-checked` already are.

### 5. `environment.etc` and the `etc` derivation

- **NixOS:** `system.etc.overlay.enable` exists in the locked nixpkgs and is
  experimental. With it off, `/etc` is rebuilt by an activation script that can
  in principle be run alone; with it on, `/etc` is a mount and swapping it is a
  remount. Neither was tested — there is no NixOS host reachable from this
  session.
- **darwin:** the `/etc` step is `system.activationScripts.etc`, fragment 5 of
  18, and per `write-text.nix` it already has a standalone `.source`. It is the
  cleanest candidate for per-part activation in the whole script, and it is
  idempotent by construction (it relinks `/etc/static` and the individual
  symlinks).

Worth knowing: `aspects/darwin/wrapperd.nix` records that
`org.nixos.activate-system`'s boot job "only relinks `/run/current-system`, sets
gcroots, checks the nixbld users and rebuilds `/etc`". So **a subset of
activation already runs on its own, at boot, today**, and `/etc` is in it. The
mechanism is not hypothetical; it is just not addressable by hand.

## How each proposal meets the declaration layer

This tree derives firewall rules, DNS records, secrets and reachability from
declarations, and `aspects/base/firewall.nix` opens by recording what happens
when a derivation silently sees the wrong thing: "every port that looked derived
was in fact a literal somewhere else, which is exactly why nobody noticed." A
runtime-swappable part the capability layer cannot see is the same failure
waiting to happen. So, per proposal:

- **`nix-conf-parts` / `builders`: safe, and safe for a stated reason.** Which
  build machine a *client* dials opens no port on the client, publishes no name,
  and uses the same `builder-ssh-key` secret whichever address it dials. The
  server side (`den.aspects.builder-server`, its `trusted-users` and its
  `authorized_keys`) is declared on the builder and is unchanged by the client's
  choice. Every alternative therefore produces identical derived facts, and the
  flake can be read without knowing which is live.
- **A substituter part would also be safe** for the same reason, with one extra
  condition: `aspects/base/celler/push.nix` derives netrc machine lines and the
  push hook's endpoints from the *resolved* endpoint, so a part that moved a
  substituter to a different address without moving the netrc line would be a
  cache that 401s. The part would have to carry both, or neither.
- **A bind-address part would not be safe.** `firewall.nix` reads the carrier's
  interface off the link and opens exactly that; `cloudflare.nix` publishes the
  resolved address. Swapping a bind address at runtime leaves the firewall open
  on the wrong wire and the DNS pointing at the old one. This is a
  specialisation.
- **A theme part would not be safe.** stylix's palette reaches far beyond the
  bar.
- **`confext` is structurally unsafe here**, not just awkward: its contents
  arrive from an image that `environment.etc` has never heard of, so *nothing*
  derived can see them. That is this bug class with the mechanism built in.
- **`roaming-resolver` is the model for how to do it anyway**, and worth
  re-reading for the trick: it changes only the **address** a published name
  resolves to, and the published name is the thing every derived fact actually
  depends on. It passes the declaration test by construction, because it changes
  nothing the declaration layer models. Any future part should be judged by
  whether it can be reframed that way.

## Not verified

1. **That the `!include` works at *system* level, and for a non-trusted user.**
   Everything measured went through `NIX_USER_CONF_FILES` or `--builders` as a
   trusted user — including the `--store daemon` run, which proves the daemon
   honours the fragment but not that it would read it from `/etc/nix/nix.conf`
   on its own. Editing `/etc/nix/nix.conf` was out of scope and no switch was
   performed, so the `!include` line `nix-conf-parts` adds has never been in a
   real `/etc/nix/nix.conf`. The aspect's content was verified by evaluating the
   laptop's `darwinConfigurations` entry; nothing was built and nothing was
   activated.
2. **That `environment.etc."nix/parts-available/…"` lands as expected on darwin.**
   The nested-path form is what nix-darwin itself uses for
   `/etc/ssh/sshd_config.d/100-nix-darwin.conf` (measured as an `/etc/static`
   symlink), which is the whole of the evidence.
3. **The NixOS half of everything.** No NixOS host was reachable. `system.etc.overlay`,
   systemd drop-in behaviour, sshd re-read semantics under `startWhenNeeded`,
   and whether `systemd-sysext`/`systemd-confext` binaries are even present in
   the locked nixpkgs' systemd output are all unchecked from here.
4. **`brew update`'s cost.** Deliberately not measured: with
   `nix-homebrew`'s `mutableTaps = false` the taps are store paths, so running
   `brew update` would either fail or mutate state this session had no business
   mutating. The 3.02 s and 20.74 s figures both suppressed auto-update, so both
   are **floors** on what activation pays, not estimates of it.
5. **That five `--greedy` casks are reinstalled on every switch.** `brew outdated
   --greedy` lists them (measured) and `--greedy` casks are outdated by
   definition, but the reinstall itself was not observed — doing so means
   running the activation step.
6. **That `nix-part`'s `sudo mv` is atomic against a concurrently starting nix.**
   `rename(2)` over an existing name is atomic on APFS, so the window `ln -sfn`
   would leave does not exist; this is reasoning from the syscall, not an
   observation.
7. **Which specialisations the measured numbers generalise to.** The three on
   this Mac are all *subtractive* (they exclude aspects). A specialisation that
   *added* something would have a marginal closure the size of what it added,
   and nothing here bounds that.
8. **The homebrew recommendation.** Not implemented, so not tested. Turning
   `onActivation.upgrade` off changes what the machine does to Homebrew
   packages, which is a decision rather than a fix.
