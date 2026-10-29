# Where the evaluation time goes

Everything here was measured on this box (10 cores, 16 GB, `aarch64-darwin`,
Lix 2.96.0-dev) on 2026-10-23, against the working tree as it stood. The method
is written out beside each number so it can be re-run; where a figure is an
estimate rather than a measurement it says so.

Two caveats that apply to every wall-clock figure below, and the reason the
*deterministic* counters are quoted in preference to them:

- **Two other agents were evaluating this flake concurrently.** Wall time is
  therefore an upper bound and noisy by tens of percent. `nrFunctionCalls`,
  `nrThunks`, `nrOpUpdateValuesCopied` and the per-position call counts are
  deterministic for a given expression and are not.
- **`NIX_COUNT_CALLS=1` is on for every profile.** It adds per-call bookkeeping,
  so `cpuTime` is inflated relative to an ordinary run. The *ratios* between
  positions are what the profile is for.

## Method

Lix has no sampling eval profiler in this build (`nix eval --help` lists none,
and `--eval-profiler` does not exist), so the instrument is the interpreter's
own counters:

```
NIX_SHOW_STATS=1 NIX_COUNT_CALLS=1 NIX_SHOW_STATS_PATH=/tmp/x.json \
  nix eval --raw '.#nixosConfigurations.secondpc.config.system.build.toplevel.drvPath'
```

`NIX_SHOW_STATS` gives the totals (thunks, function calls, `//` operations, GC
heap). `NIX_COUNT_CALLS` adds a `functions` array to that JSON: one record per
**source position** with a call count — `{ file, line, column, name, count }`.
That is the attribution; it is counts rather than seconds, but at these ratios
the two orderings agree.

Two more instruments that are already in the tree and are much cheaper than a
profile:

- `TRACE_INPUTS=1` (`lib/inputs.nix:63`) prints `LOCK_READ <layer>` when a
  layer's `flake.lock` is parsed and `INPUT_EVAL <name>` when something resolves
  an input node. That separates "read lock data" from "fetch and evaluate a
  flake".
- `builtins.trace "LAYERS_MODULE_EVAL"` (`aspects/framework/layers.nix:83`),
  seq'd into `config.flake.layerPlan`, prints exactly when the layer plan is
  forced.

Every eval was serialised behind one lock, per `../CLAUDE.md`.

## 1. One full host evaluation — secondpc

```
nix eval --raw .#nixosConfigurations.secondpc.config.system.build.toplevel.drvPath
```

| | |
| --- | --- |
| wall | **18 m 45 s** |
| `cpuTime` | 844.7 s |
| GC time | 952.9 s (over 210 cycles) |
| GC heap | **13.75 GB** |
| total allocated | **257 GB** |
| thunks | 1,433,074,982 |
| function calls | 825,072,083 |
| `//` operations | 576,751,351, copying **10,532,569,395 values** (18.3 per `//`) |
| attrset bytes | 175.5 GB of the 257 GB |

The run ended in an *unrelated* failure — `age.secrets."mail-ddns/ddns-token"`
has no generated file, which another branch is mid-edit on — at
`system.systemBuilderCommands`, i.e. after the whole module fixpoint and almost
all derivation instantiation. Lix writes the stats file anyway, so these are the
numbers for a near-complete host evaluation, not a complete one.

GC time exceeding `cpuTime` is not a typo: Lix reports `gcFraction = 1.128`
because collection runs on its own threads. 13.75 GB of live heap on a 16 GB box
is also the direct explanation for `../CLAUDE.md`'s rule about serialising host
evals — two of these do not fit, and the loser is killed silently.

### The top contributors, with positions

Deduplicated by `(file, line)` — `NIX_COUNT_CALLS` emits one record per column,
so a call site appears twice:

```
 189,878,961  optionalAttrs    <nixpkgs>/lib/attrsets.nix:1479
 186,563,626  pathSetByScope   <den>/nix/lib/aspects/fx/identity.nix:112
   4,123,586  value            <nix-effects>/src/comp.nix:114
   3,821,592  bothHave         <nixpkgs>/lib/modules.nix:1075
   3,415,113  value            <nix-effects>/src/comp.nix:36
   3,376,092  isOptionDecl     <nixpkgs>/lib/modules.nix:897
   3,247,728  __functor        <nix-effects>/src/api.nix:14
   2,599,949  escapeOptionPart <nixpkgs>/lib/options.nix:826
```

**Two positions are 45.6% of every function call in the evaluation**, and they
are the same position twice: `optionalAttrs` is called *from* `pathSetByScope`.

`<den>/nix/lib/aspects/fx/identity.nix:112` is this:

```nix
pathSetByScope =
  _:
  let
    prev = (state.pathSetByScope or (_: { })) null;
    scope = state.currentScope;
    scopeSet = prev.${scope} or { };
  in
  prev
  // {
    ${scope} =
      scopeSet
      // { ${nodeKey} = true; }
      // lib.optionalAttrs (nodeBaseKey != nodeKey) { ${nodeBaseKey} = true; };
  };
```

The aspect-membership set is accumulated as a **chain of lambdas stored in
state**, each one re-applying its predecessor (`(state.pathSetByScope or …)
null`). Nix does not memoise a function application, so every *read* of the path
set re-runs the entire chain accumulated so far, and the chain grows by one per
resolved aspect node. That is quadratic in the number of nodes, and it is where
the 576 M `//` operations and the 10.5 billion copied values come from: 175.5 GB
of the 257 GB allocated in this evaluation is attribute sets built by that one
expression.

The consumer is `hasAspect`:

```
   1,114,498  refKey                 <den>/nix/lib/aspects/has-aspect.nix:8
   1,111,157  pathSetFor             <den>/nix/lib/aspects/has-aspect.nix:111
   1,111,157  check                  <den>/nix/lib/aspects/has-aspect.nix:114
   1,111,157  __functor              <den>/nix/lib/aspects/has-aspect.nix:117
   1,111,157  flattenPathSetByScope  <den>/nix/lib/aspects/fx/identity.nix:81
```

1.11 million `hasAspect` calls, each forcing the accumulator: 186.6 M inner
calls, ~168 per `hasAspect`.

`../CLAUDE.md` says "`hasAspect` is a projected lookup", and for the *entity
surface* that is true — the flake-level capability loop really does answer in
seconds. Inside a host's fixpoint it is not a lookup; it is this.

### By source tree

Function calls grouped by the store path the position lives in:

| tree | calls | share |
| --- | --- | --- |
| nixpkgs lib + NixOS modules | 544,296,726 | 66.0% |
| **den** | 216,343,218 | 26.2% |
| nix-effects (den's `fx` interpreter) | 48,396,099 | 5.9% |
| **this flake's own aspects** | 13,895,962 | **1.7%** |
| nixvim lib | 1,158,918 | 0.1% |

This is the single most useful number in the document. **The aspects are not the
cost.** 98% of the work is the module system and den's identity/effect
machinery; tuning our own expressions can only move the 1.7% plus whatever
*call volume* our expressions drive into den.

Within our 13.9 M:

```
   1,210,466  capClosure   aspects/base/capability.nix:213
   1,065,114  <lambda>     aspects/base/capability.nix:309
   1,050,519  givenBy      aspects/base/capability.nix:17
   1,040,794  satisfies    aspects/base/capability.nix:223
   1,040,772  <lambda>     aspects/base/capability.nix:229
     844,804  <lambda>     aspects/base/capability.nix:216
     448,271  <lambda>     aspects/docs/_searchix-sources.nix:126
      70,397  <lambda>     aspects/docs/_searchix-sources.nix:159
      69,548  relativise   aspects/docs/_searchix-sources.nix:95
      68,891  isOurs       aspects/docs/_searchix-sources.nix:148
```

`capability.nix` is ~85% of our own calls, which is expected: `visibleFrom` and
`providersFor` filter `den.aspects` by `hasAspect`, so our call count is the
multiplier on den's 186 M. The entry that should not be there at all is
`aspects/docs/_searchix-sources.nix`.

## 2. The surprise: a secondpc rebuild evaluates the whole fleet

`aspects/services/searchix.nix:50`, inside the aspect's **`nixos` body**:

```nix
nixos =
  { pkgs, ... }:
  let
    optionSources = import ../docs/_searchix-sources.nix { inherit lib pkgs inputs; };
```

and `aspects/docs/_searchix-sources.nix:40`:

```nix
hostsOf = configs: lib.subtractLists excludeHosts (lib.attrNames configs);
nixosHosts = hostsOf self.nixosConfigurations;
darwinHosts = hostsOf self.darwinConfigurations;
…
homeTargets = … map (user: { options = homeOptionsFor cfg user; }) …
```

`homeOptionsFor` is a **fresh `lib.evalModules`** of the home-manager submodule
payload plus that host's definitions, per `(host, user)` pair.

`den.aspects.searchix` is included by exactly one host —
`aspects/hosts/secondpc/default.nix:205` — and `excludeHosts` defaults to
`[ "secondpc" ]`, which breaks the self-reference. It does not break the other
direction: **evaluating secondpc evaluates every other host in the fleet, plus
one extra home-manager `evalModules` per (host, user) pair.** The option trees
are the content of `options.json`, so they are genuinely forced, not merely
referenced.

That is consistent with everything else measured:

- `aspects/nixos/facter.nix:12` already says so in a comment — "(aspects/docs/
  _searchix-sources.nix) forces every option and trips over it".
- 1.11 M `hasAspect` calls is a fleet's worth of aspect resolution, not one
  host's.
- the evaluation makes **network fetches**, which a host evaluation has no
  business doing. From the run's stderr:

  ```
  fetching git input 'git+https://codeberg.org/blitz/gorgon.git'      (×3)
  fetching github input 'github:nix-community/gomod2nix/7f8d7438…'
  copying path '/nix/store/lfm7kb5…-source' from 'https://cache.nixos.org'
  fetching github input 'github:tomyun/base16-fish/86cbea4d…'
  copying path '/nix/store/kbjvsxn…-source' from 'https://nix-community.cachix.org'
  ```

  `gomod2nix` is an input of `searchix` (`partitions/nixos/flake.lock`);
  `base16-fish` is stylix's (`partitions/gui/flake.lock`) and belongs to another
  host's GUI; `gorgon` is in no lock this repo holds, so it is reached through a
  re-evaluated (patched or auto-unified) flake's own lock — and it is fetched
  **three times in one evaluation**, which is consistent with three unshared
  walks of the same lock graph (`lib/inputs.nix`'s `sharedTop` exists to make
  them one; `patchFlake`'s `backupNodes` builds its own `nodesFn` per entry).
  That attribution is an inference from the count, not a measurement.

### What to do about it

The tree is a derivation (`pkgs.runCommand "searchix-option-sources"`) served
over loopback by a static server whose root is that derivation, so the host
needs the derivation's *path*, which needs the evaluation. Rearranging the
expression cannot help; the dependency is real.

What removes it is making the option-source tree an out-of-band artefact:
`packages.searchix-option-sources` already exists (`aspects/docs/searchix.nix:11`)
and CI can build it. Have the service consume it as a **pinned output** — a
store path recorded in a generated file, exactly the arrangement
`patched-inputs.nix` and the layer plan already use for "this needs an
evaluation the build must not pay for" — or have searchix fetch it over HTTP
from somewhere that is not this host's own closure. Then a secondpc rebuild
evaluates secondpc.

Not implemented here: it changes what the running service serves, and the
before/after cannot be measured without the pinned file existing first.

## 3. `patchedInputs` and the two minutes to *list* the checks

**Yes, avoidable — and the measurement says the cost is not where the question
assumed.**

Measured, all three against the same flake, serialised, `--impure`:

| expression | wall | `cpuTime` | function calls | thunks |
| --- | --- | --- | --- | --- |
| `c: 1` (flake load, nothing forced) | 7.3 s | 8.7 | 17,393,284 | 17,208,364 |
| `attrNames c.flake.layerPlan` | **126.7 s** | 127.9 | 29,470,615 | 22,744,304 |
| `attrNames c.patchedInputs` | **135.5 s** | 134.7 | 31,339,537 | 24,700,055 |

So listing the names costs **+126 s** over loading the flake, and **+119 s of
that is `layerPlan` alone** — 94% of it. The user-reported ~1:59 reproduces.

One honest anomaly, because it changes what the number means: the *deterministic*
counters barely move. `layerPlan` adds only 12.1 M function calls, 5.5 M thunks
and 0.7 GB of allocation on top of the flake load, and **every I/O primop count
is byte-identical** (`readFile` 43, `fromJSON` 41, `fetchTree` 1518,
`derivationStrict` 770 — the same in all three runs). 12 M calls is about 12 s of
evaluation at the rate the secondpc profile implies (825 M calls / 845 s). The
other ~107 s is the process not evaluating: on a 16 GB box with two other agents
holding multi-gigabyte evaluations open, that is page pressure, and Lix's
`cpuTime` tracks elapsed time here (it equals wall in all three runs) rather than
CPU. **The 2 minutes is real and reproducible on this machine as used; the share
of it that is intrinsic to `layerPlan` rather than to contention is not
separated by these numbers.** The fix below removes the dependency either way.

Three further facts, in order of how much they settle the question.

**(a) A host evaluation does not pay it at all.** `LAYERS_MODULE_EVAL` appears
**zero** times in the 18-minute secondpc log. `config.flake.layerPlan` is never
forced by a host build, exactly as `aspects/framework/layers.nix:471` claims:
`flake.nix` reads the generated `patched-inputs.nix` and never runs the module
system to find out what is patched. So this is a cost on `nix flake check`,
`nix flake show`, the CI matrix and anything else that *lists* `checks.<system>`
— not on a rebuild.

**(b) The chain is real and it starts at the check's own predicate.**
`aspects/tooling/patch-inputs.nix:903`:

```nix
checks =
  lib.mapAttrs' (n: _: {
    name = "patched-input-${n}";
    value = (patchedDrvs pkgs).${n};
  }) (lib.filterAttrs (n: v: hasPatches v && (patchedDrvs pkgs) ? ${n}) config.patchedInputs)
```

Asking for the *names* of `checks.<system>` forces `config.patchedInputs`'s
attribute names and then the predicate per name. `config.patchedInputs`
(`patch-inputs.nix:695`) is `fromFile // fromAspects`, and `fromAspects` is

```nix
declared = lib.foldl' (a: b: a // b) { } (lib.attrValues config.flake.layerPlan);
```

so the names cannot be known without forcing `layerPlan`, whose own comment
(`layers.nix:416`) says what that costs: *"`evalDeclFn` is a full
`lib.evalModules` … 19 scopes worth of that is the bulk of what `layerPlan`
costs, and `layerPlan` is what makes `write-flake` slow."* Nothing about this is
avoidable **while the names come from the live declarations**.

**(c) The names do not have to come from the live declarations.** They are
already written down. `./patched-inputs.nix` is generated, committed, and read
by `flake.nix` itself before the module system can run; `recordedInputs`
(`patch-inputs.nix:253`) is that file as plain data, a `builtins.pathExists` and
an `import`. And `checks.patched-inputs-generated-current` already compares the
committed file with a freshly generated one **byte for byte**, so the file
agreeing with the declarations is an invariant the check set already enforces.

So the predicate can be rewritten against the recorded manifest plus lock data
only:

```nix
}) (lib.filterAttrs (n: v: (v.patches or [ ]) != [ ] && impl.mergedRaw ? ${n}) recordedInputs)
```

`impl.mergedRaw` is already exported by `lib/inputs.nix:750` and, per that
file's own header, *"reaches only `realInputs`, the layer locks and `ownerOf`,
and never forces `patchSpecs`"* — it is lock-file reads. It is also the exact
predicate `(patchedDrvs pkgs) ? ${n}` was standing in for: `patchedDrvs` is
`mapAttrs … (filterAttrs (_: hasPatches) patchedInputs)` over `applicableSpecs`,
and `applicableSpecs` (`lib/inputs.nix:387`) is
`filterAttrs (name: spec: (spec.isInput or true) -> (mergedRaw ? ${name}))`.

What this does and does not buy:

- the **name listing** stops forcing `layerPlan`;
- `nix flake check` still pays it once, because it instantiates
  `patched-input-<n>`'s *value*, which is `(patchedDrvs pkgs).${n}` and does go
  through `config.patchedInputs`. Removing that too means feeding `impl` from the
  recorded manifest the way `flake.nix` does — which is the right end state and
  is a change to what every host patches, so it is not in scope here.

Two semantic edges, both checked: nothing outside `patch-inputs.nix` sets
`prePatch`, `postPatch` or `isInput = false` (`grep` over `aspects/`), so the
recorded file's `patches`-only view loses nothing today; and a name present in
the declarations but missing from the file is precisely what
`patched-inputs-generated-current` fails on.

### NOT APPLIED, and why

The change is one line and the argument above is complete, but the before/after
could not be measured: while the two runs were queued, the working tree stopped
evaluating on an edit belonging to another branch —

```
error: dynamic attribute 'dyson' already defined at aspects/hosts/dyson.nix:514:3
       at aspects/hosts/dyson.nix:541:3
```

— so `nix eval .#checks.aarch64-darwin` fails for a reason unrelated to this,
and a number taken against a broken tree is not a number. The standard this repo
holds to is that a performance change ships with its measurement, so this one
waits for the tree.

**To apply and verify it, in one pass.** The before/after that matters is not
only the wall clock (contended, see the top of this document) but the
deterministic one: whether the layer plan is forced at all.

```
# BEFORE
TRACE_INPUTS=1 nix eval --impure --json .#checks.aarch64-darwin \
  --apply builtins.attrNames > /tmp/cb.out 2> /tmp/cb.err
grep -c LAYERS_MODULE_EVAL /tmp/cb.err     # expect 1

# apply the one-line predicate change in aspects/tooling/patch-inputs.nix

# AFTER
TRACE_INPUTS=1 nix eval --impure --json .#checks.aarch64-darwin \
  --apply builtins.attrNames > /tmp/ca.out 2> /tmp/ca.err
grep -c LAYERS_MODULE_EVAL /tmp/ca.err     # expect 0
diff <(jq -S . /tmp/cb.out) <(jq -S . /tmp/ca.out)   # expect no output
```

That last `diff` is the correctness half and matters more than the timing: the
set of check names must be **identical**, because the claim is that the recorded
manifest and the live declarations agree. If it is not identical,
`./patched-inputs.nix` is stale and `nix run .#write-flake` is the fix — which
is exactly what `patched-inputs-generated-current` would have said.

## 4. Duplicate capability resolution

**Retract the general form of the argument; keep five specific sites.**

Nix does not share a thunk between two call sites, so two textually identical
`capability.*` calls really are two evaluations. But the repo has far fewer of
them than the duplication argument implies.

Counted (non-comment lines, outside `aspects/base/capability*`):

```
grep -rnE 'capability\.[a-zA-Z]' --include='*.nix' aspects/ lib/ \
  | grep -v 'aspects/base/capability' | grep -vE ':[0-9]+:[[:space:]]*#' | wc -l
→ 95 call sites, 16 of them `contributions`
```

Of those 95, the ones that are the *same function applied to the same
arguments* — the only shape where a shared binding changes anything — are five:

| site | call |
| --- | --- |
| `aspects/network/roaming-resolver.nix:110` and `:125` | `capability.contributions peer "proxy"` — same `let` body |
| `aspects/services/paperless.nix:267` and `:345` | `capability.bindAddressFor host den.aspects.paperless "tcp" "http"` |
| `lib/service-type.nix:72` and `:103` | `capability.linkFor aspect "service" { … }` |
| `aspects/services/postgres.nix` ×2 | `capability.realise "postgres"` |
| `aspects/base/celler/push.nix` ×2 | `capability.resolveChain "celler"` |

The seven `contributions … "proxy"` sites are *not* seven duplicates: four are
comments or different hosts (`cloudflare.nix:879` and `proxy-nginx.nix:18` take
`host`, `roaming-resolver.nix` takes `peer`).

### What one resolution costs

The probe has to defeat two things: laziness, and the shared-thunk question
itself.

```nix
# /tmp/dN.nix, applied to .#debug.config
cfg:
let
  cap = cfg.den.lib.capability;
  h = cfg.den.hosts.x86_64-linux.secondpc;
in
builtins.stringLength (
  builtins.concatStringsSep "|" (
    builtins.genList (i: builtins.toJSON (cap.contributions h "proxy")) N
  )
)
```

`concatStringsSep` + `stringLength` rather than `builtins.length`: the first
attempt was `builtins.length (map (i: toJSON …) (range 1 N))`, and **`length`
does not force list elements**, so N=1 and N=20 produced byte-identical
counters and the measurement said nothing. `genList` applied to a body that
re-applies `cap.contributions h "proxy"` is a fresh thunk per `i` — which is
exactly what two real call sites are, since neither shares a thunk with the
other while both share the aspect registry underneath. That is what makes the
probe faithful rather than pessimistic.

Run against `.#debug.config` and not `builtins.getFlake`: `getFlake` on a dirty
tree costs ~85 s per invocation in tree copying, which swamped the first
attempt (91.28 s for N=1 against 92.63 s for N=20).

| | wall | `cpuTime` | function calls | thunks | `//` copied | allocated |
| --- | --- | --- | --- | --- | --- | --- |
| flake load (`c: 1`) | 7.3 s | 8.7 | 17,393,284 | 17,208,364 | 65,563,280 | 2,180 MB |
| N=1 | 8.2 s | 9.8 | 19,632,643 | 19,501,439 | 79,784,069 | 2,549 MB |
| N=20 | 9.3 s | 9.5 | 22,382,722 | 23,269,063 | 89,345,496 | 2,963 MB |
| N=40 | 9.9 s | 11.3 | 25,277,542 | 27,234,983 | 99,410,156 | 3,399 MB |

The deltas are exactly linear, so the per-resolution figure is not an estimate.
`(N=20 − N=1)/19` and `(N=40 − N=1)/39` agree to the unit:

| one repeated resolution | |
| --- | --- |
| function calls | **144,741** |
| thunks | 198,296 |
| values copied by `//` | 503,233 |
| allocation | 21.8 MB |
| wall | ~38 ms |

**144,741 calls is 0.0175% of the 825 M in a secondpc evaluation.** Eliminating
all five duplicate sites saves about 724 K calls — **0.09% of a host
evaluation**, call it two tenths of a second.

The first resolution is a different matter: `N=1 − flake load` is
**+2,239,359** function calls, fifteen times a repeat. That asymmetry is the
answer to the question underneath the duplication argument — the shared part
(`capClosure`, `givers`, `takers`, the capability registry) *is* shared across
call sites, and only the fold is redone. Which is why the fold is cheap.

**So: retract the duplication argument.** It is real, it is five sites, and it
is worth about 0.1% of an evaluation. `aspects/network/roaming-resolver.nix`'s
two calls in one `let` body are still worth binding once for readability, and
`lib/service-type.nix`'s `characterOf` recomputes `linkFor` per *field* rather
than per aspect, which is the one with a multiplier on it — but none of it is a
performance fix, and none of it is where the two minutes or the eighteen minutes
live.

## 5. Ranked, with expected savings

1. **`pathSetByScope`'s lambda-chain accumulator**, `<den>/nix/lib/aspects/fx/
   identity.nix:112`. 45.6% of all function calls and 175 GB of the 257 GB
   allocated in a secondpc evaluation; quadratic in resolved aspect nodes,
   forced 1.11 M times by `hasAspect`. Replacing the stored-lambda accumulator
   with a plain accumulated attrset would, on these numbers, remove most of
   both. **Not ours to fix** — `aspects/framework/den.nix` says why den is
   deliberately not patched in-tree (its own evaluation machinery; patching it
   is a bootstrap cycle), and `patches/den/*.patch` is the place such a change
   would be staged. Worth an upstream issue with this profile attached. The
   lever we *do* hold is the multiplier: 20 of our 38 `hasAspect` sites are
   `filterAttrs (_: a: host.hasAspect a) den.aspects`, a 291-wide scan each.
   *Expected saving: the largest available, and it is upstream.*

2. **`aspects/services/searchix.nix:50` evaluating the fleet inside secondpc's
   own config.** Makes one host evaluation a fleet evaluation plus a fresh
   `lib.evalModules` of the home-manager submodule per (host, user), and makes a
   host evaluation do network fetches. *Expected saving: most of the difference
   between secondpc and a comparable host without it; not separated here,
   because measuring it means a second 19-minute evaluation. The shape of the
   fix (build the option-source tree out of band, consume it as a pinned path)
   is in §2.*

3. **`checks.<system>` name listing forcing `layerPlan`.** 126 s of a 135 s
   listing, on a value already committed to `./patched-inputs.nix` and already
   checked byte-for-byte against the declarations. **Written up, not applied**
   — the one-line change and the two commands that verify it are in §3; the
   tree stopped evaluating before the before/after could be taken. The
   saving applies to `nix flake show`, `aspects/tooling/ci-matrix.nix`, editor
   tooling and `nix eval .#checks.<system> --apply builtins.attrNames`; `nix
   flake check` still pays it once because it instantiates the derivations.
   *Expected saving: ~2 minutes per listing.*

4. **`gorgon` fetched three times in one evaluation**, plus `gomod2nix` and
   `base16-fish`. A git input in none of this repo's locks, reached through a
   re-evaluated flake's own lock, fetched once per unshared walk of that lock
   graph. `lib/inputs.nix`'s `sharedTop` exists to make those walks one walk and
   evidently does not cover the `patchFlake` `backupNodes` path (`lib/inputs.nix:
   520`), which builds its own `nodesFn` per entry. *Expected saving: two of
   three network fetches on a cold fetcher cache; nothing on a warm one. Worth
   confirming with `TRACE_INPUTS=1` before touching.*

5. **`builtins.getFlake` on a dirty tree, in probes.** 85 s per invocation,
   against 7.3 s for `nix eval .#debug.config --apply`. Not a flake cost at all
   — a loop cost, and `../CLAUDE.md`'s fast-feedback section should say to use
   `--apply` and never `--expr` + `getFlake`. Note that `.#debug.config`'s
   `_module.args` has **no `lib`** (it has `capability`, `checkLib`, `den`,
   `ivixlib`, …), so an `--apply` probe is written in `builtins.*`.
   *Expected saving: 85 s per probe.*

6. **Five duplicate capability call sites.** 144,741 function calls each, 0.09%
   of a host evaluation in total. *Expected saving: ~0.2 s. Do it for
   readability, not for speed.*

### And one correctness finding the profiling turned up

`den.checks.cross-host-emissions-are-entity-only` (`aspects/base/capability.nix`)
cannot fire. It tests

```nix
bindsConfig = f: builtins.isFunction f && (builtins.functionArgs f) ? config;
```

and **of the 45 aspects in this tree with a non-null `gives`, zero are a bare
function** — every one is `ATTRS(__functionArgs,__functor)`, because den wraps a
declaration in a functor record. Measured:

```
$ nix eval --impure --json .#debug.config --apply 'cfg: …'
givesBareFunctionCount = 0
givesNonNullCount      = 45
```

So `builtins.isFunction` is false for every giver and the check is vacuously
green. It does not prove that no cross-host `gives` binds `config`; it proves
nothing. The fix is to read the arguments through the wrapper, which den makes
easy because it records them:

```nix
argsOf = v:
  if builtins.isFunction v then builtins.functionArgs v
  else if builtins.isAttrs v && builtins.isAttrs (v.__functionArgs or null) then v.__functionArgs
  else if builtins.isAttrs v && v ? __functor then  # emissions carry no __functionArgs
    (let r = builtins.tryEval (v.__functor v);
     in if r.success && builtins.isFunction r.value then builtins.functionArgs r.value else { })
  else { };
```

`aspects/framework/cross-host-dag.nix` uses exactly this and finds the
emissions the other check misses — `den.aspects.celler-server.provides.nixos`'s
`celler-caches`, whose `__functor` applied is `FN(config,host)`. Not fixed in
`capability.nix` because that file is being edited elsewhere.

### A thing to stop believing about `tryEval`

**`builtins.tryEval` catches `throw` and `assert`, and nothing else.** A type
error is not catchable. Measured twice while building the cycle check, from
policies handed a fabricated context: `error: expected a string but found null`
(`aspects/base/household-scope.nix:81`, `den.hosts.${system}`) and `error:
expected a set but found null`. Both went straight through `tryEval` and took
down the whole of `flake.checkFindings`.

That limits `aspects/framework/checks.nix`'s `probe` more than its comment
suggests: it converts a *throwing* check into a finding, and a check that
provokes a type error anywhere still takes the fleet-wide report with it. It
also means `tryEval (f x)` forces only the result's WHNF — a list whose elements
throw when read throws outside the `tryEval`. Both are why the cycle check
avoids calling a policy it cannot supply real arguments for, rather than calling
it and catching.

## 6. The cross-host cycle guard, and what it found

`aspects/framework/cross-host-dag.nix` declares
`den.checks.cross-host-config-dependence-is-acyclic`. It is here because it was
written alongside this profile and because what it found belongs with the
numbers.

It builds the directed graph of "host K resolves a config-dependent emission
sourced from host H" from the declarations — the `gives`/`reach` half and the
quirk/pipe half — and reports the mutual pairs and longer cycles. It needs no
host's fixpoint; the whole `checkFindings` set, with it in, answers in **51 s**.

Measured against the live fleet (11 hosts, 286 aspect entries including
`provides` children, 8 quirks):

| config-dependent quirk emission | hosts |
| --- | --- |
| `backup` — `den.aspects.audiobookshelf` | secondpc |
| `backup` — `den.aspects.homeassistant` | secondpc |
| `backup` — `den.aspects.immich` | secondpc |
| `backup` — `den.aspects.kanidm` | secondpc |
| `celler-caches` — `den.aspects.celler-server.provides.nixos` | **celler2, secondpc** |

Both quirks are collected fleet-wide (`pipe.collectAll (_: true)`, in
`aspects/services/restic.nix:182` and `aspects/base/celler/push.nix:277`), so
the four `backup` emissions give one-directional edges only — one source host —
while `celler-caches` has two source hosts and closes a cycle:

```
celler2 and secondpc depend on each other's CONFIGURATION: celler2 resolves
secondpc's `backup` (den.aspects.audiobookshelf), `celler-caches`
(den.aspects.celler-server.provides.nixos), `backup` (den.aspects.homeassistant),
`backup` (den.aspects.immich), `backup` (den.aspects.kanidm), and secondpc
resolves celler2's `celler-caches` (den.aspects.celler-server.provides.nixos).
```

So the expectation that this would be vacuously green was wrong, and it was
wrong for a reason worth knowing: the sibling check it rested on cannot fire
(see §5). The cycle is nevertheless **benign today**, and the evidence is in §1
— a full secondpc evaluation runs to completion, which an infinite recursion
would not. It is benign because `celler-caches` reads
`config.services.cellerd.expose` and nothing downstream of the pooled value
feeds `expose`; the check cannot see that, which is why it carries
`severity = "report"` and says so rather than failing anyone's build.

## Appendix: reproducing each figure

```
# §1 the host profile (18 m, 13.75 GB — hold the lock)
NIX_SHOW_STATS=1 NIX_COUNT_CALLS=1 NIX_SHOW_STATS_PATH=/tmp/secondpc.json \
  nix eval --raw .#nixosConfigurations.secondpc.config.system.build.toplevel.drvPath

# then, for the per-position table: dedupe by (file,line), NIX_COUNT_CALLS emits
# one record per column
jq -r '.functions | group_by(.file+":"+(.line|tostring))
       | map({k:(.[0].name // "<lambda>"), f:.[0].file, l:.[0].line, c:(map(.count)|max)})
       | sort_by(-.c) | .[:25][] | "\(.c)\t\(.k)\t\(.f):\(.l)"' /tmp/secondpc.json

# §2 that a host evaluation fetches over the network, and what
grep -E '^(fetching|copying)' <that run's stderr>

# §3 the layer plan, and that a host build never forces it
nix eval --impure --json .#debug.config --apply 'c: 1'                                    #  7.3 s
nix eval --impure --json .#debug.config --apply 'c: builtins.length (builtins.attrNames c.flake.layerPlan)'   # 126.7 s
nix eval --impure --json .#debug.config --apply 'c: builtins.length (builtins.attrNames c.patchedInputs)'     # 135.5 s
grep -c LAYERS_MODULE_EVAL <host eval stderr>                                             #  0

# §4 one capability resolution, N=1 vs N=20 vs N=40 (see the probe in §4)
nix eval --impure --json .#debug.config --apply "$(cat dN.nix)"

# the cycle check
nix eval --json .#checkFindings | jq '."cross-host-config-dependence-is-acyclic"'
```

Every one of these must be run behind the lock — `../CLAUDE.md`, and §1's
13.75 GB is why.
