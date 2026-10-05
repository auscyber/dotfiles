# Capability resolution graphs

The host aspect graph with the **capability layer in it**: for every take an
aspect on the host declares, which giver answered, over which carrier, and which
candidates lost with the clause they lost on.

Regenerate everything here with one command:

```
./docs/capability-graphs/regen.sh
```

## What is in the picture

| Shape / style | Meaning |
| --- | --- |
| plain rectangle | an aspect — a node the host's own graph already had |
| **hexagon** | a take: `<capability> <clauses>` |
| **trapezoid** | the giver that won, on the host it is on |
| dashed red border, `-.-x` edge | a candidate that lost. The node's label is the reason |
| `-.->|lan-home 20 (auspc to secondpc)|` | the carrier that won and its `preference` |
| `-.->|loopback 0 (same host)|` | resolved without crossing a machine |
| `-.->|not routed|` | the capability carries no address, so there is no carrier |
| subgraph per host | the giver is on another box |

The reason a candidate lost is in its **node** label rather than on the edge, and
that is forced: den-diagram's mermaid renderer emits an edge label only for
`style = "provide"`, and its DOT renderer emits one only for `"replaced"`. So
`.mmd` is the primary format here — it is also the one that renders in a browser
and in most markdown viewers with no toolchain at all, which matters more while
debugging than DOT's better layout does.

Everything a losing candidate says comes from `capability.judge`, the same
verdict list `capability.explainTake` folds into a throw. The `*-resolutions.md`
files print that table beside each take so the picture and the resolver cannot
disagree.

## Files

| File | What it shows |
| --- | --- |
| `secondpc-capabilities.mmd` | every take on secondpc (≈50) — the service box |
| `auspc-capabilities.mmd` | every take on auspc (10) — the desktop that consumes the house |
| `auspc-full.mmd` | the **whole** auspc aspect graph with the capability layer overlaid |
| `*-resolutions.md` | the same answers as text, with `explainTake`'s tables |
| `example-*.mmd` | one take each — the worked cases below |

## The worked examples

**`example-openrgb-home-assistant.mmd`** — a cross-host take.
`den.aspects.openrgb` sits on auspc and declares
`takes.home-assistant.extraComponents = [ "openrgb" ]`; Home Assistant is on
secondpc with `reach = "household"`. The graph crosses into secondpc's subgraph
and the edge says `not routed`, because `home-assistant` furnishes no address —
it is a contribution channel, not a wire. Nothing lost, so there is nothing
crossed out.

**`example-vban-udp.mmd`** — the case the losers exist for. `vban-turntable`
declares `takes.udp = { name = "music-assistant"; mtu = m: m >= 1500; }`. Two
things are crossed out:

- `tailscale 40 — mtu is 1280, which this flow refuses`. The tunnel is in reach
  and loses **on the numbers**, which is the whole reason the aspect carries a
  throw rather than a fallback.
- `19 other instances — wrong instance: want music-assistant`. Folded
  deliberately: the WHO stage rejects nineteen candidates and all nineteen say
  `is named <host>, want music-assistant`. That is the resolver picking an
  instance out of every loopback in the fleet — a search, not a decision — and
  drawing nineteen boxes for it buries the one rejection that *is* a decision.

The winner is `lan-home` at preference 20, address `192.168.0.26`.

**`example-corsair-mqtt.mmd`** and **`example-mqtt-reach-whatif.mmd`** — the
same take, before and after. `mosquitto` is `reach = "household"` today, so
`corsair-psu`'s `takes.mqtt` resolves from auspc over the lan. The what-if
renders the same take with mosquitto pretended back to `reach = "host"`, which
is what it was until recently: the request node goes dashed, the `takes` edge
goes crossed, and the only thing attached is
`mosquitto — out of reach (what-if): reach = host`. That is the shape of a reach
failure, which otherwise shows up as `dial` returning `null` with nothing to
look at.

**`example-homeassistant-mqtt.mmd`** — a same-host take, for contrast.
`homeassistant` takes `mqtt` and mosquitto is on the same box, so the edge reads
`loopback 0 (same host)` and no host boundary is crossed. Visibly a different
kind of answer from a wire.

**`example-paperwork-share-unresolved.mmd`** — an unresolved take that is
*correct*. `paperwork` is a person aspect delivered to every machine the person
has an account on; `shares` is `reach = "host"` and lives on secondpc. From
auspc the take resolves to nothing, and the graph says why:
`shares on secondpc — out of reach: reach = host`. On secondpc the same take
resolves (`not routed`, to `shares`) — the pair is the clearest illustration
here that an unresolved take is a question about the (host, take) pair and not
about the declaration.

## Cost

Measured, flake-level only — no NixOS configuration is built and no host
fixpoint is forced, so **none of this needs the eval lock**:

| Query | Cost |
| --- | --- |
| `.#debug.config` baseline (nothing asked) | ~6.5 s |
| all 10 resolutions on auspc (`view = "report"`) | ~7.9 s |
| one focused example (`view = "slice"`) | ~7 s |
| a host graph via `den.lib.capture` + the overlay | ~8 s |
| the whole `regen.sh` run (13 files, 13 evals) | ~53 s |

For reference, `nix eval --json .#checkFindings` runs five fleet-wide checks in
about 15 s. The capability layer is in that class, not the host-eval class:
`hasAspect` is a projected lookup and a giver's `gives` is a plain function, so
nothing here needs a host's fixpoint. The resolution overlay could live in
`nix flake check`.

The one expensive query is deliberately lazy: the out-of-reach set walks
`providersFor` across the whole fleet, and it is only computed for a take that
resolved to nothing.

## Why this is a script and not an app

`perSystem.apps.capability-graphs` in `../../aspects/docs/default.nix` is the
real producer and renders through `inputs.den-diagram`. **That input is declared
and locked nowhere.** `den.aspects.docs.inputs.den-diagram.url` is set, but
`aspects/framework/layers.nix` gathers `inputs`-class declarations from a fleet
capture, and nothing in the tree includes `den.aspects.docs` — so the aspect is
never traced and its input never reaches `partitions/dev/flake.nix`. Checked
against the generator's own output rather than guessed:
`flake.layerFlakeText.dev` does not mention it and `flake.layerPlan` places it
in no layer, root included.

So `aspects/docs/` as a whole is currently un-evaluable, including the
pre-existing `apps.docs` and `checks.docs-diagrams`. `render.nix` here pulls
den-diagram as a flake of its own (pinned) and reads everything else out of
`.#debug.config`, which is where `_module.args.capabilityGraph` lives. Retire it
once the input is locked and `apps.capability-graphs` works.
