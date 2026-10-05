**And the secret lives in the TAKER's scope**, not a third one named after the
edge. That is forced rather than stylistic: `donetick.nix` renders its whole
config as a template interpolating three secrets, and a template may not depend
on another scope's secret (`age.templates` → `age.scoped.<other>.access` →
`age.templates`, which surfaces only at `gen-secrets`). Keeping the credential
in the taker's scope keeps every template dependency local — and it is what the
tree already did: `nextcloud.nix` holds both materialisations in `nextcloud`'s
scope.

So the taker's copy keeps the **plain name** its template already reads
(`placeholders.db-password`) and carries the generator; the giver's is the
suffixed sibling inheriting its `rekeyFile`. `donetick.nix` therefore declares no
`db-password` at all — `postgres.nix` installs both entries into donetick's
scope, and the giver reads across scopes, which is a plain read and does not
cycle.

# Capabilities

Written before evaluating, because a full-host eval on this box needs several GB
and gets OOM-killed silently (`CLAUDE.md`). **Nothing in this document has been
evaluated.** Treat every claim below as a design statement, not a measurement,
unless it is marked otherwise.

This replaces `den.serviceTypes` / `den.services`. The research finding that
motivated it: den already does pooling (quirks), provenance
(`pipe.withProvenance`), class routing (`policy.route`), arg injection (ctx) and
cross-host carriage (`pipe.broadcast`). The only thing genuinely absent is
*capability → provider resolution*, which is roughly an eighth of the ~4,800
lines the registry grew to.

## What a capability is

`den.capabilities.<name>` is a **contract**: the options a giver must furnish.
It has no identity, no instances and no scope, which is why it is a submodule
and not `den.schema.<kind>` — a schema definition carrying options would make
den *compute* `isEntity` (`den/modules/options.nix:125-141`), silently dropping
`provides.<kind>` and making any bare `{ tcp, ... }` arg inert where it cannot
bind.

**Capabilities compose by `includes`, the same verb aspects use.** An address is
not a field of `tcp`; it is a capability `tcp` narrows. Three things follow:

- `unix` cannot satisfy a `tcp` want, because it does not include `address`.
  Structural, not "its option set happens to lack a key" — so *this database
  must never be borrowed across machines* is unrepresentable rather than
  guarded.
- A **name is askable on its own**. The lan gives no `hostname` at all, so a
  want for one finds *no provider* over the lan rather than a null.
- A carrier can give a name without giving a connection, which is what the
  public gateway does.

| capability | narrows | furnishes |
|---|---|---|
| `carried` | — | `carrier` |
| `address` | `carried` | `address`, `mtu`, `maxBodyBytes` |
| `hostname` | `carried` | `hostname`, `aliases`, `domain` |
| `segment` | — | `prefix` |
| `user` | `carried` | `name`, `group`, `mode` |
| `credential` | `carried` | `scope`, `secretName`, `sides` |
| `oidc` | `credential` | `clientId`, `issuer` |
| `tcp` / `udp` | `address` | — |
| `unix` | `carried` | `socketDir` |

## Hostnames

Each carrier furnishes the names it actually has:

| carrier | `tcp.address` | `hostname` |
|---|---|---|
| loopback | `127.0.0.1` | `localhost` |
| lan-home | `host.lanAddress` | **none** |
| tailscale | the MagicDNS name | `<host>.tailef3db9.ts.net` + bare `<host>` |

The lan's absence is the finding, not an omission: nothing serves a zone for
that segment. §2.7a's dnsmasq `svc.internal` zone is what would fill it.

`capability.namesFor host peer` is the single query that four hand-rolled
answers are each a special case of — `ssh-config.nix`'s known_hosts walk (alias
*and* tunnel address, "because ssh checks known_hosts against whatever it
actually connected to"), `builders.nix`'s `hostName or ipAddress`, `fleet.nix`'s
`faggot.sh`, and the four MagicDNS literals that used to live in
`aspects/network/tailnet.nix`.

**`aspects/network/tailnet.nix` is deleted.** Every fact in it was already on
`den.aspects.tailscale`, and the four names are now derived from the peer. Its
own comment argued writing them out was "cheaper than a mechanism that exists
for one subtype" — true of the registry it lived in, false once the mechanism is
one line that cannot go stale when a host is added or renamed.

## Finding a link

Two questions, and conflating them would have broken kanidm across hosts.

**Routed** — `linksBetween cap A B`. Both ends known. Intersect the carriers each
is attached to, drop non-segments, order by `preference` ascending.

A loopback is not a segment: every host has one and no two are connected, so
matching on `tcp` alone would make any pair "reachable over loopback". A segment
says it is one by *giving* the `segment` capability — which is now a capability
question rather than a probe for a `prefix` field.

**Service** — `resolveFrom host cap take`. The provider is *discovered*: walk the
givers `reach` makes visible, each furnishing its link **for the asker**
(`peer = host`), which is what makes "a database named after whoever asked" work.

`isRouted cap` is the branch, and it is `lib.elem "address" (capClosure cap)`.
Running the segment filter over `oidc` would reject every cross-host provider,
since no service aspect gives `segment`; running `reach` over `tcp` would accept
a peer with no path to it.

### Preference is order, not truth

Both return the **whole candidate list**. The candidate set is a fact; only the
order is judgement, and `laptop.nix`'s `celler-use.<server>.via` is standing
evidence that a single ordered default gets overridden by every real caller.

`preference` is ascending **depth of layering**, not speed: loopback 0, lan 20,
tailscale 40. A longer route *acquires dependencies the call did not have* —
`nextcloud.nix:327` scrapes over loopback so metrics do not depend on "DNS, TLS
and the tunnel being up", and `sso.nix:385` needed explicit `after`/`wants` on
nginx *and* kanidm purely because a same-host call took the long way round.

This is deliberately not the semantics den refuses. `_types.nix:40-59` declines
first-match-wins for *entity existence*, and is right: existence cannot answer
the question a first-match lookup needs to ask. A carrier is different — the
ends either share it or they do not.

## Takes, and filtering on characteristics

`takes.<cap> = { … }` is the consumer half, and what is inside it **filters what
the giver furnished** rather than pushing parameters at it. That direction is
"ask, don't announce" applied to resolution.

```nix
den.aspects.grafana.takes.postgres = { over = "unix"; };
den.aspects.vban-turntable.takes.udp = { mtu = m: m >= 1500; };
```

- a **value** compares against the same key on the link
- a **function** is a predicate, for anything ordered
- `carrier` / `sameHost` are edge facts, read from the candidate
- `where` takes the whole candidate — the escape hatch, because a closed filter
  language forces a fork the first time something needs an unanticipated shape

**Max fiber size is a carrier fact.** `mtu` and `maxBodyBytes` are furnished, so
the 1420/1280 MTU floors and Cloudflare's 100MB body cap become numbers a take
compares against instead of prose in two files. Both have been got wrong once:
`vban.nix` refuses wireguard in a comment, and `nextcloud.nix:433` sets
`maxUploadSize = "10G"` on a vhost that is *not* in the tunnel's exclude list.

## It is all a module

`gives`, `takes`, `preference` and `reach` are typed options on
`den.schema.aspect`, which den imports into every aspect submodule
(`nix/lib/aspects/types.nix:895`). Already the house mechanism —
`framework/roles.nix`, `framework/layers.nix`, `security/age-scope.nix` all use
it.

Plus `den.reservedKeys = [ "gives" "takes" "preference" "reach" ]`, because
`fx/key-classification.nix` treats an unrecognised attrset-of-attrsets as class
content and `takes.postgres.database` has exactly that shape.

Why typed and not freeform: an aspect's freeform keys accept anything, so a
misspelled `give` is not an error — it is a key nothing reads, and the aspect
silently gives nothing. Same invisibility that `strict = false` on the old
instance type produced, where a removed option left `endpoints.http.class`
evaluating happily as untyped data.

`reach` defaults to **`host`** deliberately: the safe answer is "do not reach
across machines", so being shared is opted into rather than discovered.

## A credential belongs to the edge

Five hand-rolled shared secrets are one concept:

| edge | the shared thing | today |
|---|---|---|
| app → kanidm | OIDC client secret | `basicSecretFile`, name agreed by both files |
| app → postgres | role password | a role-password unit + each consumer's `passwordFile` |
| nextcloud ↔ kanidm | one `.age`, two readers | one source, `kanidm` 0400 + `nextcloud` 0400 |
| consumer → celler | RS256 signing key | declared as `intermediary` on both sides, deduped by file |
| consumer → garage | S3 access key | lives in the consumer's scope, read by a oneshot |

**A credential has two sides and one file, and the names of the two sides are
what the permissions are made of.** One `.age` source materialised twice, each
copy owned by the unix user *that side furnishes*, each 0400. Neither side gets
a copy that can drift, because there is one source; neither can read the
other's, because the owners differ.

There is **no `sides` field**. An earlier version had one, naming the two
aspects — which restated what resolution already knows, since a credential only
exists because some aspect gave it to some peer. Which side is the giver is an
implementation fact of `capability.nix`, holding `self` and `peer` at the moment
it calls `gives`.

**Ownership is composed, not defined.** The credential type does not know what an
owner is; it asks the narrower `user` capability that does, through its own
`realise`. And `realise` is a generic field on *every* capability — handed
`{ giver, taker, link, resolve, … }` — so the resolver dispatches and knows
nothing about credentials. The version before this had a `materialise` function
for credentials sitting in the resolver, which would have grown a second such
function for the next capability that needed one.

**The implementation details live in the definition, and the scope is created at
the implement place.** `credential.realise` returns the two accounts *and the two
`age.scoped` entries* — one source with a generator owned by the giver, and a
sibling inheriting its `rekeyFile` owned by the taker. The implementing aspect
only installs them:

```nix
age.scoped = mapAttrs' (_: r: nameValuePair r.scope { inherit (r) secrets; }) (realisedFor host);
```

Written as `age.scoped.<scope>` rather than through the aspect's `secrets`
shorthand because that shorthand is one scope per *aspect*
(`age-scoped.nix`'s `secretScope`) and there is one scope per *asker*.

`nextcloud.nix` is this built by hand and is the evidence for the shape. It got
there by first manufacturing a unix **group** (`nextcloud-oidc`) holding both
accounts — defended in its own comment as better than "either side getting a copy
that can drift" — and the group went once two owners worked. agenix-rekey is fine
with it: one age file, two services on one host, deduplicated by file.

**It dissolves the cross-scope template cycle rather than guarding it.**
`age.templates` → `age.scoped.<other>.access` → `age.templates` only closes when
a credential belongs to two parties and is stored under one of them. With the
edge owning the scope, both ends reference one scope and the cycle has nothing to
close around.

## Mutual configuration needs the class, not the query

A capability answers *which provider, how reached, what credential*. It writes
nothing. But a relation has config at **both** ends — kanidm must register a
client for every service that wants one — so the pairing is:

- the **capability** supplies the key and what the consumer reads
- a **forwarded class** supplies the config, keyed by that edge

`kanidm.nix` is the first case: `kanidmClients = genAttrs (oidcTakers host) …`.
Written at **leaf depth** (`[services kanidm provision systems oauth2]`), because
den class content merges one level deep — forwarding one level shallower is what
silently deleted three of four oauth2 clients once already.

`postgres` still has its quirk doing this half; migrating it moves all five
consumers at once, not "beside".

## Depend on the smallest fixpoint that answers the question

Measured across the 146 aspect files that declare a class body:

```
lines from the first class body to EOF : 17190
of those, lines touching `config.`     :   303
```

So ~98% of what sits inside class bodies does not need the host fixpoint at all.
That is the case for hoisting, and it is stronger than expected.

Two separate rules, and the first was initially stated wrong:

**Flake-level `_module.args` reach an aspect FILE's arguments, not a class
body's.** `capability`, `fleet`, `serviceLibs` are file-level. Nothing in the
tree destructures `fleet` inside a `nixos = { … }`, and `base/builders.nix` is
the pattern: `collectBuilders` is computed at file level and the class body
closes over it.

**And the dependency that remains is not always the host's fixpoint.** `scoped`
is a *per-scope* view (`lib/age-scoped.nix`), so reading
`scoped.<scope>.secrets.<k>.path` forces that scope's submodule and nothing
more, where `config.services.<x>` forces the machine. Those are different sizes
of fixpoint and only the second is expensive. So the rule is not "get out of the
body" but **depend on the smallest fixpoint that answers the question**.

Classified over the same 146 files:

| registry read | files |
|---|---|
| only at file level | 106 |
| both file level and inside a class body | 36 |
| only inside a class body | 2 (`tooling/nixd.nix`, `editors/lspmux/lspmux.nix`) |

So registry reads are mostly already hoisted; the 36 "both" files are where the
hoist candidates sit.

## Takes have two halves, and conflating them was a real flaw

Found while migrating the postgres consumers. A take clause is either:

- a **filter** on a key the giver *furnishes* — `over = "unix"` narrows the
  candidates
- a **contribution** of a key the giver *consumes* — `extensions = [ "pg_cron" ]`
  flows the other way

The first version treated every key as a filter, so `extensions` would have been
compared against a key no link carries, matched nothing, and resolved to **no
provider**. Not a type error — an empty resolution.

So the capability definition declares both sets, and each contribution declares
how it merges. All three disciplines are live in the quirk today:

| discipline | example | rule |
|---|---|---|
| `each` | `services`, `database` | one entry per taker, keyed by taker |
| `union` | `extensions`, `preload` | `unique` of everything asked for |
| `exclusive` | `settings."cron.database_name"` | **throw** if two takers disagree |

The third cannot be a merge. `shared_preload_libraries` is one comma-joined
string and `cron.database_name` names exactly one database, so a silent
last-writer-wins is a database running someone else's cron jobs.

A key in neither set **throws**, naming what the capability furnishes and what it
accepts. Silence there is what `strict = false` produced on the old instance
type, where a removed option left `endpoints.http.class` evaluating as untyped
data and a stale `transports` block sat on auspc's entity unnoticed.

`capability.contributions host cap` is the generic fold — the postgres quirk's
collector, with the disciplines read from the definition rather than written into
the collector.

## A side is something a class body can read

`realise` gives each end its own view, and the consumer names nothing:

```nix
services.donetick.database.passwordFile = postgres.side.pathIn scoped;
```

`pathIn` is a *function* of `scoped` rather than a path, because a deployed path
needs the scope's fixpoint while the side is computed outside it. So the side is
flake-level data that becomes a path the moment a class body applies it. Neither
end names the scope, the entry, or the owner.

## The asker's name is its account, not its aspect

`homeassistant` asks for postgres and must get `hass`. The aspect name is wrong,
and peer authentication is why it matters: the role has to equal the connecting
unix user or the socket rejects it.

So `gives` is handed `resolve` as well, and `gives.postgres` resolves the asker's
`user` capability for the database and role name. `grafana.nix` had the same fact
written twice as `dbName`; `state.nix` had `hass` as two literals.

This makes `gives.user` load-bearing: a service that is asked for anything named
after the asker needs to furnish the account it runs as.

## The postgres migration

Seven consumers off the quirk, all at once rather than beside it:

| consumer | was | now |
|---|---|---|
| nextcloud | `database`, `owner`, `services` | `takes.postgres.services` |
| vikunja | same | `takes.postgres.services` |
| immich | same | `takes.postgres.services` |
| grafana | `dbName` twice | `takes.postgres.services` |
| homeassistant | `hass` twice | `takes.postgres.services` |
| donetick | + `tcp`, `passwordFile` | `tcp = true` |
| splitpro | + `extensions`, `preload`, `settings` | contributions, unchanged in meaning |

`database` and `owner` are gone from all seven — they were literals equal to the
asker's own account, restated once for the quirk that creates the database and
once for the app that connects, with nothing checking the two agreed.

**`passwordFile` is gone too**, because `postgres` now narrows `credential`: the
role password belongs to the edge, minted into `postgres-<account>` and
materialised once per side. Only a TCP taker gets one — `gives.postgres` reads
`peer.takes.postgres.tcp` and furnishes the credential fields only then, so
nothing is minted for the five consumers that use peer auth and need no secret.

**Not finished, and it is the next real step.** `donetick.nix` and `splitpro.nix`
still declare a `db-password` in their *own* scopes, and splitpro's `DATABASE_URL`
template interpolates `placeholders.db-password`. Moving the secret to the edge's
scope without moving that template would recreate the cross-scope template cycle
(`age.templates` → `age.scoped.<other>.access` → `age.templates`, which surfaces
only at `gen-secrets`). The template has to move into the edge's scope with the
secret — which is the same reasoning that says the edge scope *dissolves* that
cycle rather than guarding against it.

## Getting rid of `serviceTypes`

Five of the seven converted. Each one's fields were already aspect facts:

| file | was | now |
|---|---|---|
| `postgres.nix` | `gives.postgres`, `uses.unix`, `reach` | typed fields on the aspect |
| `postgres.nix` | `pgbouncer` | **dropped** — nothing instantiated it |
| `nextcloud.nix` | `gives`, `reach`, `uses.postgres` | `reach = "household"`, `gives.user`, `takes.postgres` |
| `homeassistant/core.nix` | `uses.tcp.shared.port`, `uses.udp.shared` | `gives.tcp`/`gives.udp` with `expose` |
| `homeassistant/music-assistant.nix` | `gives`, port, `uses.home-assistant.giving` | `gives.tcp`, `takes.home-assistant.extraComponents` |
| `connection.nix` (354 lines) | the segment registry | **deleted** — `segments.nix` + `tailscale.nix` |

`uses.tcp.shared.port` was the wrong direction: a port is what a service
*offers*, not what it asks for. So it is a `gives`, and the port lives on the
link because the server listens on it and the consumer dials it.

**Still there: `celler-service.nix`**, and the blocker is specific.
`celler-cache-pull` has a `nixos` body taking `{ http, cache, ... }` — resolved
capabilities delivered as class args — and that delivery does not exist. §3a
measured why it is not simply "resolve the aspect in a child scope": every
service aspect includes `gateway`, so a second scope double-declares the shared
`kanidmProvision` forward (`den.fwd."provision/nixos/services/kanidm/provision"
is already declared`). Structural, not a kanidm detail.

Nor can it be `_module.args.<cap>` at host scope, because the link differs per
asker: `gives.postgres` reads `peer.takes.postgres.tcp`, so donetick resolves
`over = "tcp"` while grafana resolves `over = "unix"` on the same host. One arg
cannot be both.

So delivery is the next real design decision, and the options are the three §3a
named plus the one it did not: a per-aspect shim resolved in its own scope, an
aspect-keyed class, host-nested service entities, or passing the resolution
explicitly at the one call site that needs it.

## `port` is an alias, and the types are now enforced

`port` routes into `ports` as a singleton, so a giver writes whichever is natural
and every consumer reads one list — no branch on which spelling was used.

That works because furnished values now go through a **real submodule**.
Previously `linkFor` returned whatever the giver's function returned and nothing
checked it: the `options` were declarative prose. A giver that misspells a field
or furnishes a string where an int belongs now fails at the giver, with the
option system's own message.

`capOptions cap` is the transitive option set — giving `tcp` means furnishing
`address`'s options too, since tcp narrows it.

## Firewall rules are derived

`aspects/base/firewall.nix`, included via `den.default`, opens exactly the ports
services declare as exposed. It reads the *same* links a consumer resolves, so an
open port is necessarily a port something gives — the invariant the hand-written
lists cannot hold, since `secondpc/default.nix` opens ports no service
declaration mentions.

Additive for now: `allowedTCPPorts` is a list option, so the derived set merges
with every existing literal rather than replacing it. That makes it safe to land
before the literals come out.

**One contradiction surfaced and was deliberately preserved.** The old
`den.serviceTypes.home-assistant` said 8123 stays closed because "opening it
would route around the gate", while the live `nixos` body in the same file has
always had `allowedTCPPorts = [ haPort homekitPort ]`. The declaration and the
config disagreed and the config won, because it is the half that runs. Closing
8123 has a real blast radius, so it should be a deliberate change rather than a
refactor side effect.

## A link is composed from three things

This was the biggest structural gap, and it surfaced from asking whether the
`http` carrier works with gateway/nginx. It did not, for reasons worth keeping:

A **carrier** knows the address and the transport facts — mtu, body cap, segment
prefix, DNS zone. An **endpoint** knows the port, the scheme, and which name it
is published under. `celler`'s `gives.http` furnishes a port and a protocol and
*no address*, correctly — a service has no address; it is reached over whichever
carrier the two ends share. Typed against the full option set that was a link
missing required fields: a type error for a value that was never wrong.

So capabilities declare `side = "carrier" | "endpoint" | "either"`, the halves
are typed separately, and `composeLink` joins them.

**And the third thing is the published name, which is orthogonal to the route.**

> the laptop can reach cache.ivymect.in but it could access cache.ivymect.in
> over dns through tailscale

So a link carries `address` — what you **dial** — and `hostname` — what you
**present**. The laptop dials secondpc over the tailnet and sends
`cache.ivymect.in` as the Host, which is what nginx matches the vhost on and
what the certificate is issued for. `secondpc/sso.nix` already does this in the
other direction: `networking.hosts."127.0.0.1" = [ "auth.<zone>" ]` pins the
public name to loopback, because kanidm checks the browser's origin exactly
while the traffic has no business leaving the box.

That is why `hostname` and `address` are separate capabilities rather than one
overloaded field, and the name is attached to **every** candidate — one name,
many routes.

### Narrowing runs one way, so the carrier is asked for the base

Giving `http` gives `tcp`; giving `tcp` does not give `http`. `segments.nix`
gives `tcp`, because a wire carries streams and knows nothing about schemes. So
`carrierCapFor` asks the carrier for the routed base while the endpoint is asked
at the narrow capability. Asking the carrier for `http` finds nothing, which is
how this first failed.

A related bug: the carrier is *found* at `tcp` but cloudflare furnishes `http`,
so typing against the requested capability rejected `protocol` as unknown.
`linkRaw` now returns the matched capability and each half is typed against
that.

## A zone is a carrier

`gateway.domain` is an `os`-class option, so it lives in a host's evaluated
config — reachable from another host only through
`inputs.self.nixosConfigurations.<other>.config`, the coupling only
`celler/server.nix` does deliberately. The question is cross-host by
construction ("where do I reach a service published by that box"), so the answer
cannot live there.

It was briefly `publicZone` on the host entity, beside `lanAddress`, on the
reasoning that both are facts nothing can derive. **That is true of an address
and false of a zone**: a zone is a thing several hosts can attach to, which is a
carrier. So `den.aspects.publish-ivymect-in` carries `domain`, gives the public
`http` with Cloudflare's cap, and **sets `gateway.domain` through its own class
content** — so including it both makes a host publish under the zone and tells
the gateway which zone that is. One source, nothing to drift.

`prefix` and `domain` are now typed on `den.schema.aspect` too. They were
freeform keys on `segments.nix` and `tailscale.nix`, so a misspelled `domian`
was a key nothing read and a carrier that silently addressed nothing.

### Attachment is one-sided for a published route

A **segment** needs both ends: two hosts are siblings on the lan because each is
cabled to it. A **published route** needs only the serving end — the laptop
reaches `cache.ivymect.in` from a cafe without publishing anything, so requiring
it to include the zone would make the public route resolve for nobody.

`attachment = "both" | "provider"` on the aspect, which is §2.7's `inboundOnly`
as a membership rule rather than a flag a resolver must remember to check.

## The carrier is derived at the point of use

> its just a connection to the cache needs some transport, and on usage, it
> derives what carrier it is using

So the two hand-written carrier choices are gone, and the derivation lands where
they did:

| was | why it was right | now |
|---|---|---|
| `laptop.nix`'s `via = "tailscale"` | no `lanAddress`, and push exceeds the 100MB cap | derived: lan has no address, cloudflare fails `maxBodyBytes` |
| `server.nix`'s `consumeVia = "tailscale"` | same cap argument, written out in prose | kept **only as the fallback** for a scope with no host binding |

`consumeVia` stays `tailscale` rather than reverting to the `virtualHost`
default precisely because a fallback must not be the public route. The prose
that justified it *is* the derivation now — `maxBodyBytes` on the carrier
against `maxBodyBytes = b: b == null` on the push flow — so the two agree by
construction instead of by someone having worked it out once.

**And push and pull resolve separately**, which is the whole reason they are
separate aspects: `urls` (substituters) uses `pullTake`, the celler client
config uses `pushTake`. The old `celler-use` had `pull`/`push` split per server
while `via` was one value shared by both, and `cloudflared-tunnel.exclude`
holding `cache.ivymect.in` is that gap patched by hand, one host at a time.

## Different carriers for each direction

> this does mean we can use different ones for each direction

Yes, and it showed that the body cap was being **restated** per flow. So
capabilities now carry `rules` — constraints the capability imposes on a
composed link, independent of any take — and the body cap is one:

```nix
address.rules = [
  ({ carrier, take, ... }:
    if carrier.maxBodyBytes != null && take.direction == "out"
    then "caps request bodies at … bytes, and this flow sends them" else null)
];
```

`celler-cache-push` is now just `takes.http.direction = "out"`. It says what it
**does**; the constraint is derived. Before it said `maxBodyBytes = b: b == null`
— correct, and a thing every future uploading flow would have to remember, which
is exactly how `cache.ivymect.in` ended up in `cloudflared-tunnel.exclude` while
`cloud.ivymect.in` did not.

**`direction` is a taker option, not a furnished one**, and the distinction is
load-bearing: direction is a property of the *flow*, not of what the giver
offers — the same endpoint is uploaded to and downloaded from. As a furnished
option it would be filtered on, compared against the link, and reject
candidates.

So for one cache and one name: push resolves to the tailnet, pull may take the
public route, and both present `cache.ivymect.in`.

## A proxy is a mechanism a service asks for

> gated and gateway should more be that the services needs some way to proxy /
> nginx provides a proxy mechanism / and then it just is passed into nginx
> generically / ideally it could even replace it with traefik or something else
> and it would still work as the proxy is the abstraction

Publication is not something a service *does*; it is something a service
**needs** and something else provides.

`den.capabilities.proxy` is a contribution surface — `subdomain`, `subpath`,
`port`/`upstream`, `websockets`, `sso`, `groups` (unioned, which is how
`access.nix` adds a grant without the service knowing), `maxBodyBytes`. All
facts about the **service**.

`aspects/services/proxy-nginx.nix` is nginx's *implementation* of that give: the
vhost, the ACME host, `proxyWebsockets`, `proxy_buffering off`,
`client_max_body_size`. All facts about **nginx**.

Swapping in traefik or caddy is writing another file like that one. No service
changes, because no service names nginx.

`gateway.nix`'s `gated.<n>` is the same need expressed as a forwarded class, and
it works — but its options *are* nginx's vocabulary and its implementation reads
them straight into `services.nginx.virtualHosts`, so the need and the mechanism
are one thing.

**The proxy is also the one thing entitled to assume loopback.** A proxy and the
service it forwards to are on one host by construction, so the scheme and the
address are the proxy's to supply and no service can get them wrong — the
opposite of `audiobookshelf.nix` assuming loopback for a cross-service call,
which breaks silently the moment either end moves.

**Not a replacement yet.** `gated.<n>` still drives every live vhost;
`proxy-nginx.nix` emits vhosts for `takes.proxy` declarations only. `virtualHosts`
is an attrset keyed by name, so the two coexist until a service moves — one at a
time, each gated on the rendered vhost being unchanged.

## `gated` is a narrowing of `proxy`

> gated should be an abstraction of proxy as well on top of it / its something
> on top of proxy

So it is not a mechanism beside proxying — it is proxying **plus**
authentication, and `includes = [ caps.proxy caps.credential ]` is what says so.
Three things fall out rather than being arranged:

- **A gated taker is a proxy taker.** It contributes `subdomain`, `port`,
  `websockets` to the same fold, so the vhost comes from whatever gives `proxy`
  and the gate from whatever gives `gated`. Neither half restates the other's
  fields.
- **Giving `gated` gives `proxy`.** An nginx-plus-oauth2-proxy aspect satisfies a
  bare `takes.proxy` too, which is right: a gate-capable proxy can also just
  proxy.
- **Taking `gated` is not satisfied by a plain proxy.** Narrowing runs one way,
  so a host with nginx and no gate cannot answer a service that needs one — where
  the alternative is publishing it unauthenticated and looking like success.

That forced a symmetry fix: **`takers` now follows the narrowing closure**, the
way `satisfies` already did on the giving side. An exact-key match would leave a
`takes.gated` aspect out of `takers "proxy"`, so nginx would render no vhost and
the service would be gated at nothing. `contributions` likewise reads from
whichever take key matched, since `takes.gated.subdomain` is contributing a
`proxy` option.

### The caller key is an edge credential, cross-host included

> it also can work with creating mutual credentials as well for service accounts
> if they are cross host for example

`gated` narrows `credential` as well, because a caller's API key belongs to the
*pair*: the caller presents its own key and the gate swaps it for the service's
internal one on the way through. `gateway.nix` mints `<caller>-<service>` exactly
that way today — the same five-workarounds-for-one-concept pattern the OIDC
secret and the database password were.

And cross-host is the *easy* case here: `credential`'s `realise` produces one
`.age` source and one materialisation per side, and when the sides are on
different hosts agenix simply rekeys that source to each host's key, which is its
ordinary behaviour. The same-host case — two owners of one file, deduplicated by
file — is the special one, and it already works.

`clients` is **unioned**, because the callers of a service are the union of
everything that asked, which no single file knows.

## Reconciling the two body limits

The proxy's limit and the carrier's cap are different facts that interact, and
nothing reconciled them. `cloudflare.nix` now derives the exclusions:

```nix
excluded = published names whose takes.proxy.maxBodyBytes > tunnelBodyCap
```

`tunnelBodyCap` is declared once and read twice — by the public carrier's
`maxBodyBytes`, so a flow that sends bodies cannot resolve to that route, and by
`excluded`, so a vhost accepting more cannot be published through it. Two
consequences of one fact.

**This fixes a live bug.** `cache.ivymect.in` is in `exclude` by hand because
`celler push` sends NARs past the cap and its vhost says `client_max_body_size
15g`. `cloud.ivymect.in` is **not** excluded, while `nextcloud.nix` sets
`maxUploadSize = "10G"` and notes the cap in prose — so uploads over 100MB fail
mid-flight, at run time. Same constraint, same host, one caught and one missed.
Derived from the number, both are caught.

A service that states *no* limit stays on the tunnel: null means "not stated",
not "unbounded", and excluding everything unstated would take every vhost off.

## Being gated is not optional-by-accident

`gated` narrows `proxy`, so a gated service lands in the proxy fold and
`proxy-nginx` would render its vhost — while the forward-auth leg is still
`gateway.nix`'s, driven by `gated.<n>`. That would serve it **unauthenticated**
and look like success.

So it throws, naming the service and what to do. A silently ungated vhost is the
worst failure that file could produce and it is invisible until someone browses
to it; a throw at eval is the cheapest version of the same discovery. The guard
lifts when `gated-oauth2-proxy` emits the auth itself.

A service with `sso = false` passes, because it does its own OIDC and never
wanted forward auth — `vikunja` and `donetick` are both real OIDC clients.

## The vhost conversion

**All 17 `gated.<n>` writers converted** to `takes.gated`, and the capability
layer *translates* rather than reimplements: `proxy-nginx.nix` turns a take into
a `gated.<name>` entry on `gateway.nix`'s forwarded class. One gate
implementation, with the capability as the front door — literally "it just is
passed into nginx generically".

An earlier draft rendered `services.nginx.virtualHosts` itself, which would have
meant two implementations of one thing: the forward-auth leg, the caller-key
rewrite map, the per-caller credentials and the ACME wiring all needing a second,
subtly different copy. I added a throw to stop it publishing gated services
ungated in the meantime — the right stopgap and the wrong shape. Translating
removed the need for both.

Converted: `vikunja`, `donetick`, `splitpro`, `immich`, `grafana`,
`audiobookshelf`, `tempo`, `homepage`, `bazarr`, `qbittorrent`, the four
servarr-generated apps, and — after the refactor below — `tautulli`, `esphome`
and `music-assistant`.

A take says `port`, not `upstream`: the proxy and the service are on one host by
construction, so the scheme and the loopback address are the proxy's to supply.
The `api` block passes through **verbatim**, because its six options are facts
about how one proxy rewrites a header — re-modelling them here would make this
type the union of one implementation's options, which is the mistake
`endpoints`/`access`/`group` on the old service type already made.

### Three conversions were wrong, and the fix was a refactor

**A take is keyed by the taking aspect**, where `gated.<n>` was keyed by an
explicit name. So three services declared *inside another service's aspect*
broke:

| file | aspect | old key | what broke |
|---|---|---|---|
| `plex.nix` | `plex` | `tautulli` | vhost → `plex.<zone>`, group → `svc-plex`, and `gateway.services.tautulli.url` (which its own homepage entry reads) stops existing |
| `esphome.nix` | `homeassistant` | `esphome` | published as `homeassistant` |
| `music-assistant.nix` | `homeassistant` | `music-assistant` | **collides with esphome's take** |

The last is the sharp one: two services writing into one aspect contend for a
single `takes.gated` slot, and the second definition collides with the first.
That is fine while everything is class content — den merges it — and stops being
fine the moment either wants a take.

**So each got its own aspect, pulled in by the one it used to live inside:**

```nix
den.aspects.homeassistant.includes = [ den.aspects.esphome den.aspects.music-assistant ];
den.aspects.plex.includes = [ den.aspects.tautulli ];
```

Nothing changes for the host, which includes `homeassistant` and `plex` as
before, and the subordination is preserved structurally: tautulli is inert on
any host without plex, exactly as when it lived inside plex's entry. The
in-tree precedent is `_postgres-backup.nix`'s
`den.aspects.postgres.includes = [ den.aspects.postgres-backup ]`.

`music-assistant.nix` also had **two** `den.aspects.music-assistant` blocks
after the earlier `serviceTypes` work — one near the top and one at the bottom.
They are one aspect now, which is the same "two halves of one thing in two
places" the serviceType/aspect split was.

### And the raw vhosts, which needed four new aspects

`secondpc/web.nix` declared four plain loopback proxies — `music`, `logs`,
`requests`, `bitwarden` — while their services were configured in `media.nix`,
`observability.nix` and `vaultwarden.nix`, all inside `den.aspects.secondpc`.
So each service was spread across two aspects and **neither was about that
service**: the vhost sat in a file that knows nothing about the thing it fronts.

Each is now its own aspect — `navidrome`, `seerr`, `loki`, `vaultwarden` —
holding its service config and its `takes.proxy`, included by the host:

```nix
den.aspects.secondpc.includes = [ den.aspects.navidrome den.aspects.seerr … ];
```

`takes.proxy`, **not** `takes.gated`: none of these was behind oauth2-proxy, and
converting them to the narrowing would have put a second login in front of
services that have their own accounts.

**The rendering is the same**, which is what makes the switch checkable rather
than hopeful: `gateway.acmeHost` defaults to the same zone the raw vhosts wrote
out, and `recommendedProxySettings` is already global in `nginx.nix`, so the
per-location copy in `web.nix` was redundant.

`web.nix` is now what its name says — ACME certs and their secrets.

### One vhost deliberately left raw

`aria2.ivymect.in` is a static UI served from a `root`, an `alias` location for
the magnet handler, and a proxyPass on one sub-path. Converting it would mean
putting `root` and `alias` on `den.capabilities.proxy` — nginx vocabulary, which
would make the type the union of one implementation's options and hand traefik
fields that mean nothing to it. The capability covers "publish this service under
a name and forward to it"; this is a web page that happens to proxy.

`immich.nix` and `media.nix`'s `logs.pierlot.com.au` also stay: the first
*augments* a gateway-built vhost rather than declaring one, and the second is a
different zone on basic auth, left pointed at whatever already uses it.

### And the body limit is now an option rather than a literal

`gated.<n>.maxBodyBytes` renders `client_max_body_size`, taking the **maximum**
across entries sharing a vhost — several services share `arr.<zone>` and that
directive is per server block, so the max is the only answer that does not break
whichever service needs the most. `nextcloud.nix`'s `10G` and
`celler/server.nix`'s `15g` are both this, written out in `extraConfig` today.

## Shares, and a den property I had wrong

`shares.nix` is off its quirk: `den.capabilities.share` is a contribution
surface (`displayName`, `quota`, `permissions`, `path`), the two share aspects
carry `takes.share`, and samba/nextcloud read a resolved record instead of a
`config.shares` option. The quirk, the `share-owner` policy on
`den.schema.user.includes`, and the conflict-thrower are all gone.

**All four keys are `each`, and that is a consequence rather than a shortcut.**
postgres has `union` and `exclusive` because many takers contribute to one
server-global thing — one `shared_preload_libraries`, one `cron.database_name`.
For shares **the taker IS the share**, so there is nothing server-global to fold:
`union` on `permissions` would hand every share the union of every *other*
share's, silently making a read-only folder writable.

**The part I got wrong in the brief.** I said the providers should read
`capability.contributions host "share"`. They cannot. A share aspect is delivered
by `person-includes` to a person's *accounts*, so it resolves at the **user**
scope — and `host.hasAspect` is den's *projected* membership, answering "is this
delivered into **this** scope". A host-level fold sees no share aspects at all.

So the resolution asks the **account**, with the same capability functions (both
take any entity carrying `hasAspect`), and members are derived from which
accounts answered rather than from an owner stamp. That also preserves the
existing limit exactly: a person with no account on the serving host contributes
nothing.

Verified: `share-family-photos` and `share-paperwork` both present. That was the
hinge — if `u.hasAspect` had not resolved, the shares would have vanished
*silently*.

(That group is `share-photos` now. The share was renamed — the household is not
a family — and the measurement above is left as it was taken rather than
back-dated, since the point it makes is about `hasAspect` resolving and not
about the name.)

## Service accounts, derived

`gateway.serviceAccounts` was hand-declared for the callers that are not
themselves gated services, while each gated service listed its callers in
`api.clients` — caller side in one file, callee side in another, nothing checking
they agreed. The set is now derived from gated services ∪ every name in any
gated entry's `api.clients`, with each account's `description`/`envPrefix`/
`restartUnits` coming from the caller's own `takes.credential`.

Hand-declaration dropped from 4 entries across 3 files to 1. Verified: **9
accounts** — `bazarr homepage lidarr prowlarr qbittorrent radarr seerr sonarr
tempo`.

**A correction to something I asserted.** I briefed this as "secondpc has exactly
2 service accounts". That is the number hand-declared in `media.nix`;
`gateway.nix` has derived one per gated service since before this work, so the
real set was always ~9. The agent checked the parent revision instead of trusting
the brief.

**And one regression that is real.** A misspelled name in `api.clients` used to
fail an assertion and now mints an account of that name. The three legitimate
caller kinds are not distinguishable by name — a gated service, an aspect with
`takes.credential`, and a `service-accounts.json` mint that is deliberately *not*
an aspect — so any check against the aspect graph rejects exactly the third. It
is documented at the site rather than papered over.

The secret naming was also wrong in three comments: it is one key per
**principal** (`<caller>/api-key`, swapped at the gate for `<svc>/internal`), not
`<caller>-<service>`, which no code has ever minted.

## Capability delivery cannot be a per-aspect policy

A policy on `den.schema.aspect` is the right shape and does not work. Four
attempts, four distinct recursions, every one reported as
`den.aspects.<host>._module.freeformType: infinite recursion`:

| attempt | what forced the cycle |
|---|---|
| gate on `config.gives != null` | the value |
| gate on `options.gives.isDefined` | the option, hence the submodule's freeformType |
| gate inside the policy body | body still reads `config.den.capabilities`, and a policy on every aspect forces `den.aspects` |
| raw `givenBy`, no registry read | the giver closures read `self.*` (`tailscale.nix`'s reads `self.domain`), forcing the aspect from inside its own `includes` |

All four are the back-edge den's `explanation/structural-introspection.mdx` names
under **"Never in `includes`"**: *"The tree depends on every aspect's `includes`;
`includes` depending on the tree is a back-edge with no fixed point."* The doc
says it of `hasAspect`; it holds for any read of the aspect being described — and
the fourth attempt shows a giver cannot even be *asked* what it gives from that
position, because answering reads itself.

`roles.nix` survives the same shape only because its body reads the aspect's own
plain-typed option and `host.hasAspect`, and never the flake config or a closure
that reads the aspect back. That condition is the precondition for the pattern,
and it is not stated anywhere in den's docs.

So delivery stays `_module.args` at host scope, with one cost kept in writing:
the arg name is per host, so two aspects on one host giving the same capability
have no single value to bind, and that throws. The collision is an artefact of
the channel, not the model.

## Use den's own lib

Where den already answers the question, this calls it rather than re-deriving:

| what | den's | instead of |
|---|---|---|
| can this parametric function take these args | `den.lib.canTake.atLeast` | a hand-rolled `functionArgs` check |
| typed fields on every aspect | `den.schema.aspect` | a side table keyed by aspect name |
| which host includes what | `host.hasAspect` | a declared membership flag |
| content into a scope with ctx bound | `den.lib.policy.resolve.to.withIncludes` | a per-class fold |
| config onto the right class | aspect class keys + `mkForward` | a `nixos`/`homeManager` arm each |

`den.lib.canTake` is the one that changed behaviour: a giver asking for an arg
the ctx does not carry is now **inert** rather than throwing from inside someone
else's file, which is the same deferral den gives every other parametric aspect.

Checked and **not** adopted: `den.lib.take` is deprecated in favour of `canTake`
(its own header says so), and den exposes no closure helper — `modules/aspects/definition.nix`
flattens `den.hosts` with the same `concatMap attrValues` this does, so that
idiom is den's own rather than a hand-roll.

## A nested capability is parametric over its chain

Three relations, and the third one turned out to need no declaration at all.

`includes` is **is-a**, and it narrows: `tcp` includes `address`, so giving tcp
answers a want for an address. `takes` is **uses-a**: `pull` takes `http`,
because that is how a cache is read, and it deliberately does not narrow it — as
a narrowing a pull would answer somebody's bare `takes.http`.

`gives` was **exposure**, the opposite of narrowing, and it was a list on the
parent: `celler.gives = [ cache ]`, `cache.gives = [ pull push ]`. It is gone.
A capability definition may now be a function of its ancestors:

```nix
den.capabilities.push =
  { cache, celler, ... }:
  {
    includes = [ caps.carried ];
    takes = [ caps.http ];
    inherit cache celler;
    furnishes = { name = cache.name; carrier = cache.carrier; };
  };
```

`{ cache, celler, ... }:` **is** the edge declaration, from the child, which is
the direction `includes` already runs — a narrowing names what it narrows rather
than being listed by it. So the parent's list was the same edge written twice and
able to disagree with the `cache`/`celler` fields beside it, which is exactly what
it did: `pull` declared `options.cache` and `options.celler` as strings a giver
furnished, so three places had to agree on one chain.

The relation is recovered by inverting the parameters, and the **direct parent is
the parameter that itself requires all the others**: `push` takes `cache` and
`celler`, `cache` takes `celler`, so `cache` is the parent and `celler` the
grandparent. That is the transitive reduction of the requires relation, and it is
why a definition may name its whole chain without saying which link is which.
Verified against the registry:

```
params  : cache -> [celler]   pull -> [cache celler]   push -> [cache celler]
parents : cache -> celler     pull -> cache            push -> cache
exposes : celler -> [cache]   cache -> [pull push]
```

which is byte-for-byte what the two deleted lists said.

### Two mechanisms, not one, and the registry stays typed

A capability has **declarations** (`options`, `includes`, `takes`, `rules`) that
do not depend on the chain, and **values** that do. So the registry holds the
definition applied with nulls — the chain-free view, which is what lets
`includes = [ caps.tcp ]` keep referencing by value — and `capability.nix`'s
`bind` re-applies it with the resolved ancestors where `furnishes` and `realise`
need them. One submodule evaluation, overlaid per chain rather than re-run: the
declarations were already type-checked by the registry's own pass, and an
evaluation per (capability, chain) is a cost this layer cannot carry.

The destructured args become a typed `chain` option rather than freeform keys.
`inherit cache celler` is written at the top level, where it reads, and
`parametrise` moves it under `chain`, where it is typed — because
`types.submodule` sets `shorthandOnlyDefinesConfig`, so a definition **cannot**
declare options of its own, and the `imports` route fails silently-ish with
`the option den.capabilities.cache.celler does not exist`. The move keeps the
property that mattered: every top-level key that is not one of the destructured
args still has to be a real option.

### The flows are derived, so a cache cannot be added without them

"pull and push are things that are automatically there." There is exactly one
pull per cache and every field of it follows from the cache and the celler above,
so nothing declares them — `furnishes` computes the link from the chain. The
whole of a cache is now:

```nix
den.aspects.celler-main = {
  includes = [ den.aspects.celler ];
  cache = { name = "main"; public = true; };
  gives = { self, ... }: { cache = _: { inherit (self.cache) name public; carrier = self.name; }; };
};
```

and a second cache arrives with its flows instead of needing two more entries
that could disagree with it. What an aspect still gives is the **instances**: one
celler carries several caches and which ones is a deployment fact, which is why
`cache` is given and the flows below it are not.

### The take mirrors it, and recurses

```nix
takes.celler = {
  name = "secondpc";
  gives.cache.main.gives.pull = { };
};
```

`gives.<cap>.<instance>` keys a sub-request by the instance's name, so the name
is structure rather than a clause. The take type is **recursive** — a sub-take is
a take, with its own `within`, `carrier` and `gives` — which was not merely
tidier: typed as `attrsOf (attrsOf raw)` a sub-take had none of the declared
clauses `judge` reads off every take, and the first full eval died on
`attribute 'within' missing`.

An **empty instance set means every instance**, which is what makes
`gives.pull = { }` read as "the pull of this cache" rather than "no pulls". So
`gives.cache = { }` asks for every cache on that celler, which is a useful thing
to be able to say and not a special case.

### A sub-level is reached, not searched

"specifically a scoped pull that can be consumed from that celler-cache, but
*that* celler-cache." So `resolveChain` resolves the root across every visible
giver and then reaches each level below through whatever won above. Two sources,
and which applies is a fact about the capability rather than a case the resolver
picks: **given**, by an aspect on the winning host, or **derived**, from
`furnishes` on the parametric definition. A leaf carries `chain` — the resolved
link at every level, keyed by capability — and `path`, `pull/main/celler@secondpc`,
leaf first, `/` for a step and `@` for placement, as den's `fx/identity.nix` does.

Two smaller things fell out of it:

- **The instance name belongs on `carried`**, beside the carrier. Every level
  filters on it, so it is not a fact about cellers or caches — it is what
  distinguishes two instances of anything. Null for a capability with nothing to
  distinguish: there is one loopback and one lan, and neither is named.
- **A giver cannot furnish its own name**, because its `gives` is evaluated in
  the ASKER's scope, so `host` there is the consumer. `celler/server.nix` had
  `name = host.name or self.name` with a comment claiming the opposite. The name
  falls back to the candidate's host instead, which for a celler is not a
  shortcut: "a celler server is named after its host" is a fact `push.nix`
  already relies on.
- **`capOptions` folded the wrong way.** `capSurface` is `[ cap ] ++ parents`, so
  folding it forwards let an included capability override the one that narrows
  it. `carried.name` is what made it matter — `user` declares a required `name`
  and would have inherited the generic nullable one.

## An edge is two hosts, not one

`realise` carried one `host`, which is enough for a credential whose giver runs
in one place and wrong the moment it does not. kanidm is one box; `celler` is
ONE ASPECT ON TWO HOSTS, and a realisation keyed on the giver's *name* collapses
two servers' credentials onto one secret.

So an edge now carries `giverHost` and `takerHost` (defaulted from `host`, so the
one-host callers read as before, but both existing callers state them anyway) and
`chain`, the resolved ancestors — a celler token is minted at the celler and
scoped to the caches below it, so the realisation needs the levels above its own.
`realiseOn` takes a resolved candidate directly, since `resolveChain` already
answered every one of those questions and a caller that resolved should not have
to take the answer apart and hand the pieces back.

`credential` gained `perProvider` for the keying, **defaulting false, and that is
a hard constraint rather than a taste**: it decides a secret's name on disk, so
flipping it for an existing credential renames the file and the value has to be
regenerated. Every credential in the tree today has a single-host giver.

For the same reason the celler token is **not** migrated onto `credential`: its
secrets are `celler_token_<server>` and `signing_key_<server>`, and the capability
form would name them `celler_token` plus `celler_token-<server>`. That is a rename,
a rename needs `gen-secrets`, and `gen-secrets` is forbidden while the master key
is mid-recovery. The mechanism is in place; the migration is gated on that.

## Exclusivity is a capability's own declaration

"port conflicts is more of something that a capability defines as it being only
declared onceable in a scope." A port is not special — it is the first instance of
*a giver claims something no second giver may claim* — so the rule belongs on the
capability and the enforcement is one generic walk:

```nix
exclusive.ports = {
  scope = "host";
  claims = link: ctx: ...;   # a port per name, bind falling back to ctx.dialledAt
  key = c: toString c.port;
  overlaps = a: b: ...;      # equal port is not yet the same socket
  describe = c: ...;
};

exclusive.subdomain = {
  scope = "fleet";           # the zone is fleet-wide, so the clash is too
  claims = link: _: lib.optional (link.subdomain != null) { inherit (link) subdomain; };
  key = c: c.subdomain;
};
```

`subdomain` is the argument for doing it this way: two hosts publishing one name
is the same bug one scope wider, it is exactly what happens when a service moves
between hosts, and a per-host check cannot see it. A new exclusive resource now
needs no change to the walk.

This is the GIVER-side mirror of a `takerMerged` contribution whose type refuses two definitions, which
refuses two *consumers* disagreeing about a server-global setting and says nothing
about two providers claiming one resource.

### `lib.unique` was hiding the thing the check exists to find

`networking.firewall.allowedTCPPorts` was `lib.unique (portsFor "tcp")`. With
ports exclusive per host, two providers *cannot* claim one, so the list is unique
by construction — and deduping it turned two services fighting over one socket
into one firewall rule and no complaint. Dropping it is the point, not a tidy-up.

## A bind address is not the service's own fact

"bind should also be like derived from wants as well, if loopback only wants it,
then only loopback demand." So `bindAddressFor` takes the union of the addresses a
capability's consumers actually dial on that host, which resolution already
answers per pair: loopback where every consumer is local, the carrier's address
where exactly one is not, the wildcard where several carriers are in play.
Verified on secondpc: `dialledAt = 127.0.0.1`, because every tcp consumer there
resolves over loopback.

The declared `bind.address` stays as the escape hatch for a service that binds
somewhere resolution cannot know about — a container veth, an access point's own
link — and `null` means derive it. A default of `"127.0.0.1"` was tried and backed
out: it would have had every service in the tree assert something about itself
that nobody checked.

**No cycle, and the reason is worth stating:** the derivation resolves CARRIER
halves (`linkBetween` over `carrierCapFor`) and never the endpoint half that
carries `ports`. Deriving a port's bind from something that reads `ports` would
close a loop through the type. That is also why the rule gets `dialledAt` handed
to it in `ctx` rather than reading a resolver itself.

`expose` wants the same derivation — a port needs a firewall hole exactly when
some consumer is not local — and is still declared. The two should meet.

## Three findings from testing the checks rather than trusting them

- **The exclusivity check was vacuous and passed.** Planting a deliberate second
  claim on celler's 8069 produced no assertion. `portClaims` was correct (7 claims,
  both 8069s present) and so was the conflict logic; what failed was DELIVERY.
- **`assertions` from a `den.default` aspect does not reach `config.assertions`**,
  while `networking.firewall` from the same aspect does. `den.default` lands in
  every scope, host and user alike, and an assertion emitted from a user scope is
  not the host's. The check moved to `den.schema.host.includes` — which is the
  right home anyway: "is this socket claimed twice on this machine" is a question
  about a host, the same reasoning `caches.nix` records for moving the public
  substituters off `den.default`.
- **celler2 was not a celler, as far as capabilities could see.** It included
  `den.aspects.celler-server._.nixos` rather than `den.aspects.celler`, so nothing
  there gave the `celler` capability and a nested take had no root to resolve
  from. Migrating the laptop's and auspc's hand-written `celler-use` silently
  dropped `celler_token_celler2` and `signing_key_celler2` from both scopes and
  changed the push hook — no error, just a push that would never happen. **Fixed,
  see below.**

## A capability declaration is parametric in what varies between deployments

celler2 could not include `den.aspects.celler` because that aspect fixes
`services.cellerd.expose.port` at 8069 and the container serves 8080. The port is
not something a giver can look up either: `gives` is evaluated in the ASKER's
scope, so `host` there is the consumer. Same reason `name` could not be furnished.

So the port is a **parameter of the declaration**. `cellerCapability { port;
subdomain; }` builds the capability half — `reach`, and `gives` for
`celler`/`http`/`user` — and the two deployments supply their own numbers:

```nix
den.aspects.celler           = cellerCapability { port = 8069; subdomain = "cache"; } // { … };
den.aspects.celler-container = cellerCapability { port = cellerPort; subdomain = "cache2"; };
```

Nothing in the resolver changed: givers are found by walking `den.aspects` for
whatever gives the capability, never by name, so a second celler aspect is found
the moment it exists. Two things fell out:

- **Both are split cleanly from their deployment.** celler2 takes no `proxy` — it
  publishes through its own cloudflared ingress, not through an nginx that is not
  on that box — and `cache-main` is separate from `celler-main`, so a host says
  which cache it serves without being told how to run a server.
- **`endpointFor` was resolving the right server at the wrong port.** It had
  `aspect = den.aspects.celler` hardcoded, which is secondpc's, on 8069. It now
  asks `providersFor h "celler"` for the aspect that actually gives it on that
  host. The symptom would have been a substituter that 404s.

Verified at flake level, in seconds rather than a host eval:

```
cellerGivers  secondpc -> [celler]   celler2 -> [celler-container]
cacheGivers   celler2  -> [cache-main]
default take  pull/main/celler@secondpc   pull/main/celler@celler2
laptop take   push/main/celler@celler2            (name = "celler2" filters)
auspc take    pull+push × both servers            (no name: reach = fleet)
```

auspc's is the shape worth noting: one declaration, no server named at all, and
it resolves to every celler the fleet has — so a third one needs no edit there.
`resolveChain` also got `emptyTake //` on sub-takes, so a take written out by
hand is total rather than missing the clauses `judge` reads.

## The build blocks on ONE undecryptable secret, not on code

`nixosConfigurations.secondpc.config.system.build.toplevel.drvPath` fails. It
gets as far as `agenix-home-manager-mount-secrets`, so the whole config renders
and only the rekey inputs are missing — nothing in the capability work is
implicated.

The cause, after two wrong turns worth recording:

1. The first symptom was a celler PANIC about `[jwt.signing]` being unset in a
   TOML that has nothing wrong with it. The env var name was correct all along
   (`CELLER_SERVER_TOKEN_RS256_SECRET_BASE64`, celler `config.rs:39`) — but
   `read_non_empty_var` treats EMPTY as unset, `$(...)` swallows rage's exit
   status, and rage's own error footer prints before celler even runs. So a
   failed decrypt surfaced three layers from its cause. `cellerTokenScript` now
   checks the value is non-empty and fails naming the file (length only; stderr,
   because stdout IS the generated secret).
2. **`secrets/generated/secondpc/celler-signing-key.age` is encrypted to the old
   GPG identity** (`-> gpg-v1 EC52A498…`), while every working secret is
   `X25519` + `piv-p256`. `signingKey`'s `rekeyFile` is a FIXED path to it, so
   every secondpc celler token has to decrypt it first — and every host pulls
   from secondpc by default.

**A census is only valid on the header.** A first pass grepped the first 2000
bytes, which includes ciphertext, and any body line starting `-> gpg-v1` counted
as a recipient. That produced one false positive, and it was the file a
conclusion had just been drawn from — celler2's signing key, which is fine.
Parse lines between `age-encryption.org/v1` and `---`, nothing else.

Corrected: **115 of 233 secrets are on the old identity.** Only these are
generator DEPENDENCIES, i.e. decrypted at generate time and therefore able to
stop a run: secondpc's celler signing key, `static/cloudflare_api`,
`static/tailscale_auth`, two `celler/r2_token-*`, and four `openclaw-token`. The
rest are only deployed, so they are stale on target rather than blocking.

The 38 `celler_token-*` files in the old flat/hyphenated layout, plus
`celler-push/celler_token`, are ORPHANS — the code wants
`celler-host/celler_token_<server>` — so a third of the old-identity set is
already dead weight.

### Why the celler signing key is the expensive one

Two routes, and they are not equivalent:

- **Recover the GPG identity, then `rekey`.** Re-encrypts it to the PIV identity
  and nothing else changes: existing tokens stay valid, `celler-keys.json`'s
  `secondpc.main = main:xS+QBlQ…` stays correct, signed NARs keep verifying. It
  also clears the other four blockers in the same pass.
- **Delete it and let the `openssl genrsa` generator re-mint.** Tokens generate
  immediately, but the cache's signing identity changes, so `celler-keys.json`
  is stale until `update-celler-keys` runs and every NAR already signed with the
  old key stops verifying — in practice a cache flush. celler2 escapes this only
  because `celler-keys.json` has no celler2 entry.

## State

Written, unevaluated:

- `aspects/base/capability-types.nix` — the capability definitions, and
  `gives`/`takes`/`preference`/`reach` on `den.schema.aspect`
- `aspects/base/capability.nix` — `providersFor`, `namesFor`, `linksBetween`,
  `linkBetween`, `resolveFrom`, `filterCandidates`, `takers`/`takersOn`,
  `visibleFrom`, `isRouted`, `materialise`
- `aspects/network/segments.nix` — `lan-home` and `loopback` as givers
- `aspects/network/tailscale.nix` — the tailnet as a giver, names derived
- `aspects/services/postgres.nix` — `gives.postgres`, `takes.unix`, `reach`
- `aspects/services/kanidm.nix` — `gives.user`, `gives.oidc`, taker-driven
  provisioning
- `aspects/network/tailnet.nix` — **deleted**

Known gaps, in the order they will bite:

1. ~~A credential names a scope that nothing creates.~~ **Closed**: the
   `credential` definition produces the entries and `kanidm.nix` installs them
   as `age.scoped.<scope>`. Unverified, like everything here.
2. **`lan-home`'s prefix is unconfirmed.** `/16` was carried over and never
   checked. It decides whether auspc (192.168.1.56) and secondpc (192.168.0.26)
   are siblings at all; at /24 the correct answer for a link between them is
   *none*.
3. **A person has no scope.** Scopes are per-host and per-user
   (`agenix-rekey.nix:45`), so a person with no unix account has nowhere for a
   credential to live. The per-person samba and Home Assistant credentials both
   need this.
4. **Four `runs` sites remain** — auspc's `lan-home.address` and
   `celler-cache-pull`, secondpc's `celler.defines.cache.main`,
   `lan-home.address`, `postgres` and `nextcloud`. The two `lan-home.address`
   lines duplicate `lanAddress` and go with the aspect include; the celler ones
   need the cache list off the registry first.
5. **2,537 lines still scheduled for deletion** — `service-dep-type.nix` (1463),
   `service-instances.nix` (451), `connection.nix` (354), `service-implement.nix`
   (105), `service-user.nix` (58), `service-libs.nix` (44), plus
   `den.serviceTypes` / `den.services` / `runs` in `service.nix`. Nothing goes
   until the replacement is proven, or there would be neither.
