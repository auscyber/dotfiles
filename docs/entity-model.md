# Entity model — verification reference

**Status: oracle, not documentation.** This is derived from the user's own statements about what they
want, *not* from the code. Where the implementation disagrees with this file, this file is the
question to answer, and the implementation is the thing on trial. Nothing here was read out of
`aspects/` or `lib/`.

Marks used below:
- **[S]** stated by the user (quote is nearby or the wording is theirs)
- **[I]** inference by the author of this document — unverified, challenge it
- **[G]** gap: the user never said, and it matters

---

## 1. Entity kinds and kinds of relation

Not the whole graph — the *kinds* of node and the *kinds* of edge, one worked example each.

```mermaid
graph TD
  subgraph Principals
    P[Person]
    HH[Household]
    H[Host]
    FL[Fleet]
  end

  subgraph Services
    SC[Service class<br/>den.services.lan]
    SI[Service instance<br/>main/celler@secondpc]
    AT[Attachment<br/>lan.lan-home on secondpc]
    LK[Link<br/>a tcp with ip+port]
  end

  subgraph Contributions
    AS[Aspect / custom class]
    PA[Path]
    GR[Group]
  end

  P -->|member-of| HH
  HH -->|owns| SI
  H -->|owns| SI
  P -->|wants| SI
  H -->|wants| SI
  SI -->|wants / depends-with-character| SI
  FL -->|owns| SI

  SC -->|includes  (inheritance)| SC
  SC -->|narrows  (superset chain)| SC
  SC -->|provides| LK
  SC -.->|instantiated-as| SI
  SC -.->|instantiated-as| AT

  SI -->|placed-on  @| H
  SI -->|inherits-from| SI
  H -->|attaches| AT
  AT -->|carries| LK
  LK -->|routes-over  (layered)| LK

  AS -->|contributes-to| SI
  AS -->|contributes-to| H
  H ==>|includes aspect ⇒ creates| AT
  H -->|declares| PA
  AS -->|declares| PA
  PA -->|derives| GR
  SI -->|tagged| GR
```

### The transport stack is a separate, layered graph [S]
> "its network stack that creates a graph" / "is a layered directed graph" / "we want to use the
> least layered, highest priority thing where possible"

```
  websocket   ⊃ http ⊃ tcp          (user's word: "superset")
  protocols:    unix | tcp | udp
  carriers:     loopback | lan | tailscale | wireguard | wider-net | (cloudflare)
  every host has loopback by default, as a connection to itself          [S]
```
A *link* request is agnostic ("some service needs a 'tcp link' between two things"); what is
guaranteed by a tcp link is **an ip address**, and the link that gets created **has a port** [S].
A scope may hold several tcps; that is fine, and it becomes "a list or a priority mapping of
`tcp.lan.lan-home`, an inverse that means you can ask for a tcp, and then check if it
intersects" [S].

---

## 2. Relation table

| Relation | Direction | Meaning | Declared / derived | Obliges |
|---|---|---|---|---|
| `member-of` | Person → Household | person belongs to a place; a person may be in several, possibly as a guest [S] | declared | dropping the membership drops "their spaces and devices and entities" for that household [S] — a cascade, not a dangling ref |
| `owns` | Household \| Host \| Fleet \| *other* → Service | the thing responsible for the service existing. "households dont always own services, lots of things can own services" [S] | declared | owner supplies the policy/settings the service needs; set of legal owners is **[G]** |
| `wants` | Person \| Host \| Service → Service | "auscyber wants audiobookshelf service, like `users.auscyber` is we need auscyber user" [S]. Hosts want too: "time machine backups are something that ivys laptop wants, and it depends on samba in that nested manner" [S] | declared | the want must resolve to a *placed* instance; wants nest transitively |
| `depends-with-character` (`/`) | Service → Service | a dependency edge that itself carries a character and attributes: `push/…` vs `pull/…` [S]. "pretty much all service connections have a character to them and attributes to them that need to be intertwined" [S] | declared | the edge, not the node, owns shared material: cache access-token, oauth keys for kanidm, credentials for postgres [S]. All edges of the same character between the same pair are taken as a **fiber** and resolved once, then "pushed into each of those sub services" [S] |
| `provides` | Service class → capability | "the definition of what a lan is, is that it provides a tcp and udp connection" [S] | declared, in the class | anything that *includes* this class provides those capabilities too |
| `includes` | Service class → Service class | inheritance. `den.services.lan = { includes = [den.services.connection]; … }` [S]. "includes is important because the graph needs inheritance" [S] | declared | "a connection has options that must be defined on all things that includes it in an inheritance fashion such that we need it defined" [S] — including a class makes its options **mandatory** on the includer |
| `narrows` / superset | Service → Service | "a transport is a type of service, a narrowing" [S]; "websocket a superset of http a superset of tcp" [S] | declared in the class graph; **derived** when answering "does this intersect?" | a want for the general kind must be satisfiable by a narrower instance |
| `placed-on` (`@`) | Service instance → Host | `celler@secondpc` [S]. Placement is part of identity | declared **or** solved **[G]** | moving a service to another host "should know and be able to resolve everything automatically" [S] |
| `inherits-from` | Service instance → Service instance | "`main/celler@secondpc` inherits things from `celler@secondpc` obviously" [S], while still being "a separate service to `celler@secondpc`" [S] | **derived** from the identity path | the prefix of an identity path is itself an entity, and is a parent |
| `attaches` | Host → Attachment | `lan.lan-home = { address = "192.168.0.26"; prefix = "16"; interface = "br0"; }` for secondpc — "the same as setting those options by taking the lan definition" [S] | declared, by a **class that writes the entity via a policy** [S, user's chosen option] | "its something that is contributed and becomes part of the graph for secondpc" [S]; must not require full config eval [S] |
| `includes-aspect` ⇒ *creates* | Host → Aspect, yielding an Attachment | including an aspect **brings a connection instance into existence**: "including the tailscale aspect for example, means that the host now has a 'tailscale' which implements connection, which is named what my tailscale network is, that provides tcp and udp and such and has an address, and now can be used by services to derive connections" [S] | the inclusion is **declared**; the instance and its capabilities are **derived** from the aspect | the instance is named after the **network** (the tailnet), not after the aspect [S] — one tailnet, one name, shared across every host that includes it. Nothing per-host restates tcp/udp/address |
| `routes-over` | Link → Link/carrier | wireguard "requires a route over the internet over udp from one host to another, but it also produces another route between services / or maybe a route over tailscale / its nested" [S] | derived | the layering must be counted, so that "least layered, highest priority" can be chosen [S] |
| `declares` (path) | Host \| Aspect → Path | hosts define a "storage folder", not `/mnt/hdd`; consumers use `storageFolder/audiobookshelf` [S] | declared | **no hardcoded absolute paths** in services [S] |
| `derives` (group) | shared Path → unix Group | "dynamically produce unix groups that contain shared used paths so that they can be created as stuff happens" [S] | **derived** | two services sharing a path get a group without anyone writing it |
| `tagged` | Service → Group (tag sense) | "group is simply just a way to tag them" [S] | declared | nothing — it is only a label |
| `contributes-to` | Aspect → Service \| Host \| Person | "we should probably actually consider things such as access as being something that contributes to a service" [S]; "transport to be a service or like a service and provide its things to the other service instead of being bespoke" [S]; "shares shouldnt be declared on households, they are aspects, they are custom classes" [S] | declared | **no bespoke fields.** Anything that "only makes sense in relation to other services" arrives as a contribution, because bespoke "will lead to inconsistency and difficult maintainability" [S] |

Direction note [S]: "configuring of services is more that a host consumes a household entity and
produces config" — the host is the *consumer*, the producer of config; it is not the root of
ownership.

### Hard constraints (stated, not derived)

**C1 — Encapsulation.** A consumer that asks for `tcp` must not learn what carried it:
> "we resolve the route from the host to the music assistant server, by seeing that there is a tcp
> service connecting them which goes over tailscale, but **it being tailscale is not necessarily
> visible to the service it just knows it has some way to connect over tcp**" [S]

This is a requirement, not an optimisation. A consumer reads an endpoint; the underlay name is not
part of what it reads. Tested by checks 33–35.

**C2 — Non-recursion.** Raised unprompted by the user:
> "we also need to make sure things are not recursive as well" [S]

Recorded here as a constraint on the model, not an implementation note, because two real recursions
in this work had one cause: **reading evaluated config where declarations were needed.** Resolution
walks declarations. Tested by checks 36–37, and it is the same invariant as §6.2/check 40.

---

## 3. Worked examples (user's exact strings)

### Lead example — `musicassistant`

The user's best end-to-end case: one declaration, two different correct answers, no change at the
call site.

> "connecting to say a music assistant server named `musicassistant` or something, which we define
> as being on the household somehow, we resolve the route from the host to the music assistant
> server, by seeing that there is a tcp service connecting them which goes over tailscale, but it
> being tailscale is not necessarily visible to the service it just knows it has some way to connect
> over tcp" [S]

```
  household ──owns──▶ musicassistant ──placed-on──▶ (some host)
                            ▲
                            │ wants  tcp
                            │
                       consumer service ──placed-on──▶ host H
                                                        │ includes tailscale aspect
                                                        ▼
                                              tailscale connection instance
                                              (named after the tailnet,
                                               implements connection,
                                               provides tcp + udp, has an address)
```

| consumer is… | resolver picks | what the consumer sees |
|---|---|---|
| on the same host as musicassistant | loopback (least layered, local) | an endpoint |
| on a different host, tailnet only | the host's tailscale connection instance | an endpoint |

In both rows the consumer's declaration is **byte-identical**, and in neither row does the word
`tailscale` or `loopback` appear in what it reads. This one case exercises ownership (household),
placement, wanting, aspect-inclusion-creating-a-connection, providing, route resolution and
encapsulation at once.

### Identity path

"the identity should be the entity service relation path" / "as in deps" [S]
```
  celler@secondpc                 celler server, placed on secondpc
  main/celler@secondpc            the cache called `main` on that celler  → separate entity,
                                  inherits from celler@secondpc, also identified by cache.ivymect.in
  push/main/celler@secondpc       laptop's push edge to that cache
  pull/main/celler@secondpc       laptop's pull edge to the same cache
  pull/other/celler@secondpc      laptop's pull edge to a different cache on the same celler
```
"there may be multiple caches on one celler instance" [S]. The three laptop→`celler@secondpc`
edges above form one fiber, from which **one** access-token is generated and then distributed into
each sub-service [S].

**A want on a person.** `members.auscyber.services.audiobookshelf = {};` reads as "auscyber wants
audiobookshelf service" [S].

**Access is not a bare string.** Not `services = { vikunja = "admin"; }` but
`services.vikunja.access = "admin"`, alongside `services.vikunja.settings`, "and the admin comes
from the household not from the base person" [S].

**A class definition.**
```nix
den.services.lan = { includes = [ den.services.connection ]; provides = { tcp = {}; udp = {}; }; };
```
where "a connection needs a host or something or an ip address" [S].

**An attachment.** `lan.lan-home = { address = "192.168.0.26"; prefix = "16"; interface = "br0"; }`
for secondpc [S] — the user flagged this one: "thats not a hard example, but thats the information
we want to define and need to consider in terms of implementation" [S].

**A priority mapping.** `tcp.lan.lan-home` [S].

**A path.** `storageFolder/audiobookshelf`, never `/mnt/hdd` [S].

---

## 4. Open questions and contradictions

These are the point of this document. None are resolved here.

1. **Config or policy?** "we want it to be config, its important that it is config" [S] versus "the
   point of it being policy is that it means we dont have to eval full config and its on the parent
   entity" [S]. For attachments the user picked "Class that writes the entity via a policy". So: is
   the *attachment* policy while the *resulting service config* is config? Or is `lan.lan-home`
   itself literally config that a policy mirrors? Also unanswered: "im unsure if it goes on the
   aspect for secondpc or the host itself" [S].
2. **Is `lan` one thing or two?** "lan is a service in reality, its something which a host has" [S]
   versus `den.services.lan` as a global definition [S]. Class and instance appear to share a name.
   Does `lan.lan-home` live under the host, under `den.services`, or both with a link between them?
   **Partly resolved:** the instance now has a stated origin — *including an aspect creates it*, named
   after the network [S]. So the class is `den.services.connection`/`den.services.lan`, the aspect is
   what a host includes, and the instance is what the inclusion produces. What remains open is whether
   a hand-written `lan.lan-home = { address; prefix; interface; }` is a *third* authoring route or the
   same route with the aspect inlined.
3. **Where does identity actually come from?** Three competing sources in the same breath: the
   resolved dependency path ("the identity should be the entity service relation path") [S], a
   hand-written local name (`main`) [S], and an external address (`cache.ivymect.in`) [S]. The user
   wants all three to collapse so "that they are deduplicated in the entity evaluation" [S] — but
   the dedup key, and what happens when two of the three disagree, is **[G]**.
4. **`@` or `/`?** "maybe honestly slashes as well tbh instead of `@` like `main/celler@secondpc`"
   [S]. Undecided. If `/` is used for placement too, placement is no longer distinguishable from a
   dependency step in the string — is that intended? Does a *reader* need to tell them apart?
5. **Identity vs movability — the sharpest one.** Identity contains `@secondpc` [S], but "we should
   also IMPORTANTLY, be able to move a service to another host and it should know and be able to
   resolve everything automatically" [S]. Moving the celler server rewrites the identity of every
   descendant (`main/celler@secondpc` → `main/celler@contabo`), so every reference to it also moves.
   Is identity *stable* across a move (and `@host` is a rendering of a derived placement), or is a
   move a rename that the graph re-resolves? These are different implementations.
6. **Transport preference: one lattice or a host override?** "if we have lan or tailscale, we take
   lan" [S] versus "laptop should use tailscale primarily, therefore all the ip addresses it uses
   are the tailscale ip addresses" [S]. And "if a service is connecting to its local instance, we
   dont need it using its cloudflare ip" [S]. Precedence between global rule, host preference and
   locality is unstated. **[I]** that locality always wins and host preference overrides the global
   rule — needs confirming.
7. **Who may own a service?** "lots of things can own services i think" [S] — the set is never
   enumerated, and whether ownership is single or multiple is **[G]**.
8. **"Group" means two different things.** A tag ("group is simply just a way to tag them") [S] and
   a derived unix group over shared paths [S]. One word, two kinds. Same namespace?
9. **Person vs user direction.** "i would also prefer to be honest for users that people dont define
   the users, but that the users define the person" [S] — yet the want example is written at
   `members.auscyber.services.…` [S]. Is `members.<n>` the person entity, or the household's
   membership record pointing at a person defined elsewhere by a user?
10. **Who writes `access`?** The user wants access to be a *contribution* from a service-like thing
    [S], and also says the value `"admin"` "comes from the household not from the base person" [S].
    Is `services.vikunja.access` the household's contribution landing on the membership, or a field
    the membership declares that the household overrides?
11. **"Superset" direction.** The user says http is a *superset* of tcp [S] and also that a transport
    is a *narrowing* of service [S]. http has more characteristics, so as a type it is narrower. The
    arrow direction for intersection queries needs pinning down before any `intersects` check is
    written — the two words currently point opposite ways.
12. **Network segment ownership is data, not a rule.** Asked whether a segment is household- or
    fleet-owned, the user said "Depends on the network" [S]. So the owner is a per-segment field —
    but what, if anything, constrains it, and what a fleet-owned segment means for hosts in
    different households, is **[G]**.
13. **Unix sockets and placement.** "additionally for protocol, we should also have unix socket" [S].
    A unix socket only exists when both ends are on one host — **[I]** that asking for `unix` is
    therefore also a co-placement constraint, which the user never said.
14. **Encapsulation versus `tcp.lan.lan-home`.** Constraint C1 says a consumer "just knows it has
    some way to connect over tcp" and must not see that it was tailscale [S]. But the user also wants
    "a priority mapping of `tcp.lan.lan-home`, an inverse that means you can ask for a tcp, and then
    check if it intersects" [S] — a string that *names the carrier*, and an operation that implies
    somebody compares carriers. Both cannot be true of the same consumer. **[I]** that the resolver
    and the host policy may name carriers while the service may not — i.e. encapsulation is a
    property of the *consumer's view*, not of the whole system. Needs confirming (Q8).
15. **Aspects declaring ports.** "given we can make aspects declare transports and ports and such,
    it may be useful to have aspects also declare paths such that we can reason about permissions"
    [S] — port *allocation* (who prevents collisions, is a port asked for or assigned) is **[G]**,
    even though "the tcp link itself that we create has a port" [S] implies assignment.

---

## 5. Falsifiable checks

Each is a single assertion an implementation either satisfies or does not.

**Identity**
1. `celler@secondpc` and `main/celler@secondpc` evaluate to **two distinct entities**.
2. `main/celler@secondpc` resolves `celler@secondpc` as its parent and inherits its values, with
   local values winning.
3. Two independent declarations that name the same cache (by local name `main` on
   `celler@secondpc`, and by address `cache.ivymect.in`) produce **one** entity, not two.
4. An identity is a path whose every proper prefix is itself a resolvable entity.

**Wants**
5. A want declared on a person (`members.auscyber.services.audiobookshelf = {}`) and a want declared
   on a host both produce a demand of the same kind — neither path is privileged.
6. A host-level want with a nested dependency (laptop wants time-machine, time-machine depends on
   samba) pulls samba in transitively, without samba being named by the laptop.
7. A want that cannot be placed on any host is an **error**, not a silent no-op.

**Transport selection**
8. A want for `tcp` from a service on secondpc, where secondpc has both a LAN attachment and
   loopback, returns **at least two candidates with loopback ranked first**.
9. The same want, between two services on *different* hosts, never returns loopback.
10. Given both a LAN path and a tailscale path between two hosts, the resolver picks LAN — unless the
    requesting host declares a tailscale preference, in which case it picks tailscale and the
    resolved address is the tailscale address. (Check both halves; see open question 6.)
11. A service connecting to its own local instance never resolves to a cloudflare/public address.
12. A want for `tcp` is satisfied by a `http` provider (by the superset/narrowing relation), and a
    want for `http` is **not** satisfied by a bare `tcp` provider.
13. A resolved link always carries both an ip address and a port.
14. A wireguard-over-internet route resolves with a strictly higher layer count than the direct LAN
    route between the same pair, and loses to it on "least layered".

**Classes and inheritance**
15. A class that `includes` `connection` but leaves a required connection option undefined fails
    evaluation with a message naming the missing option.
16. `den.services.lan` advertises `tcp` and `udp` purely from its `provides`, with no per-host
    restatement.
17. `lan.lan-home = { address; prefix; interface; }` on secondpc is observably identical to writing
    the lan definition's options out longhand.
18. Attachments can be read without evaluating any host's full system config. (This is the
    operational meaning of "we dont have to eval full config".)

**Edge character**
19. Three edges `push/main/celler@secondpc`, `pull/main/celler@secondpc`,
    `pull/other/celler@secondpc` from laptop generate **exactly one** access-token, which appears in
    all three consumers.
20. `push/…` and `pull/…` to the same target yield **different** resolved capability, from the same
    token.
21. A service depending on kanidm acquires oauth key material attached to **the edge**; two
    different services depending on kanidm do not share one key.

**Placement and movement**
22. Re-placing `celler@secondpc` onto another host requires editing only the placement, and every
    dependent (caches, push/pull edges, addresses, tokens) re-resolves without further edits.
23. After such a move, no resolved address or path still mentions the old host.

**Paths and groups**
24. No service module contains an absolute storage path; every one composes from a host-declared
    folder.
25. Two services declaring the same path cause a unix group containing both to be generated, with no
    hand-written group.

**Contribution over bespoke**
26. `access`, and transport, reach a service as contributions from service-like things — there is no
    service-local bespoke `access`/transport field that only that service understands.
27. Shares are aspects/classes; there is no share declared directly on a household.

**Households and people**
28. One person can hold membership in two households simultaneously.
29. Removing a person from a household removes that household's spaces, devices and entities for
    them, and leaves their other household's untouched.
30. `services.vikunja.access = "admin"` is accepted and `services = { vikunja = "admin"; }` is not.

**Aspect inclusion creates connections**
31. Including the tailscale aspect on a host causes a connection instance to exist on that host which
    implements `connection`, provides `tcp` and `udp`, and carries an address — with **no per-host
    restatement** of any of those three facts.
32. That instance is named after the **tailnet**, not the aspect: two hosts including the same
    tailscale aspect for the same network name it identically, and a second tailnet on one host is a
    second, differently-named instance.

**Encapsulation (constraint C1)**
33. The value a consumer reads to reach a provider exposes an endpoint (address or host plus port, or
    a socket path) and **does not expose the transport name** — `tailscale`, `lan`, `wireguard`,
    `lan-home`, `loopback` and any carrier attribute are unreachable from it.
34. `musicassistant` declared once resolves to a **loopback** endpoint for a consumer on the same
    host and to the **tailscale** endpoint for a consumer on another host, with a byte-identical
    declaration at the call site.
35. Changing which underlay satisfies a want changes only the endpoint value; **no consumer-side
    declaration changes**.

**Non-recursion (constraint C2)**
36. No resolution step reads another entity's *evaluated config*; resolution reads **declarations**
    only.
37. A service wanting `tcp` from a provider **on its own host** evaluates without recursion. (This is
    the case that has actually recursed, twice.)

---

## 6. Redundancy and confusion audit

The user asked for the abstraction to be critiqued, not only recorded: *"verify if there are nodes
that seem redundant or confusing in order to finalise the abstractions and architecture"*. This
section is the author's argument, not the user's words — every claim here is **[I]** unless quoted.

Two pieces of build evidence are weighed throughout:
- **E1.** The identity path *rendered as the protocol stack by itself*:
  `websocket/http/tcp/attachment/lan-home` fell out of the want chain without being typed anywhere.
- **E2.** Capability fields added directly to the service type (`endpoints`, `access`, `group`,
  `users`, `provides`) all had to be backed out; two recursed. The user's diagnosis: "there are a lot
  of options that only make sense in relation to other services".

### 6.1 `wants` and the identity relation are one relation

**Verdict: redundant. Collapse them.** The user said identity is "the entity service relation path /
as in deps". E1 is the confirmation: if identity were a second relation it would have had to be
authored, and it was not. So there is **one** edge kind — *this entity wants that entity* — and
identity is its *rendering*, not a sibling of it.

What survives as genuinely extra is a **label on the edge**, and here is the consolidation worth
having: `main`, `push` and `pull` are the same thing. In `push/main/celler@secondpc`, `push` labels
the laptop→cache edge and `main` labels the cache→celler edge. The user described them separately —
`main` as a name "so that they are deduplicated", `push`/`pull` as the dependency's "character" —
but structurally both are a discriminator that makes two otherwise-identical edges distinct, and
both become a path segment. One concept: **edge label**.

Consequence: a want needs exactly three things — a target, an optional label, and attributes. The
"character" machinery and the "local name" machinery should not be two mechanisms.

### 6.2 `provides` should not be a field; it should be child entities plus a derived index

**Verdict: redundant with `includes` + the entity set.** `provides = { tcp = {}; udp = {}; }` asks the
service type to hold a map whose meaning is "other entities may match me here" — which is precisely
the shape E2 says keeps recursing, and for the user's own reason. The same fact is expressible
without a field: `den.services.lan` **declares child service entities** that `include`
`den.services.tcp` and `den.services.udp`. Then "what can satisfy a want for `tcp`?" is a **query
over the entity set by class membership** — a derived index, not state on a node. It cannot recurse,
because the index is built once over paths rather than by one service reading another's fields.

The user's own notation already assumes this: `tcp.lan.lan-home` is an entity path (inverted for
priority lookup), not a field read. Keep `includes`. Delete `provides` as stored state; keep the word
only for the derived index if it is useful.

This also answers the standing test for any proposed service field: a field is legitimate only if its
value is knowable from the entity itself and its own ancestors. `address`, `prefix`, `interface`,
`port` pass. `access`, `endpoints`, `users`, `group`, `provides` all fail, and they failed in
practice.

### 6.3 Attachment is not a kind — it is a service instance

**Verdict: three words, two kinds, one of them derived.** The user uses *connection*, *attachment*
and *link*. Sorted by what each actually is:
- **connection** — a *class*: "a connection needs a host or something or an ip address".
- **attachment** — `lan.lan-home = { address; prefix; interface; }`: an *instance* of a class that
  includes connection, living on a host. The user already said the quiet part: "lan is a service in
  reality, its something which a host has". An attachment is therefore a **service instance**, and
  should not be a distinct entity kind with its own type.
- **link** — the *resolved edge* with an ip and a port, produced per want. **Derived**, never written.

So the entity kinds reduce to: **class**, **instance**, and **derived edge**. Promoting "attachment"
to a kind buys nothing and costs a parallel type to keep in step with the service type.

The user's tailscale statement is direct evidence for this reading: including the aspect "means that
the host now has a 'tailscale' **which implements connection** ... that provides tcp and udp and such
and has an address" [S]. An instance that implements a class, carries that class's options and is
then wanted by other services is a service instance by every test that matters. Its one special
property is its **naming rule** — named after the network, not after the aspect — which is a
requirement on instance keys, not grounds for a new kind.

### 6.4 `reach`, placement and `host` — one declared thing, one derived thing

The user never asked for three of these; only placement (`@secondpc`) is in their words at all, and
"reach" is not their vocabulary. **Verdict:**
- *placement* is declared — and is most likely **a want edge whose target is a host** (see Q1),
  which is why the user drifted toward writing it as `/` instead of `@`.
- *reach* is **derived**: the set of links resolvable from a placement under the transport rules.

If reach is declared or stored, falsifiable check 22 (move a service, everything re-resolves) cannot
hold, because reach is exactly the thing a move invalidates. Keep it a query.

### 6.5 Aspect and service class are probably the same kind

The user, unprompted, equated them twice: "shares shouldnt be declared on households, **they are
aspects, they are custom classes**" and "some of these services type of things should actually be
more understood as **aspects that people include**" — *include* being the same verb as `includes`.
Plus "like a transport is a type of service, a narrowing". **Verdict: likely redundant.** One kind
(a class), one inclusion mechanism, used for transports, shares, access and services alike. Two kinds
means two include mechanisms and a rule about which may include which — which nobody has asked for.
This is the highest-leverage consolidation in the document, and also the one most likely to be wrong,
hence Q4.

### 6.6 The tag sense of `group` is a nullary class

"group is simply just a way to tag them" [S] — no options, no obligations. That is a class with an
empty body, and "tagged with `g`" is `includes g`. **Verdict: redundant; drop the tag kind.** The
*other* group — unix groups derived from shared paths [S] — is a genuinely different, genuinely
derived entity, and should not share the word. Naming them both `group` guarantees a reader will
assume the tag creates a unix group.

### 6.7 `owns` may be derivable from the want path — but dedup breaks it

Candidate redundancy: the **owner is the root of the want path**. `members.auscyber.services.…` is a
want written in the owner's namespace, and "a host consumes a household entity and produces config"
reads as consumption, not a second edge kind. If true, `owns` is derived and need not be declared.

The objection is dedup: when two principals want the same service, it has two roots. Then either it
is two entities (and "deduplicated in the entity evaluation" [S] fails) or ownership is a set (and
"the owner supplies the settings" has no single answer). Unresolved — Q2.

### 6.8 Confusing but not redundant

| Problem | Why it will be misread | Proposal |
|---|---|---|
| `@` for placement, `/` for dependency | teaches a distinction that 6.4 says may not exist; the user already wobbled: "maybe honestly slashes as well tbh" | use `/` as the only structural separator; keep `@host` as *rendering* sugar for the final host segment, if at all |
| "superset" vs "narrowing" | the user says http is a *superset* of tcp and that a transport is a *narrowing* of service — the arrows point opposite ways in the same vocabulary | one verb, one direction: **`refines`**. websocket refines http refines tcp; transport refines service. Never say superset |
| `services.<n>` on a person (a want), `den.services.<n>` (a class), and a host's services (placements) | one word at three altitudes; a reader cannot tell which they are editing | three names. Wants are `wants`; classes stay `den.services`; placements are rendered, not written |
| `provides` surviving as a word after 6.2 | reads as storage | if kept, keep it only as the name of the derived index |
| `lan` as both class and the host's instance | open question 2 | class is `den.services.lan`; the host's is an instance under a different attribute, never the bare class name |

### 6.9 Structural checks implied by this audit

Add these to §5; they test the shape rather than the behaviour.

38. Every path segment of every identity corresponds to exactly one want edge — no segment is
    produced by a second mechanism.
39. Identity paths are **most-specific-leftmost**, uniformly: `push/main/celler@secondpc` and
    `websocket/http/tcp/attachment/lan-home` agree in direction.
40. No field on the service type has a value that depends on another service's fields. (The E2
    canary, and the same invariant as constraint C2.)
41. `reach` is not writable; it is only queryable, and is recomputed after a placement change.
42. `tcp.lan.lan-home` resolves as a path over existing entities, not by reading a `provides` map.

---

## THE SETTLED MODEL

Confirmed by the user. Three relations plus placement, and an implication cascade.

### FOUR verbs, and the distinction between two of them was the hard part

| verb | meaning | scoping |
|---|---|---|
| **`defines.<name>`** | a NEW sub-thing this service invents. `celler` defines `cache`; a cache defines `push`/`pull`. | SCOPED by its definer -> `celler-cache` |
| **`gives`** | offers an EXISTING global service it did not invent. A lan gives `tcp`/`udp`. | unscoped -- it keeps its own name |
| **`uses.<svc> = { requirements }`** | a need, WITH requirements | — |
| **`includes`** | IS-A, narrowing. Transfers what the included gives INTO the includer. | — |

The user's own correction of a wrong example is the clearest statement of it:

> "cache doesnt give tcp, it defines push and pull, but it needs some things, celler wants postgres, a push and pull actually both want a http with specific requirements"

```
celler          DEFINES cache           USES postgres
  cache         DEFINES push, pull
    push                                USES http { unbounded body }
    pull                                USES http { cap does not bind }

lan             GIVES tcp, udp          <- existing globals
http            (a global service)      USES tcp
```

Scoping is resolved STRUCTURALLY rather than by convention: a type you define
inline is scoped because you invented it; a service you give keeps its name
because you did not. So `tcp` is bare and `cache` becomes `celler-cache`, with no
rule anyone has to remember.

### `gives` versus `includes` — the line, and why the names stayed

The user found this line odd, which is a design signal worth recording rather than
smoothing over:

> "the line between is gives and includes seems a bit odd, we more want the includes to be something else, as gives is more saying it functions as a global service i think, im not sure"
> "gives because we need to declare things on how it is giving, because its a bit odd"

A rename to `extends`/`implements` was offered — the standard distinction between
inheriting a definition and satisfying an interface — and declined, for a reason:
**`gives` carries the MANNER of giving, which `implements` would not.**

```nix
gives.http = { limits.bodyBytes = 104857600; };   # I give http -- capped at 100MB
gives.tcp  = { };                                  # I give tcp, unqualified
```

So `gives` is an ATTRSET rather than a list, and the value is *how*. That is also
exactly where a carrier's limits live, which the resolver had already arrived at
independently as `provides.<cap>.limits.<key>`.

**Why the line reads oddly, stated so it is not re-asked:**

| | role |
|---|---|
| `includes` | the inheritance EDGE. It transfers what the included *gives* into the includer — that is what `allProvides` IS. |
| `gives` | the CONTENT that gets inherited: the interfaces I satisfy, each with the manner of satisfying it. |

`gives` reads like a relation but is a declaration; `includes` is the relation that
carries it. They are not two flavours of one thing — one is the edge, the other is
its cargo. And `uses` must transfer NOTHING, which is the mechanical limit of the
collapse: if it did, a service that uses postgres would itself provide postgres and
a third service could resolve THROUGH it.

### REQUIREMENTS LIVE ON THE WANT

> "requirements very much live on the want"

Push and pull want the SAME service with DIFFERENT requirements:

```nix
push.uses.http = { maxBodyBytes = null; };   # unbounded: excludes the cloudflare tunnel
pull.uses.http = { };                         # the cap does not bind on responses
```

This is §2.7's constraint satisfaction arriving as a want attribute rather than as
a separate traffic-class table, and it is **why push and pull are separate
services rather than edge labels** — same target, different requirements,
therefore different resolution. The audit in §6 proposed collapsing them into
labels; this is the mechanical reason that is wrong, beyond the user saying so.

Both constraints already exist hand-written in the tree, which is the evidence
this generalises working code: `aspects/services/vban.nix` refuses the 1420-byte
wireguard tunnel because "a full VBAN packet does not fit" (MTU), and secondpc
excludes `cache.ivymect.in` from the cloudflare tunnel because Cloudflare caps a
proxied body at 100MB while `celler push` sends NARs far past it (body size).

**The requirements vocabulary is NOT centrally declared.** A carrier declares what
it satisfies, a want states what it needs, and the provider normalises — the
`postgres.nix` principle. A fixed set of requirement names would be a central
registry by the back door.

### `gives` — the TYPE relation, and it means two things by level

| on a TYPE | on an INSTANCE |
|---|---|
| `gives.cache = { }` — "a celler CAN give things of type cache". Declares what may exist on what. | `gives.cache = { main = { }; }` — "THIS celler gives a cache called main". Declares an instance. |

> "the give is a class i think, we say that one of the things celler gives is things of type cache and something of type cache gives push and pull"

**`provides` ≡ `gives`.** A lan giving `tcp`/`udp` and a celler giving `cache` are ONE relation — capabilities and sub-services are not two mechanisms. This was a real collapse.

**Type names are SCOPED to their giver, not global** [S]:

> "we define things that something gives as a service, push is a service, but i would actually prefer it was more scoped in name than having push pollute"

So `den.services.push` as a top-level type is wrong; `push` means nothing outside a cache. `gives` nests type definitions, mirroring the instance nesting.

**A `gives` edge carries the giver's state DOWN** [S]:

> "when we say gives.cache, we pass in that cellers master key into its caches or doing things it needs"

`aspects/base/celler/_lib.nix`'s `signingKey` is this already — the server's RS256 key is what mints each cache's token.

### `uses` — the INSTANCE relation, and a resolved `uses` IS a connection

> "and when we say a host 'wants push on a cache' that is a connection"

So `wants` ≡ `uses` ≡ connection: one relation, whose resolved form is the connection. The edge carries the attributes of the relation — the address, the port, the shared credential, the carrier's provenance. And a want is **existential plus a contribution** [S]:

> "when celler says it wants postgres, its not saying give me a postgres instance, its saying i need a postgres somewhere give me one and i am configuring it this way"

`uses.postgres = { database = "celler"; }` is both halves: the existential requirement, and the configuration flowing TO the provider. `aspects/services/postgres.nix` is this running in production over five consumers.

### `includes` — narrowing, and it CANNOT collapse into `uses`

The mechanical reason, which is the load-bearing limit of the collapse: `includes` **transfers `gives` into the includer** — that is what `allProvides` is, and how an attachment comes to offer tcp at all. `uses` must transfer nothing. If `uses` also transferred, a service that uses postgres would itself provide postgres, and a third service's `uses.postgres` could resolve THROUGH it — a consumer masquerading as a provider, silently.

### Placement, and identity as the path

Identity is the path through giving and placement: `main/celler@secondpc` is *main, given by celler, on secondpc*. `/` is a give, `@` is placement. A move is a RENAME and the consequences are accepted (Q6).

### The IMPLICATION CASCADE — how instances come into existence

> "when we define say the celler cache on secondpc, we declare that we say the caches we will provides are 'main' and because we say they are caches, they imply that there can be pushes and pulls"

Declare an instance on its giver; every instance ITS type gives is implied, transitively. Nobody declares a push — it exists because `cache` gives push and `main` is a cache.

```
secondpc                                  declared (host)
  celler@secondpc                          declared, placed
    main/celler@secondpc                   declared: gives.cache = { main = { }; }
      push/main/celler@secondpc            IMPLIED by the type
      pull/main/celler@secondpc            IMPLIED by the type
```

**New recursion risk this introduces:** a type that transitively gives itself is an infinite cascade. Needs a bound or a named refusal rather than an abort.

### Definitions versus instances

`den.services.<key>` holds **definitions** — abstract types and shared segments. **Instances are never written down**: a connection comes from host-side config (`uses.<network>`), a sub-service from its giver's `gives.<type>.<name>`, and everything below that from the cascade. Their keys and identities are derived.

## ANSWERED BY THE USER## ANSWERED BY THE USER

Three of the questions in this document are now settled. Recorded here so the
rest of the document is read against them.

**Q6 — identity is NOT stable across a move; a move is a RENAME.** [S]

> "when we rewrite it will have consequences yes and thats it yes"

So `@secondpc` legitimately belongs in the identity, moving a service rewrites
the identity of every descendant, and the knock-on consequences are accepted.
No machinery should be built to keep identity stable across a move. This closes
tension 5 ("identity vs movability"), which this document called the sharpest —
in favour of identity-contains-placement, with rewrite accepted as the cost.

**Q5 — the sub-service hierarchy is REAL NODES, not labels on one edge.** [S]

The audit in §6 proposed collapsing `main`/`push`/`pull` into edge labels. The
user rejected it, with this structure:

```
secondpc
  - celler
    - main
      - push
      - pull
```

> "they are sub things that are consumed from each thing"

So each level is a genuine service instance, and the relation is CONSUMPTION
from the parent. The `wants ≡ identity` collapse in §6 therefore does not extend
to flattening the chain: the chain stays, and its rendering as an identity path
is a consequence of it rather than a substitute for it.

Note the word "consumed" — it is also where the shared credential sits (one
access token per consumer per celler, covering the union of the sub-things
wanted), which is the "fiber" the user described.

**Q7 — the transport precedence is CONFIRMED as written.** [S]

```
loopback  >  host preference  >  lan  >  tailscale  >  wireguard  >  public
```

tie-broken by least-layered. `host preference` outranks `lan` but cannot
outrank `loopback`, because a same-host call must never acquire DNS, TLS or a
tunnel as dependencies. This closes tension 6.

**Q8 — the carrier is PROVENANCE: present, queryable, never spelled to obtain a
connection. And it may be CONSTRAINED deliberately.** [S]

> "as in it just wants a tcp, but it has as part of it how it got the tcp, but we
> can query it, but we dont use lan-home explicitly"

> "unless we want to do like 'only on lan-home' or something like that"

So encapsulation is about the DECLARATION and the ACCESS PATH, not information
hiding. This closes the tension this document found between C1 (encapsulation)
and the `tcp.lan.lan-home` priority mapping — both are true, of different things:

| permitted | forbidden |
|---|---|
| the candidate set keyed by carrier (`tcp.lan.lan-home`) | a consumer naming `lan-home` to DECLARE what it needs |
| the link carrying its provenance — which candidate won, the ordered list, the path | reading the endpoint THROUGH the carrier (`link.lan-home.endpoint`) |
| anything INSPECTING that, for debugging or a check | a declaration that stops working when the winning carrier changes |

And a deliberate constraint is FIRST-CLASS, not a workaround:

```nix
wants.tcp = { };                        # agnostic: best candidate
wants.tcp = { only = [ "lan-home" ]; }; # constrained: this carrier or FAIL
```

Three existing cases are exactly this: `vban.nix` refusing the 1420-byte tunnel
on MTU grounds, secondpc excluding `cache.ivymect.in` from the cloudflare tunnel
on the 100MB body cap, and `laptop.nix:265` pinning `celler-use.via =
"tailscale"`. A constraint must fail LOUDLY at evaluation against the candidate
set — silently choosing another carrier is the runtime failure the resolver
exists to prevent. Note the interaction with rename-on-move (Q6): a pinned
carrier means a service that moves off it breaks, which is an accepted cost of
an explicit constraint provided the break is an eval error.

**DEFINITIONS are hand-keyed; INSTANCES are derived from host config.** [S]

The user rejected a hand-written instance key outright:

> "no this is wrong, we shouldnt have to do den.services.\"lan-home@secondpc\""

and has now said three times that the attachment is config on the host:

> "we want it to be config, its important that it is config"
> "example for secondpc is we say lan.lan-home = {address = \"192.168.0.26\"; prefix =\"16\"; interface = \"br0\"; } and then that is the same as setting those options by taking the lan definition"
> "the point of it being policy is that it means we dont have to eval full config and its on the parent entity"

So `den.services.<key>` holds DEFINITIONS — abstract types and shared segments —
while INSTANCES come from host-side class content and their keys and identities
are DERIVED from where they were declared:

```nix
# definitions, hand-keyed, declared once
den.services.connection = { abstract = true; obliges = [ [ "host" "address" ] ]; };
den.services.network    = { abstract = true; obliges = [ "prefix" ]; };
den.services.lan        = { abstract = true; includes = [ network ]; provides = { tcp = { }; udp = { }; }; };
den.services.lan-home   = { includes = [ lan ]; prefix = "192.168.0.0/16"; };

# instance: config on the host. `lan-home@secondpc` is DERIVED, never written.
connection.lan-home = { address = "192.168.0.26"; interface = "br0"; };
```

**The class is named after the BASE ABSTRACTION and keyed by the FAR END** [S]:

> "this looks more like we should say connection.lan-home"
> "and its saying a connection to lan-home"

So `connection.lan-home` is read as a sentence — *a connection to lan-home*. The
class names the relation; the key names what is connected to. Three consequences:

- **One class, not one per kind.** `lan.*` would need `tailscale.*` and
  `wireguard.*` beside it. `connection.<network>` covers every transport.
- **The class name does not duplicate the definition.** That it is a lan is
  already stated by `den.services.lan-home = { includes = [ lan ]; … }`; putting
  it in the class name too gives two places to disagree.
- **Uniform for aspect-contributed instances.** The tailscale aspect contributes
  `connection.<tailnet> = { address = …; }` through the SAME channel as a wired
  LAN, which is the user's stated principle that a transport should "provide its
  things to the other service instead of being bespoke".

And it reads correctly for the degenerate case: `connection.loopback` is "a
connection to loopback", which suggests the per-host loopbacks belong in this
channel too rather than as separately derived keys.

**And a segment is not a connection.** A lan carries the subnet mask/prefix; a
connection to it carries the IP address [S]:

> "theres a distinction between defining a lan, and defining a connection to a lan, like a lan has a prefix and such, and a connection to a lan"
> "we should make a lan actually carry in and of itself the subnet mask/prefix type of deal and the connection defines the ip address"

This is a correction to an earlier example in this conversation that put `prefix`
on the attachment: a `/16` is shared by everything on the segment and cannot
sensibly differ per host. Note the open structural question it leaves — the user
also said "a network defines a subnet mask which is a subset of connection sort
of deal", and if `network` literally narrows `connection` then a segment
inherits an obligation to carry an address it cannot have. Recorded as **[G]**.

**Still open:****Still open:** Q1 (placement a want edge or its own relation), Q2 (one owner or
several), Q3 (class stores `provides` or declares children), Q4 (aspects and
service classes one kind — this document's own highest-leverage and
likeliest-wrong consolidation).


## 7. Questions for the user

Each states the choice, the options, and the cost. Wrong answers here mean rework, not a tweak.

**Q1 — Is placement a want edge, or its own relation?**
Options: (a) placement is just a want whose target is a host, so `celler@secondpc` is sugar for one
more path segment; (b) placement is a distinct relation with its own notation and rules.
Cost of (a): hosts must be entities that can be wanted, and `@` becomes cosmetic. Cost of (b): a
second edge kind to resolve, order and render, and the protocol stack that already renders itself
from want chains (`websocket/http/tcp/attachment/lan-home`) has to special-case its last step.

**Q2 — Does a service have one owner or several, and is the owner declared or derived?**
Options: (a) the owner is derived as the root of the want path — nothing to declare; (b) the owner is
declared and single; (c) owners are a set.
Cost of (a): when two principals want the same service, either it duplicates or ownership becomes
ambiguous — which conflicts with wanting them "deduplicated in the entity evaluation". Cost of (b):
a second principal wanting the same service must be rejected or re-pointed. Cost of (c): "the owner
supplies the settings" needs a merge rule.

**Q3 — Does a class store `provides`, or does it declare child services that are then indexed?**
Options: (a) `provides = { tcp = {}; udp = {}; }` is stored on the class; (b) `den.services.lan`
declares child entities that `include` `tcp`/`udp`, and "what satisfies a want for tcp" is a derived
index over class membership.
Cost of (a): it is the same shape as the five capability fields that had to be backed out, two of
which recursed. Cost of (b): `provides` stops being something you can read off one attrset, and
`tcp.lan.lan-home` becomes a path lookup — which is how you already wrote it.

**Q4 — Are aspects and service classes the same kind of thing?**
You said shares "are aspects, they are custom classes", that services should be "aspects that people
include", and that a transport "is a type of service, a narrowing". Options: (a) one kind — classes,
one `includes`, used for transports, shares, access and services alike; (b) two kinds, with a rule
about which can include which.
Cost of (a): everything must fit one type, including things that currently look nothing like a
service. Cost of (b): two inclusion mechanisms and a coherence rule between them, permanently.

**Q5 — Are `main`, `push` and `pull` the same mechanism?**
In `push/main/celler@secondpc`, `main` names a cache and `push` names a dependency's character.
Options: (a) both are *labels on a want edge* — one mechanism; (b) a local name and an edge character
are different features.
Cost of (a): a label must carry attributes (so `push` can mean "this edge needs the push half of the
token"). Cost of (b): two naming systems both producing path segments, and two dedup rules.

**Q6 — Is identity stable when a service moves host?**
You want to "move a service to another host and it should know and be able to resolve everything
automatically", but identity contains `@secondpc`. Options: (a) identity *is* the path, so a move is
a rename and every dependent's identity changes with it; (b) identity is a stable key and `@host` is
a rendering of a derived placement.
Cost of (a): references written by hand break on a move, and secrets/tokens keyed by identity rotate.
Cost of (b): two things to look at — the key and the rendering — and the pretty path is no longer the
identity.

**Q7 — What is the exact transport precedence, and does a host override it?**
You said "if we have lan or tailscale, we take lan", also "laptop should use tailscale primarily,
therefore all the ip addresses it uses are the tailscale ip addresses", and that a service talking to
its local instance should not use a cloudflare address. Proposed order, please confirm or correct:
**loopback (same host) > host's declared preference > lan > tailscale > wireguard > public**, with
"least layered wins" as the tie-break.
Cost of getting this wrong: every resolved address in the fleet, silently.

**Q8 — May anything name a carrier, and if so who?**
Encapsulation says a service asking for `tcp` must not see that tailscale carried it. But you also
want `tcp.lan.lan-home` as a priority mapping you "can ask for, and then check if it intersects" —
which names the carrier out loud. Options: (a) encapsulation is a property of the *consumer's view*
only — the resolver, host preferences and policy may name carriers freely, services may not; (b)
consumers may *constrain* a want by carrier (e.g. "tcp, but only over lan") and the ban is only on
*reading back* which one was used; (c) carrier names never appear outside the resolver, and
`tcp.lan.lan-home` is resolver-internal notation rather than something anyone writes.
Cost of (a): two vocabularies, and a rule about which layer is which. Cost of (b): the moment one
service constrains by carrier, moving it between hosts can make the want unsatisfiable — which
collides with the move requirement. Cost of (c): you lose the ability to express "prefer lan here",
which you asked for at host level.

---

## 8. Deliberately out of scope

Secret storage mechanics, the aspect loader, generated NixOS/Darwin config, module-system technique,
anything about currently existing code, and the full node/edge inventory of the real fleet — the user
asked for something "to verify against", explicitly "not the whole network of edges and notes".

## S: A SERVICE DEFINITION IS A SCHEMA — `host` DOES NOT BELONG ON IT

> "defining a host for a service doesnt make sense given the fact that it could be
> on any host, a service definition is essentially a schema for the service"
> "not the services themselves i think"

`den.services.<name>` is a DEFINITION — the schema of what that kind of service is,
what it `gives`, what it `defines`, what it `uses` and what it `obliges`. A schema
has no host, because the same schema can be instantiated anywhere.

So `host` on a definition is a category error, and three things in the tree are that
error made concrete:

  * `den.services.celler.host = "secondpc"` in ../aspects/services/celler-service.nix
  * the eleven derived `loopback-<host>` entries in ../aspects/services/connection.nix
  * `abstract`, which exists only to mark "this one is a definition" in a registry
    that holds both. If the registry holds only definitions, every entry is abstract
    and the flag has nothing to say.

**Placement belongs to the INSTANCE, and an instance is created by a host including
an aspect** -- which is also the answer to "how does an aspect say its relation to a
service". The aspect declares the attachment it brings; the host that includes it
therefore has one; the instance is derived from the pair. Nobody writes
`den.services."lan-home@secondpc"` because nobody writes instances at all.

### [G] MEASURED: the channel this needs does not cycle

The open worry was that reading a host entity to build `den.services` closes
`den.services -> den.hosts.* -> host resolution -> service-aspects -> den.services`.
Measured, and it does NOT, provided what is read is a DECLARED OPTION:

```
den.schema.host.options.attachments      (a plain option on the host entity)
den.services = <one entry per (host, net)> from den.hosts.*.attachments
  with includes = [ den.services.<net> ] and the segment's prefix carried down
-> secondpc units 168, no recursion
```

The module system is lazy per option, so reading one host-entity option does not
force that host's `resolved`. **What WOULD cycle is `hasAspect`**: den's own
`has-aspect.nix` records that it is a lookup over the owning entity's production
run, so touching it forces resolution and closes the loop. So membership must be
read as a declaration folded onto the entity, never asked as a predicate -- which
inverts §2.7's "membership is DERIVED from aspect inclusion, never declared" for
this one use, and the reason is mechanical rather than a change of mind.

Still untested: the other half of the fold, aspect declarations -> host-entity
option. ../aspects/base/attach-probe.nix is the probe for it and has not been run
against a host that actually includes the aspect.

## G: THE CASCADE IS HAND-ROLLED AND gen-scope ALREADY HAS IT

> "this infinite recursion speaks to the lack of understanding and usage of sini
> gen and algebra and such" / "they should be lazy and such" /
> "where are the dags and such"

Correct, and the specifics are worse than the criticism implies. Measured against
`gen-scope`'s actual API surface rather than its README:

| hand-rolled here | gen-scope already has |
|---|---|
| the `defines` cascade in ../aspects/base/service-instances.nix, with `maxDepth = 8` and a throw | `lib/cascade.nix`: `mkKind { below; spawns.<kind> = self: id: { ... }; }` + `mkKinds`, a staged fold where a cyclic `below` is **INEXPRESSIBLE** rather than detected -- names resolve only against kinds minted strictly earlier, and `depth = 1 + max depth below` is the termination witness |
| `childIndex`, `useIndex` -- inverse indexes over `rawIdentity` | `transpose`, `queryReverse` |
| "a push can see its sibling pull" (asked for, unbuilt) | `siblings` |
| the parent walk for `placement` | `ancestors`, `isAncestor`, `childDepth` |
| the `includes` fold | `inherit'`, `inheritAll`, `inheritSet`, `subtypeOf` |
| `giving` collection (designed, unbuilt) | `foldContributions`, `foldEquations` |
| nothing -- we have no fixpoint at all | `leastModel`, `leastModelRounds`, `wellFoundedModel`, `stratify` |

`gen-graph` is **not even an input** (only `gen-scope` and `gen-algebra` are
declared in flake.nix), which is why every DAG in this tree is hand-written.
`gen-algebra` is used for exactly one thing, the `includes` fold via
`record.foldLayersTraced`.

### [G] WHAT THE RECURSION ACTUALLY WAS, stated precisely

`listToAttrs` over the cascade: the KEYS of the generated instances depended on
reading `allDefines` off the registry being generated, and the module system
cannot hand over a merged option until every definition of it is known. So it was
an eager key computation inside a module fixpoint.

Laziness at the VALUE level does not fix that -- the keys are demanded. What fixes
it is reading a different option than the one being written, which is why
`den.serviceTypes` (definitions) is now separate from `den.services`
(instances). That separation is independently right -- a definition has no host --
but it should not be load-bearing for termination, and under `mkKinds` it would
not be: the nodes are SPAWNED into the graph, and `den.services` becomes a
projection of graph nodes rather than a self-referential fold.

### THE PORT, with the API now known

```
den.serviceTypes          definitions          -> gs.mkKinds, ordered by `below`
host `runs`               declarations         -> the root nodes
gs.spawns.<kind>          the cascade          -> replaces mk/maxDepth entirely
gs.buildRoots             the graph
den.services              a PROJECTION of graph nodes into den entities
```

This also delivers the two model requests that are currently unbuilt, for free
rather than as features: a sub-instance resolving from its PARENT's scope (so
reaching a cache is a question about the cache, not a traversal through whichever
host holds it) and its SIBLINGS being visible to it (so a push can consider its
pull without a global lookup that would find every other cache's pull).

## G: `implement` — THE BINDINGS WORK, THE DELIVERY DOES NOT

> "we also want importantly to be able to have options and config and such on
> these services remember, there are implementation details of normal classes
> which get propagated around to different sides of things"
> "the link using tcp has a port"

### [G] VERIFIED, by a completed eval

`den.serviceTypes.celler-cache-pull.implement` receives its bindings correctly:

```
bindings = [ cache celler celler-cache tcp ]
tcp      = { address = "127.0.0.1"; carrier = "loopback"; port = 8069; protocol = "http://"; }
cache    = the celler-cache instance, `serviceName = "main"`
```

so `"${tcp.protocol}${tcp.address}:${toString tcp.port}/${cache.serviceName}"` renders
`http://127.0.0.1:8069/main` with no carrier named anywhere in the body.

TWO FAMILIES OF BINDING, and the distinction held up:
  * a RESOLVED WANT by its own name -- `tcp`, and `celler-cache` from the
    identifying edge. The link is the carrier's address plus the scheme its
    encryption implies plus the EDGE's `shared` attributes, which is where the
    port lives: "the link using tcp has a port", so the port is neither end's.
  * an ANCESTOR by the label its DEFINER gave it -- `cache`, `celler`. Keying
    these by `serviceName` instead gave `main`, the instance's own name and the
    one thing a definition cannot know.

### [G] NOT VERIFIED -- DELIVERY PRODUCES NOTHING

The content does not reach the host. Measured: secondpc's
`nix.settings.trusted-substituters` contains `http://secondpc:8069/main` -- the
OLD `celler-caches` quirk -- and not the `http://127.0.0.1:8069/main` the
`implement` body renders.

The channel tried was an INLINE module as an aspect:

```nix
den.lib.policy.resolve.to.withIncludes "service" [
  { nixos = inputs.gen-bind.lib.wrap service.implement service.bindings; }
] { inherit service; }
```

An earlier probe in the same session used the same shape for a different purpose
and also produced nothing, so this is the second observation of it rather than a
one-off: `resolve.to.withIncludes` appears to want a DECLARED aspect, not an
attrset that merely looks like one. `resolve` used bare is already known to be
load-bearing here (../aspects/base/service.nix records that wrapping it in
`include` silently yields no scope at all), so a silent no-op from this family of
call is in character.

Delivery therefore needs either a declared `den.aspects.<n>` per implementing
service, or a FORWARDED CLASS keyed by service name -- which is §3a's "second
option" (`a service-keyed CLASS ... No new scope, no collision, and the name is
the key rather than a binding"), and the one §3a called cheapest.

### [G] AN INSTANCE DOES NOT INHERIT ITS DEFINITION'S `uses`

Found by this work and fixed in the cascade. `includes` merges capabilities and
obligations, NOT field values -- the same gap that bit `address`, `prefix` and
`public` -- so `celler-cache-pull.uses.tcp` sat on the definition and never
reached `pull/main/celler@secondpc`. Measured as
`bindings.tcp: attribute 'tcp' missing` while the ancestor binding beside it
resolved fine. A definition that cannot say what its instances NEED is not a
schema.

The merge has to happen in ONE place: the two blocks of the instance body are
joined with `//`, so writing `uses` in both would silently keep only the second.
That is the third appearance of displacement-by-merge in this model, after the
`runs` loopback default and `gateway.services.<n>.groups`.

### [G] EVAL COST, because it shaped the whole session

`den.services` takes 400-4300 seconds depending on what is warm. It requires
import-from-derivation (`agenix-rekey-patched`), and `.claude` -- 171MB, NOT
gitignored, unlike `.workspaces/` -- is copied into the store on every dirty-tree
eval.

NOTHING EVER HUNG. Three separate times a 30-70 minute eval was read as
non-termination because the tool timeout is 120 seconds, and work was reverted
chasing a bug that did not exist: the `http` layer, the `implement` policy arm,
`bindings`, and the `gen-bind` input were all removed and then restored. Several
bisects measured contention against leftover evals of the session's own, which
`pkill -f` failed to kill and `kill -9` left blocking the next one. The single
number that would have settled it -- how long one eval takes -- was measured
last. Measure the baseline before interpreting silence.

## G: `implement` WORKS — AND THE DELIVERY STORY WAS WRONG IN A WAY WORTH KEEPING

Supersedes the section above that said delivery produces nothing. It does work;
what did not work was the channel, and the reason is a den spelling trap.

### [G] VERIFIED END TO END

```
den.serviceTypes.celler-cache-pull.implement = { tcp, cache, ... }: {
  nix.settings.trusted-substituters =
    [ "${tcp.protocol}${tcp.address}:${toString tcp.port}/${cache.serviceName}" ];
};
```

lands `http://127.0.0.1:8069/main` on secondpc, from
`tcp = { address = "127.0.0.1"; carrier = "loopback"; port = 8069; protocol = "http://"; }`
and `cache.serviceName = "main"`. No carrier is named in the body, so the same
line renders the lan address from another host.

Gate: units 168/115/88/60, 7 oauth2 clients, 5 postgres databases, four celler
identities, 17 instances, nixfmt clean.

### FOUR SILENT FAILURES, STACKED

Each was invisible until the one beneath it was fixed, and three of the four
produced NO error at all:

1. **An instance does not inherit its definition's `uses`.** `bindings.tcp` came
   back missing while the ancestor binding beside it resolved. Same class as
   `address`, `prefix` and `public`: `includes` merges capabilities and
   obligations, never field values.
2. **Nor its `implement`.** So the delivery filter `s.implement != null` was
   false on every instance and the arm never fired.
3. **`includes` INSIDE A FUNCTION-FORM `den.schema.host` IS SILENTLY IGNORED.**
4. **`gen-bind.wrap` returns a RECORD, not a module** --
   `{ advertisedArgs; module; signature; validator; wrapped }`. Importing the
   record gives `The option 'advertisedArgs' does not exist`, which is at least
   loud. Its signature is `wrap { bindings = ...; } module`, options first.

### [G] #3 IS BIGGER THAN THIS FILE: `service-aspects` HAS NEVER RUN

```
den.schema.host = { ... }: { includes = [ ... ]; };   FUNCTION -- includes IGNORED
den.schema.host.includes = [ ... ];                   ATTR     -- works
```

Traced rather than inferred: a `builtins.trace` in an aspect added the function
way never printed while forcing `systemd.services`, and a `throw` placed in
`den.policies.service-aspects`' own returned LIST never fired either. Options
merge fine across two function-form definitions (`runs` and `serves` both
survive) -- it is only `includes` that goes nowhere.

So `den.policies.service-aspects`, attached that way in ../aspects/base/service.nix,
has never executed in this tree. §5d's probe measured the mechanism WORKING at the
time, so this is a regression introduced since, and it had no symptom only because
every service type sets `aspect = null`, leaving its one arm empty.

den refuses both spellings for one key ("attribute 'den.schema.host' already
defined"), so the attr form needs its own file: ../aspects/base/service-implement.nix.

### THE DELIVERY SHAPE THAT WORKS IS §3a's SERVICE-KEYED CLASS

ONE aspect bound to `{ host, ... }`, iterating the registry for the services
placed here -- not a scope per service. `{ host, ... }` binds reliably; the
per-service scope was never reached. This is the same move
../aspects/base/celler/server.nix already makes by reading `children` as DATA
rather than asking for a scope per cache, and it makes §3a's collision
impossible rather than avoided: one aspect resolved once per host cannot declare
a forward twice.

### [G] EVAL COST, CORRECTED

`.claude` (171MB) was NOT gitignored and was copied into the store on every
dirty-tree eval. With `.claude/` ignored, a full host eval went from ~4000s to
**15s**. The four rounds above were only possible after that.

NOTHING EVER HUNG. Three separate times a 30-70 minute eval was read as
non-termination because the tool timeout is 120 seconds, and work was reverted
chasing a bug that did not exist. Measure the baseline before interpreting
silence.

### NEXT, AND IT IS NOW VISIBLE IN THE OUTPUT

secondpc carries BOTH `http://secondpc:8069/main` (the old `celler-caches`
quirk) and the derived `http://127.0.0.1:8069/main`. That duplication is the
quirk this model replaces, and retiring it is the next step.

## G: THE HTTP LAYER AND THE CROSS-HOST PULL

> "we also need to in actuality consider http as a superset of tcp"
> "the link using tcp has a port"

### [G] VERIFIED AGAINST A FIXTURE, in 4 seconds

The full flake could not be evaluated (see the eval note below), so the
resolution layer was tested directly with a fixture registry -- two hosts, their
loopbacks, their lan attachments, an `http` layer on each, a cache on secondpc,
and a taker-side pull on auspc:

```
target       = main/celler@secondpc              the taker's identifying edge, resolved CROSS-HOST
unaryHttp    = http@auspc                        the nearest-provider answer -- THE BUG
crossLink    = tcp.lan-home.lan-home@secondpc -> 192.168.0.26   sameHost = null
sameHostLink = tcp.loopback.loopback@secondpc -> 127.0.0.1
```

`unaryHttp` is the control and the whole argument: a unary "nearest provider of
http" gives the TAKER's own http, whose underlay is its own loopback, so the
body would render `127.0.0.1` and point at nothing.

### THE TWO RULES THIS ESTABLISHES

**A LINK IS BINARY WHEN THE TARGET IS ELSEWHERE.** Unary resolution is correct
only when the thing being reached is on this machine. Otherwise the binding is
`linkBetween` between this service and the one it is a pull OF.

**A LAYER INTERSECTS OVER ITS UNDERLAY.** `linkBetween "http"` cannot work --
the intersection is over SEGMENTS and an `http` node carries no prefix. But an
http link between two hosts IS the tcp link between them with a scheme and a
port on top, and the provider instance carries its own `uses.tcp`, so the
underlay is read off what resolution already returned. One rule, so websocket
over http over tcp inherits it without a second case.

**PARENT AND IDENTIFYING EDGE ARE ONE RELATION.** The cascade STAMPS `parent`;
a taker DECLARES `uses.celler-cache.identifying = true`. The ancestor walk
follows whichever exists, via `resolveWant` -- which returns a KEY where
`resolved` returns only a value, and a key is what the walk needs to continue.
Without this a taker-declared pull bound no `cache` and its body could not name
the thing it is a pull of.

### [G] STILL UNVERIFIED: the type-level `bindings` path

The fixture exercises `service-scope`/`service-link`. It does NOT exercise the
real `bindings` option, so "auspc's pull binds `http.address = 192.168.0.26`"
remains inferred from the pieces rather than measured end to end.

### [G] THE EVAL STALL, unresolved and NOT understood

`den.services` through the real flake parks the evaluator at 0.0% CPU
indefinitely. Established:

  * with `allow-import-from-derivation false` it reaches its decision point and
    errors in 5s, naming `agenix-rekey-patched.drv`
  * that derivation builds standalone in 1s and its output is in the store
  * the daemon answers in 2s; `git ls-remote` to the one remote input is 2s
  * substituters and remote builders disabled changes nothing
  * a stale `~/.cache/nix/fetcher-lock-*` was a SYMPTOM -- left by this
    session's own `kill -9`s -- not the cause

So an eval needing one 1-second build blocks forever on requesting it. CAUSE
STILL UNKNOWN after eight hypotheses, every one of them wrong:

  1. the `http` layer            -- removed, still stalled
  2. the `implement` policy arm  -- removed, still stalled
  3. `bindings`                  -- removed, still stalled
  4. the `gen-bind` input        -- removed, still stalled
  5. a stale fetcher lock        -- real, but a SYMPTOM of this session's own
                                    `kill -9`s; removing it changed nothing
  6. the network / a slow remote -- codeberg `ls-remote` answers in 2s
  7. the binary-link call        -- disabled, still stalled
  8. remote x86_64-linux builds  -- `/etc/nix/machines` does route linux builds
                                    over wireguard and a trivial one takes 21s,
                                    which WOULD look like 0% CPU. But there is
                                    no ssh to a builder and no daemon child
                                    while an eval is parked, so it is not that
                                    either. Asserted in conversation before
                                    checking; retracted.

What is established and nothing more: with IFD disabled the eval reaches its
decision point in 5s and names `agenix-rekey-patched` (an aarch64-darwin
derivation that builds locally in 1s and is already in the store); with IFD
enabled the evaluator parks at 0.0% CPU indefinitely; the daemon answers in 2s;
and it reproduces with no other processes running.

ALSO ESTABLISHED, and it is the part that mattered most: 0% CPU is NOT by itself
evidence of a deadlock, and a long eval is NOT evidence of a loop. Both were
read as proof repeatedly, and work was reverted and restored twice on that
reading. Several "measurements" were this session's own processes blocking each
other, and each `kill -9` left a fetcher lock that blocked the next attempt --
the symptom was partly manufactured by the attempt to diagnose it.

**THE FIXTURE IS THE WORKING FEEDBACK LOOP** for anything in
`lib/service-scope.nix` or `lib/service-link.nix`: 4 seconds, no den, no IFD,
no host entities. Use it before reaching for a host eval.

### CAVEAT THAT OUTRANKS THE CODE

The cross-host link resolves over `lan-home` only because its `prefix` is
declared `/16`, which is UNCONFIRMED. auspc is `192.168.1.56` and secondpc is
`192.168.0.26`: at `/24` those are two segments and the correct answer is an
empty intersection. A link that resolves here is also a claim about the house.

# touch: measuring whether an unrelated edit rebuilds patched inputs
