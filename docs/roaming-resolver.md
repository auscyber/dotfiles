# The roaming local resolver

A laptop is on the house lan for part of the week and in a cafe for the rest.
The same services are wanted from both places, under the same names, and the
best route to them is different. This is the aspect that makes the *route*
follow the machine while the *name* stays put.

`aspects/network/roaming-resolver.nix`, included today by
`aspects/hosts/laptop.nix` and nothing else.

Unlike `capabilities.md`, most of this document *was* evaluated, and the
measurements are labelled. Where something is assumed, it says so — there is a
section at the end that is nothing but that, because two links in the chain
cannot be checked without being on the machine and on the wire.

## The sentence the whole design hangs on

> for tls, on the laptop if we want sso.ivymect.in and we are connected locally
> we should have sso.ivymect.in resolve to secondpc-house which then resolves to
> 192.168.0.26 — as the name sso.ivymect.in matches what the certificate
> provides, its fine, its just using a different route to get there

That is the insight that makes a local resolver the right mechanism rather than
a hack. Everything downstream of a published name — the certificate, the nginx
`server_name`, kanidm's exact-origin check, every bookmark and every config file
holding a URL — depends on the *name*. None of it depends on the *address*. So
the smallest possible intervention is to change only the address, and DNS is the
layer whose entire job that is.

`capabilities.md` had already arrived at the same split from the other side:

> So a link carries `address` — what you **dial** — and `hostname` — what you
> **present**. […] one name, many routes.

The capability layer's answer for "how do I reach that from here" is a
preference-ordered list of routes carrying one name. The resolver's job is to
*materialise the chosen one as DNS*. Nothing more.

## Direction: the resolver consumes resolution, it does not participate in it

This was left open —

> its the other way around i think, instead of lan-home furnishing an
> intermittent link — or i dont know, keep looking

— and the answer is neither. The resolver is a **consumer** of the capability
layer and contributes nothing back to it. Three things forced that.

### Giving the laptop `lan-home` would be actively wrong

The obvious move is to put the laptop on the segment and let `linksBetween` do
the work. It even works mechanically. Measured:

```
$ nix eval --impure --json .#debug.config --apply 'cfg: …'
lanLinkFromLaptop = { addr = "192.168.0.26"; iface = null; mtu = 1500; }
```

The link resolves, because `segments.nix` reads the address off the **peer**
(`peer.attachments.lan-home.address or peer.lanAddress`) and the interface off
the asking host, and a null interface is allowed.

But `lan-home` has `preference = 20` against the tailnet's `40`, and preference
orders *every* resolution, all of which happen at **evaluation**. Including it
would rewrite, into the built system:

- the celler substituter and `trusted-substituters` (`aspects/base/celler/push.nix`)
- the netrc machine lines and the push hook's endpoints
- `aspects/network/ssh-config.nix`'s known_hosts and host entries
- the time-machine destination (`aspects/darwin/time-machine.nix`)
- `bindAddressFor` on anything the laptop is a consumer of

all of them naming a `192.168.x` address, on a machine that spends half its life
somewhere that address means nothing. **The route is not knowable at evaluation,
which is exactly why it must not be ranked at evaluation.**

And it is wrong in the other direction too. Measured:

```
lanLinkToLaptop = { addr = null; }
```

The laptop has no `lanAddress` (DHCP; measured — only `auspc` 192.168.1.11 and
`secondpc` 192.168.0.26 have one in the whole fleet). So including the carrier
would make the laptop a lan *sibling* from every other host's point of view,
with a null address — which `house-wifi.nix`'s header already names as the
silent half of this failure:

> matching by segment alone would make a wifi-only host find the link and then
> resolve a null address, which is the silent half of the failure rather than the
> loud one.

### The consumer-side question is already answerable

`capability.linkFor` takes the **giving aspect** and answers for one named
carrier without any membership rule applying — and `capability.nix` exports it
for precisely this:

> `linkFor` and `capOptions` are exported because a consumer sometimes wants one
> specific provider's link rather than a resolution

So the resolver asks a different question from `linksBetween`: not "which
carriers do we share" but "**if** I were on that wire, where would that peer
be". Both numbers above come from that call. No change to the resolution layer,
no new capability, no new attachment rule.

### And the resolver has nothing to furnish

A `hostname`-giving carrier would have to furnish **one** name at evaluation.
The roaming answer is a *list*, chosen at run time. Worse, `segments.nix`
explicitly refuses to invent a namespace:

> Furnishing one here to silence a resolver would be inventing a namespace.

And the TLS insight removes the need anyway: the published name is kept, so no
consumer needs a new name. Nothing asks the resolver for a name; the resolver
answers for names somebody else published.

**So: `den.aspects.roaming-resolver` gives nothing and takes nothing. It reads
`linkFor`, `contributions` and `providersFor`, exactly as
`aspects/base/service-reach.nix` reads `dial` — which is the existing precedent
for "a consumer of links that emits runtime behaviour".**

## `attachment = "consumer"`: not needed, and it would have made things worse

The suggestion was a third value beside `both` and `provider`. It is a coherent
idea and it is the right *name* for what a roaming host has — an attachment only
the asking side holds. It is still the wrong thing to add, for two reasons, and
the second one is decisive.

**It cannot be added here.** `linksBetween`'s membership filter iterates the
*peer's* providers:

```nix
shared = lib.filterAttrs (n: asp: asp.attachment == "provider" || a ? ${n}) b;
```

`b` is host B's provider set, `a` is host A's. A carrier only A holds never
appears in `shared`, so `consumer` would need that line changed — and
`aspects/base/capability.nix` is not this change's to edit.

**It should not be added at all.** If `linksBetween` admitted it, a
consumer-attached `lan-home` would land at `preference = 20` and win every
eval-time resolution — which is the failure the previous section is about. Give
it a preference above the zone's 60 so it never wins, and it does nothing. There
is no preference at which a consumer-side intermittent carrier is both visible
to resolution and safe for it, because the problem is not *where* the route
ranks, it is that **its existence is not a fact at evaluation time**.

So the roaming attachment is declared outside the resolution layer entirely, as
`roams` on the aspect:

```nix
den.aspects.Ivys-MacBook-Pro.roams = [ den.aspects.lan-home ];
```

Same vocabulary as `includes`, deliberately not `includes`. `includes` means "is
on this wire"; `roams` means "the route exists when this box happens to be
there". One reader, `roaming-resolver.nix`, and nothing in `capability.nix`
changes.

If a second roaming host ever needs this and the `consumer` value starts looking
necessary again, the thing to re-read is that a `consumer` attachment is only
useful to a resolver that decides at run time — which is this aspect, which does
not need the carrier to be in the candidate list to ask about it.

## Intermittency without a boolean

> intermittent makes less sense, its more of intermittent *contains* a lan-home
> link, as it would require checking if false for users of it

Nothing in this design is a flag any consumer tests. The split is the one
`capability.nix` already makes for its candidate lists:

> eval time knows the SHAPE of the decision and run time knows what is up

**Evaluation produces the whole shape**: for each wire this host roams onto, for
each peer on it, the address that wire reaches the peer at and every name that
peer publishes. That is a complete, static plan. **Run time produces one bit per
group**: is it there.

The "links and possibilities" are the groups; the resolver is the consumer that
picks. No consumer of a service ever learns that any of this happened — which is
the property `roams`-not-`includes` buys.

Read the plan without a host eval:

```
nix eval --impure --json .#debug.config \
  --apply 'cfg: cfg.den.roamingPlan.Ivys-MacBook-Pro'
```

Measured output, one group:

```json
[{ "carrier": "lan-home",
   "peer": "secondpc",
   "address": "192.168.0.26",
   "hinge": "secondpc.roam.internal",
   "carrierNames": ["secondpc.tailef3db9.ts.net", "secondpc"],
   "names": [ … 26 names … ] }]
```

## The hinge is a transaction, not a CNAME

> when we have an intermittent resolver, we provide the intermittent links that
> abstract over the changes — like we give as an address `secondpc-house` or
> something like that, which may be tsnet or may be 192.168.0.26

The hinge is real and it is the **group**: one address, N names, one probe, one
flip. It is deliberately *not* a CNAME that N published names point at, and that
is a finding rather than a simplification.

**A CNAME to a locally-absent target blocks the fall-through to the public
route.** Make `sso.ivymect.in` a CNAME to `secondpc-house` and the off-lan case
has to remove `secondpc-house`'s address — at which point the resolver answers
the alias with no address, and the client gets nothing. It never tries the
public answer. The only way the public route comes back is for the local view to
have *no data at all* for `sso.ivymect.in`, which means the flip has to remove
every name in the group, which makes it a transaction over N names rather than
one record N names share.

So the group *is* the hinge, and the name asked for is served alongside for its
own sake: `<peer>.roam.internal` is an A record that exists exactly when the
group is pinned, so

```
dig @127.0.0.1 -p 5335 secondpc.roam.internal
```

says in one line which wire the box thinks it is on. `.internal` because ICANN
reserved it for private use, so it cannot collide with a real delegation. It is
NXDOMAIN when unpinned — there is no public answer and never will be, so "not on
that wire" is a local NXDOMAIN rather than a lookup that leaves the box to fail.

## Which names, derived

The published-name set comes from the capability layer, not from a list.

**Source 1 — the takers.** `capability.contributions <peer> "proxy"` is the
taker side of `den.capabilities.proxy`: every service that asked to be
published, with the subdomain it asked for. Measured on secondpc: 25 entries, 7
with an explicit subdomain and 18 with `null`.

`lib.defaultTo <takerName>` and **not** `or`, because the taker option has
`default = null` so the attribute *exists* — `CLAUDE.md`'s seven-bug trap. A
null subdomain means the service did not override it and the gateway publishes
it under the client's own name, which is what `gateway.nix` documents:
`default = name`.

**And a filter that is not optional.** A taker with a non-`/` `subpath` is
published under *somebody else's* subdomain — `proxy`'s own vocabulary for
"several services under one name". Without that filter the fallback above
invented five names. Measured, exactly:

```
subpath (non-"/") on secondpc:
  bazarr /subs   lidarr /music   prowlarr /indexers   radarr /movies   sonarr /tv
```

and none of `sonarr.ivymect.in`, `radarr…`, `bazarr…`, `lidarr…`,
`prowlarr…` is a vhost on that box. They all sit under `arr.ivymect.in`.

**Source 2 — the givers.** An endpoint may furnish its own `subdomain` rather
than take one. Measured on secondpc: exactly one does, `celler` → `cache`. So
`cache.ivymect.in` — the name asked about twice — is *derived*, not written.

**Source 3 — `alsoPublishes`, for a name a host-config literal owns.** A service
that renders its own `services.nginx.virtualHosts` entry records no `proxy` take
at all: `proxy-nginx.nix`'s `emits` is false without a port or an upstream, so
the name is invisible at flake level. Five such names exist on secondpc, and
each now declares its subdomain beside the vhost that serves it:

| name | declared on | why it is undiscoverable |
|---|---|---|
| `sso`, `auth` | `den.aspects.sso` | raw vhosts in `aspects/hosts/secondpc/sso.nix` |
| `arr` | `den.aspects.bazarr` | `gateway.services.bazarr.domain`, set in a `nixos` body |
| `home` | `den.aspects.homeassistant` | a `vhosts` key built from a file-local `domain` literal |
| `search` | `den.aspects.searchix` | a file-local `domain` literal |
| `aria2` | `den.aspects.secondpc-web` | a deliberately raw vhost (its own note says why) |
| `slsk` | `den.aspects.slskd` | `services.slskd.domain` — **and this one still does not reach the plan**, see below |

A **subdomain**, never a name: the zone still comes from the host's zone
carrier, so a service that moves zones is not edited. It is not a hand-written
zone — it is one fact declared once, at the site that owns it, in the same shape
`cloudflare.nix` uses for its hand-written `exclude` beside its derived
`excluded`.

### What the derivation actually covers, measured

secondpc's real vhost set is 31 entries (full host eval). Nine of those are not
names under a zone this flake models: `grocy-upstream` and `searchix-sources`
are not domains at all, and `logs.pierlot.com.au` / `media.pierlot.com.au` are a
second zone with no carrier aspect. That leaves 27 published `ivymect.in` names.

The plan pins **26 of 27**.

The miss is `slsk.ivymect.in`, and the reason is worth recording because it is
not fixable by declaring harder: `secondpc.hasAspect den.aspects.slskd` is
**false** at flake level (measured) although the vhost is in secondpc's
evaluated config. The aspect reaches the host through a sub-scope that entity
projection does not see — the same limitation `framework/requires.nix` states
about itself:

> only aspects reachable at flake level are read, so a specialisation routed in
> from a user-scoped contribution is not seen here

The `alsoPublishes = [ "slsk" ]` declaration is in place and correct; it will
start working the day that aspect is included where `hasAspect` can see it.

## Why the address, the name set and the probe are three separate things

The pin asserts three claims at once, and the probe has to check all three or it
is not safe:

1. that address answers,
2. the thing answering is the proxy that serves these vhosts,
3. the certificate it presents is the public one.

A TCP connect to `192.168.0.26:443` checks only (1), and that is not enough: at
a cafe, or at somebody else's house, `192.168.0.26` may well be a printer with
443 open. So the probe is a TLS handshake against a **published name** with
`--resolve` forcing the candidate address:

```
curl -sS --max-time 3 -o /dev/null \
  --resolve audiobookshelf.ivymect.in:443:192.168.0.26 \
  https://audiobookshelf.ivymect.in/
```

`--resolve` is what makes it independent of the resolver it is about to
configure — no chicken-and-egg, and no cache to flush first. And the success of
that command is *exactly* the user's TLS insight, executed: the name matches the
certificate, over a different route.

**No `curl -f`**, for `service-reach.nix`'s reason verbatim: a gated vhost
redirecting to the identity provider *is* the service answering, and `-f` would
turn a login page into an unpinned route.

**Fail-closed.** Anything other than a clean handshake removes the pin, so the
worst case of a broken probe is the behaviour of a machine without this aspect
at all.

## Why unbound, and why `/etc/resolver/<name>` is the part that matters

### The darwin constraint, checked

nix-darwin has **no `services.unbound`** — its service set is `dnsmasq`,
`dnscrypt-proxy` and `nextdns`. That is not a blocker: `pkgs.unbound` builds for
aarch64-darwin (measured: `unbound-1.26.1`, `meta.broken = false`,
`meta.platforms` includes it), and a `launchd.daemons` entry is ~10 lines, which
`aspects/network/tailscale.nix` already does for `tailscale-autoconnect`.

unbound is the right choice over dnsmasq for one reason: **`unbound-control
local_zone` / `local_data` is an atomic runtime flip over a unix socket.** No
config rewrite, no SIGHUP, no file to watch, no cache to clear. dnsmasq's
equivalent is `--hostsdir`, which works but goes through the filesystem and
cannot express "this name answers NODATA for AAAA".

### What darwin has that NixOS does not

`/etc/resolver/<name>` routes **one name** to a chosen resolver on a chosen
port. That is what keeps this a scalpel. Measured on the laptop:

```
$ ls /etc/resolver | grep -v 'in-addr\|ip6'
search.tailscale
tailef3db9.ts.net
ts.net
$ cat /etc/resolver/tailef3db9.ts.net
# Added by tailscaled
nameserver 100.100.100.100
nameserver fd7a:115c:a1e0::53
```

So the mechanism is confirmed live on this machine, already in use by
tailscaled, and there is no `ivymect.in` entry to collide with. The `port` line
nix-darwin's own `dnsmasq` module writes is the same directive this aspect
writes.

**One file per name, not one per zone, and that is not fastidiousness.**
`/etc/resolver/ivymect.in` would route the whole zone, and the zone is not only
vhosts. Measured, `capability.contributions secondpc "dns"` record names:

```
ivymect.in (×2), _dmarc.ivymect.in, _imaps._tcp.ivymect.in,
_submissions._tcp.ivymect.in, bitwarden.ivymect.in (×2),
_dmarc.bitwarden.ivymect.in, _imaps._tcp.bitwarden.ivymect.in,
_submissions._tcp.bitwarden.ivymect.in, *.ivymect.in
```

A local view of that zone answering NODATA for the apex MX and the mail SRVs
would break mail on this laptop in order to fix a web route. Per name, the blast
radius is the name: if the daemon is down, 28 names fail and the rest of DNS
never touches it.

The wildcard in that list is also why the published name set cannot be recovered
from the zone's own records — `*.ivymect.in` resolves anything.

`local-zone … static` rather than `redirect`, so an AAAA or HTTPS query for a
pinned name comes back NODATA instead of leaving the box. Without that, a client
with a public v6 answer routes around the pin.

### What the Linux half would take

systemd-resolved's split DNS is per-**link** and per-**domain**, never per-name.
Routing `~ivymect.in` to a local resolver on a NixOS host is one line, and it
inherits exactly the mail problem above — so the Linux version is a different
design (unbound as the host's only resolver, forwarding everything, with the
roaming names as local data) and is not in scope here. `surfacelaptop` is the
host that would want it.

Rather than coming up quietly inert on a NixOS host, the aspect carries an
assertion in its `os` body saying so. That is the module system doing the check,
per `CLAUDE.md`.

### Port 5335

Not 53, so nothing contends with mDNSResponder or with MagicDNS's
`100.100.100.100:53`, and because the resolver file carries a `port` line
regardless.

## Dynamic re-activation: what is actually possible

> and also dynamic re activation of things such as to remove cache.ivymect.in
> when its inaccessible

This is the part where the honest answer is smaller than the question, and one
measurement changes what the right answer even is.

### Eval-time config cannot do it, and nothing here pretends otherwise

`nix.settings.substituters` is written into `/etc/nix/nix.conf` at build time.
A running system cannot drop a substituter. The only existing lever is build
time, and it already exists: `den.aspects.celler-host.requires` probes the
celler, `aspects/tooling/nh-checked.nix` reads the result, and
`laptop.nix`'s `specialisations.no-celler` is the profile that excludes the
aspect whose check failed. That is the mechanism and it is a *build* mechanism.

### What happens today when a substituter is down — measured

```
$ nix config show | grep -iE 'timeout|fallback|download-attempts'
fallback = true            (default False)
initial-connect-timeout = 5
max-connect-timeout = 300
download-attempts = 5
stalled-download-timeout = 300
narinfo-cache-negative-ttl = 3600
```

`fallback = true` is set fleet-wide in `aspects/base/caches.nix`, and that file
already explains why in detail: without it a narinfo fetch that cannot connect
is **fatal**, and `secret-edit` could not open a secret purely because a cache
was unreachable.

So an unreachable substituter today is a *miss*, not an error, and it costs on
the order of 25 seconds of connect retries. That is the measured baseline, and it
is already an acceptable degradation. nix needs no help from this aspect.

### And the measurement that reframes the request

The laptop's celler substituter does **not** use `cache.ivymect.in`. Measured:

```
reachEndpoint "http" pullTake <laptop> {celler on secondpc}
=> [ { carrier = "tailscale";          addr = "secondpc.tailef3db9.ts.net";
       hostname = "cache.ivymect.in"; }
   , { carrier = "publish-ivymect-in"; addr = "cache.ivymect.in";
       hostname = "cache.ivymect.in"; } ]
```

`celler/push.nix` builds the URL from `link.address`, and the winning
candidate's address is the **tailnet name**. So the laptop's substituter is
`http://secondpc.tailef3db9.ts.net:8069/<cache>`, and **pinning
`cache.ivymect.in` in DNS cannot move nix onto the lan**, because nix never asks
for that name.

This generalises, and it is the most useful thing in this document:

> **DNS pinning improves exactly the routes the capability layer did *not*
> resolve** — the ones that go out as published names. Anything the capability
> layer resolved already carries an address, and DNS is not in its path.

Which is still most of the actual traffic: 26 browser-facing vhosts — immich,
nextcloud, plex/requests, audiobookshelf, grafana, home-assistant — reached by
name, from a browser or a CLI, going over the lan at full speed instead of out
through Cloudflare and back through hairpin NAT.

### The one lever that *does* move an eval-time answer

Pin the **carrier's own name** for the peer:
`secondpc.tailef3db9.ts.net → 192.168.0.26`. That moves the substituters, the
ssh config and the time-machine destination onto the lan at home with no rebuild
at all, because all three name the tailnet.

It is implemented, as its own aspect:

```nix
den.aspects.roaming-resolver-carrier-names
```

Membership, not a flag. **It is not enabled**, and the reason is stated here
rather than hidden: `/etc/resolver/tailef3db9.ts.net` is written by tailscaled
(measured above), and a more specific file beating it is darwin's documented
longest-suffix rule but has **not been observed against a running MagicDNS**.
Turn it on once that can be checked on the box; it is a one-line include.

The first version of that option was also a loaded gun, caught by reading the
plan output: `capability.providersFor host "hostname"` includes `loopback`,
which furnishes `localhost` for any peer — so the pin set contained `localhost`,
and enabling the aspect would have pointed `localhost` at `192.168.0.26`. The
filter is `capability.nix`'s own rule, that a carrier several hosts attach to
says so by giving `segment` and a loopback deliberately does not.

### What the resolver genuinely contributes to the "unreachable" case

One thing: **a pin is never handed out unless it currently works.** The probe
runs on every network change (`WatchPaths` on `/var/run/resolv.conf`, which is
what `/etc/resolv.conf` symlinks to — measured on this laptop, and configd
rewrites it on link changes) and every 120 seconds as a backstop. Off the lan,
or with the house down, the local view holds no data and the public answer comes
back unchanged.

It does **not** make an unreachable public route fail fast. It could — a
`local-zone … always_nxdomain` would turn a dead house into an instant lookup
failure instead of ~25s of connect retries — and that was deliberately not
built, because whether nix treats NXDOMAIN as permanent or retries it
`download-attempts = 5` times is not something that can be established from
reading nix's documentation, and guessing it would trade a measured 25-second
degradation for an unmeasured one.

## What this costs, and what is not known

### Cost

- **28 files in `/etc/resolver`** on the laptop, plus two launchd daemons. The
  unbound closure is 2.6 MB.
- **Every pinned name depends on one daemon.** If unbound is not running, those
  28 names do not resolve — and because they are routed by file, nothing falls
  back to the system resolver for them. `KeepAlive = true` is the mitigation and
  it is not a proof.
- **The forwarders are explicit**: 1.1.1.1, 1.0.0.1, 9.9.9.9. unbound cannot
  re-read `/etc/resolv.conf` on a network change the way configd can, so a
  network that forces its own resolver and blocks outbound 53 will break the
  pinned names while leaving everything else working. That failure is confined
  to the routed names, and it is the strongest argument for narrowing the routed
  set to the names that actually benefit if it ever bites.
- **A probe every 120 seconds** issues one TLS handshake per group. One group
  today.
- **Six files gained an `alsoPublishes` line** — a real maintenance cost, paid
  per raw vhost, and the reason the preferred shape is a `takes.proxy`.

### Not known

These are assumptions, not measurements. Nothing on a running machine could be
checked from the session this was written in — there is no ssh from it, and no
switch was performed.

1. **That `/etc/resolver/<fqdn>` wins over `/etc/resolver/<zone>`** by
   longest-suffix match. This is darwin's documented behaviour and the reason
   `roaming-resolver-carrier-names` is off; the published-name files have no
   competitor so they do not depend on it.
2. **That nothing else on the laptop binds `127.0.0.1:5335`.** Nothing should.
   `lsof` on UDP 53 needed privileges this session did not have, so even port 53
   being free is unconfirmed — which is part of why the port is 5335.
3. **That configd rewrites `/var/run/resolv.conf` on every relevant link
   change.** The symlink is measured; the write behaviour is assumed. The
   120-second interval exists because of this.
4. **That nix-darwin's `environment.etc` symlinks into `/etc/resolver/`
   coexist with tailscaled's own files there.** nix-darwin's own `dnsmasq`
   module does exactly this, which is the whole of the evidence.
5. **That unbound runs as root under launchd with `username: ""` and
   `chroot: ""`.** Correct per unbound's configuration, never started.
6. **That the probe's first name has a certificate.** It is the first of the
   peer's published names in sorted order — `audiobookshelf.ivymect.in` today.
   If that vhost ever loses its certificate the probe fails closed, which is the
   safe direction but would silently stop all pinning for that peer.
7. **That `unbound-control local_zones_remove` exits non-zero on a zone that
   was never added.** `|| true` is there on the unpin path because that is the
   normal case off the wire and `errexit` must not abort the remaining groups —
   but the exit code was not observed.
8. **The `/16` on `lan-home`.** `segments.nix` flags it as unconfirmed and it
   matters here as much as anywhere: auspc is `192.168.1.11` and secondpc is
   `192.168.0.26`, so at `/24` they are not on one segment at all. The plan does
   not depend on it — the pin's address comes from the peer's declared
   attachment, and the probe checks it — but any claim about who is a sibling of
   whom on that wire does.

### Blocked deploy steps

Nothing was deployed and nothing was committed. To land it:

```
darwin-rebuild switch --flake .#Ivys-MacBook-Pro
```

then, on the machine and on the house lan:

```
sudo launchctl print system/com.ivy.roaming-resolver | head
roaming-resolver-probe                      # should print "pinned to 192.168.0.26"
dig @127.0.0.1 -p 5335 secondpc.roam.internal +short
dig sso.ivymect.in +short                   # should be 192.168.0.26 through the resolver file
```

and off the lan, the same three, expecting NXDOMAIN on the hinge and the public
address for `sso.ivymect.in`.
