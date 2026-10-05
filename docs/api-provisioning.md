# Configuration that only exists behind an API

A NixOS module can *enable* a service. It cannot always *configure* one. A
growing class of services keep part — sometimes all — of their configuration in
their own database, reachable only through their own HTTP or websocket API, with
no config file, no environment variable and no CLI to render it from Nix. For
those services the module gets you a running daemon and a login page, and
everything that makes the daemon useful is clicked in by hand and then exists in
exactly one place: a state directory that `restic` snapshots and nothing can
rebuild.

This document is about the general mechanism for closing that gap, and about one
concrete instance of it: **Music Assistant**, whose provider instances
(`chromecast`, `vban_receiver`, `filesystem_local`, `tidal`, …) are today a
hand-typed comment in `aspects/services/homeassistant/music-assistant.nix` and a
path in `aspects/services/homeassistant/state.nix`'s `backup` entry.

**Nothing in this document has been evaluated** — same disclaimer as
`capabilities.md` and `grocy.md`. A full-host eval needs several GB on this box
and none of the questions here needed one. What *was* checked is stated where it
matters:

- Music Assistant's behaviour is read from upstream **2.10.6** source — the
  version `pkgs/by-name/mu/music-assistant/package.nix` builds on the nixpkgs
  this flake actually locks — fetched into the store and quoted inline with file
  and line.
- nixpkgs modules are read from source and quoted with paths.
- The design sketch below is a sketch. No `.nix` file was written or edited.

### A provenance correction, because it bit twice while writing this

`flake.lock` contains **four** nixpkgs nodes, and the one named `nixpkgs` is not
the one this flake uses. The root input is `nixpkgs_3`, rev
`f20cd90e28fe0403873f473b29e42662cd86e8c4`, narHash
`sha256-EjZqHKObBfIOeHZnMRVV87HB3pPq/WBkmRJWyFxmZu8=`,
`/nix/store/glw7ngr0dwbp3al07rdlqkrbas7w3a02-source`. The node *called* `nixpkgs`
is rev `1cabc318…` — which is the revision `docs/grocy.md` cites, so that doc's
provenance line is now stale. The only reliable way to get it is to ask the
flake:

```
nix eval --raw .#inputs.nixpkgs.outPath
```

This is not pedantry. The two revisions ship **music-assistant 2.10.6** and
**2.7.2** respectively, and 2.10.6 changed the two things this whole design turns
on: it made API authentication mandatory, and it removed the ability to *add* a
provider by writing its config. Reading the wrong tree produces a design that
cannot work.

A second consequence, worth fixing separately: the long comment in
`aspects/services/homeassistant/music-assistant.nix` asserting that
`services.music-assistant.openFirewall` "DOES NOT EXIST" **is now wrong**. On
`f20cd90e` the module declares `enable`, `package`, `extraOptions`, `providers`
*and* `openFirewall`, and the firewall block it guards covers AirPlay's 5000/7000,
Sendspin, Snapcast, Squeezelite and Spotify Connect — but still not the VBAN
receiver's 6980, so the comment's *conclusion* ("nothing is lost by its absence")
survives its premise. The claim was true of `1cabc318` and is a good example of a
source-read comment outliving the source.

## The problem, stated once

There are four ways a service can be configured, and they are not equally good:

1. **A file the service reads at startup.** Render it from Nix, restart the unit,
   done. This is what we want and it needs no mechanism.
2. **A file the service reads *and writes*.** Now Nix and the service's own UI are
   two writers of one file. Somebody has to lose.
3. **A CLI the service ships.** Run it from a one-shot unit. Still a push, still
   needs ordering and idempotency, but no network and no credential.
4. **An API only.** The configuration lives in the service's database. The only
   way in is a request, which means: the service has to be *running* before you
   can configure it, you need a *credential* to talk to it, and you have to decide
   what "already configured" means well enough to not do it twice.

Case 4 is the subject here. The three hard parts are, in order of how often they
are got wrong: **idempotency** (reconcile, do not recreate), **ordering plus
readiness** (the unit being `active` is not the API being up), and **credentials**
(the thing you need to authenticate is usually produced by the thing you are
configuring).

## What this repo already does

Three mechanisms exist here already, and they are three different answers. Mining
them is the most useful thing in this document, because two of them are good and
the third documents a constraint that any new design has to respect.

### `prowlarr-connect`, and the servarr download client — schema-prefill upsert

`aspects/services/prowlarr.nix:70-180` and
`aspects/services/media/_servarr.nix:160-248` are the same program written twice.
Each is a `Type = "oneshot"`, `RemainAfterExit = true` unit named
`<service>-connect`, `after` the service it configures (and, for prowlarr, after
every *arr it syncs to), `requires` it, `wantedBy = multi-user.target`, with
`pkgs.curl` and `pkgs.jq` on `path` and the API key arriving through
`EnvironmentFile` from an `age.templates` entry.

Four things in them are right and should survive into anything new.

**The body is built from the service's own schema, not hand-written.** Both read
`$base/<kind>/schema`, select the entry whose `implementation` matches, and
override only the fields they care about:

```sh
tmpl=$(api "$base/applications/schema" \
  | jq --arg i "$impl" '[.[] | select(.implementation == $i)] | .[0]')
```

The comment says why, and it generalises past the *arrs: "an application entry
carries required fields beyond the three worth setting, and omitting any is a flat
400 naming none of them." Taking the service's own template and patching it keeps
every default the service expects, across versions, without the Nix side knowing
what they are.

**The identity key is chosen deliberately, and the comment records the bug that
chose it.** Prowlarr matches on `implementation`, not `name`:

> Matched on implementation: an entry added through the UI is named "Sonarr", so a
> name lookup misses it and the POST then collides with it.

This is the whole of idempotency in one line. "What makes two objects the same
object" is a per-service decision, it is usually not the name, and getting it
wrong produces duplicate rows rather than an error.

**Readiness is a bounded poll against a real endpoint**, not against the unit:

```sh
for _ in $(seq 60); do
  if curl -sfS -H "X-Api-Key: $key" "$base/system/status" >/dev/null 2>&1; then
    break
  fi
  sleep 2
done
```

with the reason stated in `_servarr.nix:184`: "The app rebuilds its database on
first start, so the API can lag well behind the unit being 'started'."

**`curl -f` is deliberately *not* used for the write**, and the comment is one of
the more useful lines in the tree:

> No `curl -f`: it hides the response body, and the body is the only thing that
> says WHICH field the app objected to.

So the write captures `-o "$resp" -w '%{http_code}'`, checks `case "$code" in 2*)`
and prints the body to stderr on anything else. A provisioner that swallows the
rejection body is a provisioner you cannot debug.

What these two get **wrong or leave undone**:

- **They are duplicated.** The unit skeleton, the readiness loop, the `api()`
  helper, the `-o/-w` write and the error case are byte-for-byte the same in both
  files, differing only in the endpoint and the fields. That is the shared half
  this document wants to name.
- **They never delete.** Nothing removes an entry that Nix stopped declaring, so
  the push is "upsert the declared set" and not "make the live set equal the
  declared set". For the *arrs that is the right default (the UI is a legitimate
  second author), but it is an unstated default rather than a chosen one.
- **The key comes through the environment.** `EnvironmentFile` plus
  `printenv PROWLARR__AUTH__APIKEY` puts the secret in the unit's environment,
  which is visible to anything that can read `/proc/<pid>/environ` for that pid —
  root only, in practice, so this is a small thing. It is also why
  `takes.credential.restartUnits = [ "prowlarr-connect.service" ]` has to exist:
  the unit holds the value in memory, so rewriting the template has to restart it.
  `LoadCredential` avoids both, and is what new work should use.
- **Prowlarr's `prowlarrUrl` lesson is per-service and irreducible.** The callback
  URL has to be loopback while `baseUrl` stays public, because prowlarr validates
  an application by making the *arr call *back* to it and that leg carries no key.
  No generic layer can know that.

### `kanidm-provision` — a state file and an external reconciler

`aspects/services/kanidm.nix` and `aspects/services/gateway.nix` drive nixpkgs'
`services.kanidm.provision`, which is the same pattern wearing better clothes.
Shape, as nixpkgs implements it:

- The desired state is `builtins.toJSON { inherit (cfg.provision) groups persons systems; }`
  written to the **store**.
- A separate binary (`pkgs.kanidm-provision`) reads it and reconciles over the
  API, tracking what it created so that `autoRemove` (default `true`) can prune
  what Nix stopped declaring.
- It runs as `ExecStartPost` on `kanidm.service` itself — not a separate unit —
  after a count-to-30 `curl` readiness loop.

Three things this repo adds on top, all of which generalise:

**Secrets are paths in the JSON, never values.** `basicSecretFile` carries a path;
the nixpkgs option documentation is blunt about it ("Do NOT use a path from the
nix store here!"); the paths are made reachable inside the sandbox via
`BindReadOnlyPaths`; and the tool opens the files itself. `aspects/services/gateway.nix:1244`
records the other half — the provisioner *writes* a value to a path at 0600, which
is how `ldapTokenFile = name: "/var/lib/kanidm-provision/${name}-ldap.token"`
(`kanidm.nix:117`) can be referenced from Nix as a path to a value Nix never sees.
That is the right shape for "a credential the service mints".

**When the tool's schema was missing an entity, we patched the tool.**
`patches/kanidm-provision/service-accounts.patch` and
`application-passwords.patch` add `/v1/service_account` support and API-token
minting, and the first patch's own comment is an idempotency lesson worth
reproducing in full:

> Kanidm will happily mint a second token with the same label, so the caller has
> to check before creating one or every run would issue a new one.

That is the generic failure mode of create-only APIs, and the generic fix: list,
match on a stable label, create only if absent.

**`extraJsonFile` is the escape hatch, and it has a cost.**
`gateway.nix:1232` records it: "setting `extraJsonFile` also switches off the"
— nixpkgs' eval-time referential assertions. nixpkgs validates that every group a
scope map names actually exists, that names are unique, and so on, and skips all
of it when `extraJsonFile != null`. A design that lets a caller hand over raw
documents should expect to lose its own static checking of them.

### `sync-tunnels` — and the constraint every new design inherits

`aspects/network/cloudflare.nix:49-56` is the single most important paragraph in
this repo for anything that wants to push declared state somewhere:

> **WHY STEP 4 IS NOT PART OF STEP 2.** The route list is `attrNames
> config.services.nginx.virtualHosts`, and a generated secret is part of
> `config.age.secrets` — so computing it inside the generator puts `age.secrets ->
> nginx.virtualHosts -> age.secrets` in the fixpoint (plenty of those vhosts read
> a secret path), which is the infinite recursion `lib/age-scoped.nix` warns
> about. `sync-tunnels` reads the SAME evaluated option from outside the fixpoint,
> where that read is just a read.

The generalisation: **the desired state may be computed from `config`, and may
read secret *paths*, but nothing that `age.secrets` depends on may read the
desired state.** Push from a unit on the host (inside the fixpoint, reading
`config.age.secrets.<n>.path`, which is a plain read) or from a flake app (outside
it entirely, reading the evaluated option). Do not push from a *generator*, and do
not route a provisioned credential back into `age.secrets` in the same evaluation
that produced the desired state.

`sync-tunnels` also shows the second pattern worth having: a `perSystem` app
(`packages.sync-tunnels`, `apps.sync-tunnels`) that `nix eval`s the fleet once,
upserts each record (`PATCH` if found, `POST` if not), and is documented as
idempotent. That is the right home for anything that must talk to a **third
party** rather than to a local service — it needs the operator's credential, not
the host's.

### And the two things that are deliberately *not* mechanisms

`aspects/services/homeassistant/state.nix` is the honest status quo. Home
Assistant's `.storage` holds areas, floors, every device and entity, and every
long-lived access token, and the comment says what that means:

> AREAS AND FLOORS LIVE ONLY HERE — they have no YAML form and no database row, so
> losing this directory loses the layout of the house along with every device,
> entity, person and long-lived access token.

and for Music Assistant, in the same `paths` list:

> Music Assistant's provider INSTANCES, which `./default.nix`'s header and
> `../audiobookshelf.nix`'s both explain cannot be declared — they are typed into
> its UI and stored only here.

So the current mechanism is `restic`. That is a real answer — it makes the state
survivable — and it is not a substitute for declaring it, because a restored
snapshot is not a reviewable statement of intent and cannot be diffed.

`aspects/services/homeassistant/prom-device.nix` is the other non-mechanism, and
it is a *constraint*: it builds HA devices from `fleet` rather than from
`inputs.self.nixosConfigurations.<host>.config`, with the reason spelled out —

> Asking another host's configuration what its node_exporter port is would make
> every evaluation of this box evaluate that box as well; `aspects/base/celler/server.nix`
> is the one documented exception in this repo and this is not it.

This matters below, because Music Assistant's VBAN receiver wants auspc's LAN
address.

## What nixpkgs does

The short version: **pushing Nix-declared configuration over a service's own API
is a real nixpkgs pattern, it appears in about six modules, and there is no shared
abstraction for any part of it.** Every one hand-rolls its readiness wait, its
idempotency scheme and its secret injection. Three of the six (kanidm, influxdb2,
firezone) are by one maintainer and are near-copies of each other, which is the
best available evidence that the pattern has a stable shape worth naming.

The distinction the question asks for, because conflating these is how "grafana
does this already" gets said:

| module | class | mechanism |
|---|---|---|
| `services.web-servers.unit` | **API** | `curl -X PUT` the whole config to a control unix socket |
| `services.syncthing` | **API** | ~12 `curl` calls against `/rest/config/*`, including reconcile and `DELETE` |
| `services.kanidm.provision` | **API** (via tool) + CLI | `kanidm-provision` over HTTPS; `kanidmd recover-account` to bootstrap |
| `services.influxdb2.provision` | **API** (via tool) + CLI | `influxdb2-provision` (Python); `influx setup` to bootstrap |
| `services.firezone.server.provision` | **runtime RPC** | `rpc 'Code.eval_file(...)'` into the live BEAM node |
| `services.automx2` | **API** | `curl -X POST --json @<store json>` to `/initdb/` |
| `services.grafana.provision` | **files** | `provisioningSettingsFormat.generate` → `GF_PATHS_PROVISIONING`. **Not the API.** |
| `services.keycloak.realmFiles` | **files** | tmpfiles symlinks + `--import-realm`; skipped if the realm exists |
| `services.authelia` | **files** | `config.yml` |
| `services.immich.settings` | **files** | JSON at `IMMICH_CONFIG_FILE` |
| `services.jellyfin` | **files** (partial) | writes `encoding.xml` into the mutable config dir |
| forgejo / gitea | **CLI** | `migrate`, `admin regenerate hooks`; no declarative admin user |
| nextcloud | **CLI** | `occ` |
| paperless | **CLI** | `paperless-ngx manage_superuser` |
| discourse | **CLI** | `discourse-rake admin:create_noninteractively` |
| mastodon | **CLI** | `rails db:migrate` / `tootctl`; no declarative users |
| vaultwarden | **none** | env file only; the admin page is manual |
| authentik | **not packaged** | not in nixpkgs at all |
| music-assistant | **none** | unit and package only |

To be explicit about the one that gets miscited most: **grafana's `provision` is
file-based.** `mkProvisionCfg` runs `provisioningSettingsFormat.generate`, a
`runCommand` assembles `$out/{alerting,datasources,dashboards,plugins}`, and that
directory becomes `services.grafana.settings.paths.provisioning`. There is no
`curl` in the module. It is a good design — grafana rereads the directory while
running — but it is category 1, not category 4, and it is only available because
grafana built it.

### Three shapes worth borrowing

**nginx Unit is the purest case**, because Unit genuinely has no config file:

```nix
preStart  = '' [ ! -e '${cfg.stateDir}/conf.json' ] || rm -f '${cfg.stateDir}/conf.json' '';
postStart = ''curl -X PUT --data-binary '@${configFile}' --unix-socket '/run/unit/control.unit.sock' 'http://localhost/config'  '';
ExecStop  = ''curl -X DELETE --unix-socket '/run/unit/control.unit.sock' 'http://localhost/config' '';
```

Idempotency **by destruction**: `preStart` deletes the persisted config so the
daemon always boots empty, then the whole thing is PUT in one shot. No diff, no
reconcile — Nix is the only writer and the service's own persistence is treated as
a cache. That is the cheapest correct answer *when it is available*, and the thing
to check first for any new service: can I just throw the state away and re-push?
(For Music Assistant: no. See below.) Note also the **unix socket**, which
sidesteps authentication entirely; worth asking for from any service that offers
it.

**syncthing is the most sophisticated, and three of its tricks are better than
ours.** `nixos/modules/services/networking/syncthing.nix`:

- The provisioner is its **own unit** with `requisite = [ "syncthing.service" ]`
  rather than `requires`. `requisite` + `after` means "run after it, never cause
  it" — a cleaner statement than our `requires`, and the only one of the nixpkgs
  API-pushers where a provisioning failure leaves the service running.
- **The credential is unknown at eval time, so it is scraped at run time** in a
  retry loop from the service's own `config.xml`, and the header is passed as a
  *file* so the key never reaches `/proc/*/cmdline`:
  ```sh
  (printf "X-API-Key: "; cat "$RUNTIME_DIRECTORY/api_key") >"$RUNTIME_DIRECTORY/headers"
  curl -sSLk -H "@$RUNTIME_DIRECTORY/headers" --retry 1000 --retry-delay 1 --retry-all-errors "$@"
  ```
  Readiness is folded into *every* call via `--retry-all-errors` rather than being
  a separate wait phase. `umask 0077` at the top of the script.
- **Secrets go into JSON bodies without ever being interpolated**, two ways. Either
  render the body pre-secrets and swap at runtime with `jq --rawfile`, or use
  curl's own indirection, which is the cleanest idiom in the whole tree:
  ```sh
  curl -X PATCH --variable "pw_bcrypt@$RUNTIME_DIRECTORY/password_bcrypt" \
       --expand-json '{ "password": "{{pw_bcrypt}}" }' <url>
  ```
- It is the only one that **prunes**: gated on `overrideDevices`/`overrideFolders`
  it GETs the live list, set-subtracts the declared ids with `jq`, and DELETEs the
  remainder. Opt-in "Nix owns this namespace", which is the same switch kanidm
  spells `autoRemove`.
- And it asks the service whether the push needs a restart rather than deciding:
  `GET /rest/config/restart-required` → `POST /rest/system/restart`.

**kanidm is the "external tool plus state file" shape** already described. The one
extra thing worth lifting is its *bootstrap*: rather than storing a long-lived
admin credential, if no password is declared it generates a throwaway one each
boot with `kanidmd recover-account` and pipes it straight into the provisioner. No
long-lived admin token exists anywhere. That is the ideal, and it is only possible
because kanidm ships a local CLI that can reset the admin out of band.

### There is no shared readiness helper, and this is the idiom

No `lib.mkWaitForUnit`, nothing in `nixos/lib`, no shared script. Grepping
`waitUntil|mkWaitFor|waitForUnit|waitForPort` across `nixos/modules` finds only
each module's private copy. `machine.wait_until_succeeds` exists in the *test*
driver only.

The most-copied form is kanidm's count-to-30 loop. The variant worth copying is
influxdb2's, because it adds the one check the others lack — don't spin for thirty
seconds against a corpse:

```sh
count=0
while ! <ping command> &>/dev/null; do
  if [ "$count" -eq 300 ]; then echo "Tried for 30 seconds, giving up..."; exit 1; fi
  if ! kill -0 "$MAINPID"; then echo "Main server died, giving up..."; exit 1; fi
  sleep 0.1
  count=$((++count))
done
```

(`$MAINPID` only exists inside the service's own unit. From a *separate* unit the
equivalent is `systemctl is-active --quiet <unit>`. Also: influxdb2 and firezone
both write `count=$((count++))`, which is post-increment and therefore never
advances the counter — a live bug in nixpkgs. kanidm's `$((++count))` is correct.
Use `$((count + 1))` and stop thinking about it.)

Failure behaviour is uniform and worth knowing: in every nixpkgs case except
syncthing the wait lives in `ExecStartPost`/`postStart`, so a timeout **fails and
tears down the main service**. Only syncthing isolates the provisioner in a unit
whose failure the service survives.

### The two patterns for "I cannot read this value back"

Both come from the CLI side, and both apply directly to secret config values that
an API refuses to return.

**paperless keys a flag file on the declared value**, which is the cleanest thing
in nixpkgs for this:

```sh
superuserState="$PAPERLESS_ADMIN_USER:$PAPERLESS_ADMIN_PASSWORD"
superuserStateFile="${cfg.dataDir}/superuser-state"
if [[ $(cat "$superuserStateFile" 2>/dev/null) != "$superuserState" ]]; then
  paperless-ngx manage_superuser
  echo "$superuserState" > "$superuserStateFile"
fi
```

Not create-once and not a reconcile: **re-apply when the declared value changed**,
with the state file as a content hash. (Store a `sha256` rather than the value
itself, as below.)

**mastodon asks the database instead of keeping a flag**, which is better when the
service will answer: `select count(*) from pg_class …` → zero means seed, non-zero
means migrate. Prefer this whenever the API *can* be asked.

### And the question nixpkgs actually answers, in jellyfin

`nixos/modules/services/misc/jellyfin.nix` renders `encoding.xml` into jellyfin's
own mutable config directory, and therefore has to decide who wins. Its answer is
an option:

- `forceEncodingConfig = true` — compare; if identical exit 0, else move the live
  file to `encoding.xml.backup-$(date -u +%FT%H_%M_%SZ)` and install the Nix one.
- `forceEncodingConfig = false` (**the default**) — if different, warn and do
  nothing:
  ```
  WARN: $encodingXml already exists and is different from the configured settings. transcoding options NOT applied.
  WARN: Set config.services.jellyfin.forceEncodingConfig = true to override.
  ```

That is the clearest statement in nixpkgs of the real design question, and
nixpkgs' default answer is **the service wins, loudly**. Any design here has to
pick a side per field, not globally — which is the argument for upsert-the-declared
rather than make-equal-to-declared as the default, with pruning opt-in.

## What the community has

The honest summary first: **there is no general-purpose "declare it in Nix, push
it over the API" framework worth depending on, and `nix-community` has nothing.**
A search of that org for anything matching `*provision*` returns zero. What exists
is one reusable tool we already use, one *better* model that is service-side, one
very young generic framework, a handful of per-service projects, and a different
school of thought that should be rejected on purpose rather than by omission.

### The case study: `declarative-jellyfin` → `jellarr`

This argument has already been had in public, with a migration at the end of it,
and it is worth recording because it is exactly the choice this document makes.

**`Sveske-Juice/declarative-jellyfin`** (~93 stars, upstream on Forgejo) makes
Jellyfin declarative by **writing its internals directly**: it carries its own XML
serialiser to produce `network.xml`, `encoding.xml`, `system.xml` and friends —
hand-encoding C# serialisation quirks, down to nullable `int?` having to emit
`<GuideDays xsi:nil="true" />` — and it creates users, permissions and API keys
with `sqlite3 INSERT` against `jellyfin.db`, with Jellyfin's `PermissionKind` enum
transcribed into Nix as integers. To do that it replaces the service's entrypoint
(`ExecStart = lib.mkForce "${jellyfin-init}/bin/jellyfin-init"`), which starts
Jellyfin once to let migrations run, stops it, edits the database, and starts it
again.

It works. And it carries `supportedVersions = [ "10.11.3" … "10.11.11" ]` with a
hard assertion against `cfg.package.version`, because it is pinned to the
internals of eight specific releases and every Jellyfin bump is a port.

Its own author has deprecated it in favour of **`venkyr77/jellarr`** (~179 stars,
AGPL-3.0), which takes the *same configuration surface* and applies it over
Jellyfin's **official REST API**, through a client generated from Jellyfin's own
OpenAPI spec. jellarr's README states the critique: declarative-jellyfin
"directly manipulates XML files and SQLite databases", "stops/starts Jellyfin
service multiple times", "breaks when Jellyfin changes internals".

Three things in jellarr are directly relevant here:

- **Readiness is explicit and bounded**: `preStart` polls
  `${base_url}/System/Info/Public` 120 times at one second, `exit 1` on timeout.
- **It is field-scoped, not namespace-scoped**: "only modifies explicitly
  configured fields". No pruning, no local state. That is the gentlest drift
  policy on offer and the one this design copies.
- **It also has a `dump` path** that reads a live server back out as YAML — an
  import route for an existing install. That is a genuinely good idea and is
  noted below as future work.
- **And it still needs one dirty write to get started.** Jellyfin only mints API
  keys through its UI, so jellarr ships a `bootstrap` oneshot that waits for
  `jellyfin.db`, `systemctl stop jellyfin`, sleeps, `sqlite3 INSERT … WHERE NOT
  EXISTS` into `ApiKeys`, and starts it again. **The API model is clean for
  configuration and dirty for its own credential**, and that is the general shape
  rather than a Jellyfin quirk. It is worth noticing how much better off Music
  Assistant leaves us here: `POST /setup` mints the first admin *and* returns a
  token, over HTTP, with no database write.

(A third option, `matt1432/nixos-jellyfin`, also exists. Three people
independently built this for one service.)

### `kanidm-provision`, and the one idea to steal from it

We already depend on it. The idea worth lifting into any future design is the one
thing it does that nothing in this repo does: it keeps its **ownership record on
the server**. A kanidm group `ext_idm_provisioned_entities` holds everything the
tool has ever provisioned, and pruning is "members of the tracking group, minus
the desired state". That record survives a host reinstall, needs no backup, and is
inspectable with kanidm's own CLI.

It is also careful about *granularity* — `overwriteMembers` (default true; false
gives append-only, so manual group additions survive) and a separate
`removeOrphanedClaimMaps`, because claim maps are not in the tracking group. They
needed two granularities of "do I own this" and still ended up with a special
case. There is no `--dry-run`.

And it is the strongest available evidence for the hardest limit on this whole
pattern: **APIs routinely refuse to accept secrets.** Provisioning an oauth2 basic
secret or an admin password is not possible through kanidm's API, so nixpkgs
*patches the kanidm server*, per version —
`pkgs/servers/kanidm/generic.nix` takes `enableSecretProvisioning ? false` and
applies `provision-patches/<version>/oauth2-basic-secret-modify.patch` and
`recover-account.patch`, exposed as `kanidmWithSecretProvisioning_1_10` and
`_1_11`. The in-tree comment is blunt about it: "This is NOT officially supported
(and will likely never be), see kanidm/kanidm#1747." Our own
`patches/kanidm-provision/` is the next layer of the same thing. Budget for this;
it is not an edge case.

### `recyclarr`, and the mistake not to copy

`recyclarr/recyclarr` (~2140 stars, MIT) is our prowlarr problem solved properly
and upstream of us: TRaSH-Guides custom formats, quality profiles, quality
definitions and naming, pushed over the Servarr REST API. It is in nixpkgs as both
a package **and** a NixOS module (`services.recyclarr`,
`nixos/modules/services/misc/recyclarr.nix`). If the *arr side here ever grows
past "one download client and an application list", this is what to reach for
rather than extending the shell script.

Two things to take from it and one not to:

**Take: its secrets handling is the best in this survey, and it is a nixpkgs
primitive we are not using.** The module runs its whole `configuration` attrset
through `utils.genJqSecretsReplacement { loadCredential = true; }`, so you write
`api_key._secret = "/path/to/secret"`, the **store only ever holds a
placeholder**, the function hands back both a `preStart` script and the
`LoadCredential` list, and the real value is spliced in at run time. It is defined
at `nixos/lib/utils.nix:398` of this flake's nixpkgs and returns
`{ script; credentials; }`. The sketch below uses it.

**Take: it names its drift policy in options** — `delete_old_custom_formats`,
`replace_existing_custom_formats` (whether to overwrite formats it did not
create), `reset_unmatched_scores`.

**Do not take: its ownership record is a local cache.** The transaction phase
consults an `ICachePersister<CustomFormatCache>` in `RECYCLARR_DATA_DIR` mapping
TRaSH ids to Sonarr/Radarr numeric ids, and pruning deletes what is *in the cache*
but no longer in config. Lose `/var/lib/recyclarr` and you lose the ability to
prune, and may recreate duplicates. That is a state file in miniature, and it is
the sharpest design fork in this survey: **kanidm puts the ownership record on the
server, recyclarr puts it on the host, and only one of those survives a restore.**

It also has **no readiness handling at all** — a plain oneshot on an `OnCalendar`
timer with no ordering against Sonarr or Radarr. It fails and retries tomorrow.
Honest, simple, and it means first-boot provisioning is not guaranteed.

### The model that dominates all of this: the service ingests the file

**Authentik's blueprints** are the right answer and are worth wanting. Authentik's
own worker applies blueprint YAML idempotently, on startup *and* on a recurring
task. The reconciler is the service's own, so: no readiness problem (it is
in-process), no credential bootstrap, no ordering, and continuous drift correction
for free. `nix-community/authentik-nix` (~181 stars) enables it with
`settings.blueprints_dir = mkDefault …`, which being `mkDefault` can be pointed at
your own directory.

**If a target service has anything like this, it beats curling its API**, and it
is the first thing to ask an upstream for. The friction is pure Nix plumbing —
blueprint files may not be symlinks and must end in `.yaml`, so the store-symlink
trick fails and the workaround is `runCommandLocal` with `cp -rL`; see
authentik-nix#98, still open.

Two negative examples of the same model show its failure mode, which is **no
pruning and no re-apply**:

- **`services.keycloak.realmFiles`** passes JSON to `kc.sh start --import-realm`,
  and the option documentation is the argument against boot-time file ingest:
  "**If a realm already exists in the server, the import operation is skipped.**"
  Create-if-absent, once, forever. Change the file and nothing happens. It is
  declarative on first boot and imperative thereafter. (The real tool for keycloak
  is `adorsys/keycloak-config-cli`, ~1211 stars, which pushes YAML over the admin
  REST API, is idempotent, needs no restart — and whose deletion support,
  `import.remote-state.enabled`, **stores its remote state inside Keycloak so it
  purges only what it manages.** That is kanidm-provision's server-side tracking
  group, independently reinvented. It is *packaged* in nixpkgs and has **no NixOS
  module** — the packaging happened, the module did not.)
- **grafana's file provisioning has the same hole**: removing a datasource entry
  and rebuilding does not remove the datasource; you maintain a separate
  `deleteDatasources` tombstone list. And the file schema moves anyway —
  `provision.notifiers` was removed outright in Grafana 11
  (`mkRemovedOptionModule`).

### Home Assistant's `.storage`, for the record

Worth stating because it is the same problem one aspect over, and because it means
this design has a second customer. HA's helpers (`input_*`) are UI-only and live
as JSON in `.storage`; HA accepts JSON there but **stopped accepting YAML as
input**, so they cannot be declared (home-assistant/core discussions#2906 is the
upstream ask). HA *does* have a websocket and REST API that can create them, and
**nobody has built the shim.** Current practice everywhere, including here, is:
declare what YAML allows, back up `.storage`, click the rest.

What *was* made declarative is instructive: blueprints
(NixOS/nixpkgs#377688, merged 2025-02-03) are supplied as **store symlinks**, and
the review settled on removing store-pointing symlinks before adding new ones so
stale blueprints do not accumulate. That is a minimal ownership record for the
file model — "anything here that is a symlink into /nix/store is mine" — and it is
the same idea as kanidm's tracking group with the record kept in the filesystem.

### The only generic framework, and the other school

**`applicative-systems/declarative-runtime`** (~34 stars, MIT, created mid-2026)
is the one real candidate for a generic layer, and its framing is almost word for
word this document's premise: upstream modules configure a service's static
surface and deliberately do not manage runtime state; here you declare the desired
runtime state in Nix and a unit applies it against the live local instance after
the primary unit starts. Mechanism: generate `.tf.json` from Nix with
`builtins.toJSON` (explicitly no HCL, no terranix), run **OpenTofu** in a
`Type=oneshot` unit `After=` the service, gated on a readiness probe, re-applying
when the generated config changes. Namespace is `services.<svc>.runtime.*`, chosen
so a pairing reads as an extension of the upstream module — a good convention.
Pairings exist for Forgejo (15 resource types) and Keycloak (~95).

Three of its stated design decisions are worth adopting verbatim:

- **Run-once, not a drift timer**: "a failed apply fails that unit visibly
  (`systemctl status`) without tearing down the service."
- **State is local and per-host**, co-located under the service's own state dir,
  with no remote backends — deliberately scoped so it cannot become shared mutable
  infrastructure.
- **Testing**: "Behavior is proven with NixOS VM integration tests
  (`runNixOSTest`): boot a VM with the pairing enabled, let the reconciler apply,
  then assert the runtime state by querying the live service API — not the
  Terraform state. Eval-only or build-only success is not evidence the
  reconciliation works."

It is too young and too narrow to depend on — 34 stars, two pairings, and it only
helps for a service that has an OpenTofu provider, which Music Assistant does not.
Borrow the design decisions.

**The other school is Terraform/OpenTofu directly**, with `terranix` (~539 stars)
rendering `config.tf.json` from Nix. Providers exist for Grafana, Keycloak,
Authentik and the Servarrs, so it genuinely covers most of these targets, and it
buys real things: plan/apply, diffs, drift detection, a dependency graph, import
of existing resources, and somebody else maintaining the schemas.

It should be **rejected here, on purpose**. The reasons, in order:

1. **The state file.** An out-of-band mutable artifact that has to be stored,
   backed up, locked and kept consistent with a host that is otherwise rebuilt
   from a flake. Lose it and you lose ownership tracking. Everything in this repo
   is derived from one evaluation; a Terraform state is the opposite of that.
2. **It is a second deploy mechanism**, so `nixos-rebuild switch` stops being the
   thing that converges the machine.
3. Provider binaries are frequently not in nixpkgs and have to be vendored.
4. Secrets land in state in plaintext unless you are careful.

`declarative-runtime`'s compromise — tofu as an implementation detail, state
pinned per-host beside the service, invoked from a systemd unit, surface exposed
as ordinary NixOS options — is the most interesting synthesis available, and the
middle path worth citing.

(The Kubernetes analogy, for one line of framing: `kubenix` and `nixidy` are
Nix→manifests, and the **reconciler is supplied by the platform** — the control
loop, or Argo CD, whose pruning also depends on ownership labels on live objects.
k8s users write only the declaration because the cluster brings a reconciler. On a
NixOS host there is no reconciler, and that is precisely the thing being proposed
here.)

Nothing comparable exists for Music Assistant. There is no
`declarative-music-assistant`, no `music-assistant-provision`, and no CLI.
`arion` is confirmed not this (it generates compose files). `nix-rest-provision`
does not exist.

### The consensus, which is folklore rather than doctrine

There is no canonical blog post or RFC. What exists is a consistent set of answers
in help threads plus two in-tree implementations. The single most on-point thread
is Discourse's **"Dealing with configurations that are set in a database"**
(discourse.nixos.org/t/.../64483), whose answer is this document's whole premise:

> If you'd actually like to have the configuration be declarative, you'll need to
> run scripts on every launch that use whatever APIs your services expose.

and, on why not an activation script:

> If they fail, you can't update or even boot your system.

**A systemd oneshot, never an activation script** is the one point of unanimity. A
oneshot fails visibly and locally, is independently restartable, is debuggable
after the fact, and cannot brick a rebuild. The shape everyone converges on is
`<svc>-init`/`<svc>-provision`, `Type=oneshot`, `RemainAfterExit=true`, ordered
after the service, a bounded readiness poll, then diff-and-apply with secrets via
`LoadCredential`. `restartTriggers` is the agreed way to re-run on a changed
declaration without a timer. And the honest community baseline, from
"Tips on idempotency": "all of your options are hacky and not-Nixy, because that's
the nature of mixing imperative and declarative management."

The reported pitfalls, each of which the design below has to answer:

1. **Readiness is not liveness.** A bound port, a 200, and a working API are three
   different propositions. Syncthing's init loop checks `Content-Type` because
   during the LevelDB→SQLite migration a *different, temporary* API answers on the
   same port with `text/plain`. Check a semantic endpoint, inspect the response,
   bound the wait, fail loudly.
2. **Ordering hands you a credential problem you did not plan for.** Every tool
   surveyed solved it differently: syncthing *reads* the key out of `config.xml`;
   kanidm *resets* `idm_admin` through the server's offline CLI on every start;
   jellarr *stops the service and writes SQLite*. "The API has no way to mint its
   own first credential" is the common case, and Music Assistant's `POST /setup`
   is the happy exception.
3. **Secrets in the store is the easy half; secrets in argv and in state is the
   hard half** — and the real wall is an API that refuses them at all.
4. **A failed oneshot leaves the service running with stale config and nothing
   tells you.** `systemctl status` is the entire alerting story in every
   implementation surveyed.
5. **Reconcile-and-prune is structurally incompatible with any feature that lets
   the service create its own entities.** This is the subtlest finding and it is
   load-bearing. nixpkgs' syncthing module carries
   `assertion = !(cfg.overrideFolders && anyAutoAccept)` — "overrideFolders will
   delete auto-accepted folders". **Music Assistant discovers players by mDNS and
   UPnP**, which is exactly an auto-create path, so this is not hypothetical here.
6. **Pruning needs an ownership record, and where you put it is the biggest
   architectural decision.** Four observed answers: server-side marker (kanidm,
   keycloak-config-cli, Argo CD labels, HA's store-symlink test — independently
   reinvented at least four times, which is as close to a verdict as this survey
   offers); local cache (recyclarr — lose it, lose pruning); full state file
   (OpenTofu); or **no pruning at all**, which is nixpkgs' own `ensure*`
   convention. `services.postgresql.ensureDatabases` says it outright: "This
   option will never delete existing databases… have to be removed manually."
   **If an option is called `ensure*`, users expect no pruning; if it prunes, it
   must not be called `ensure`.**
7. **Eval success proves nothing.** Assert against the live service API in a
   `runNixOSTest` VM. declarative-jellyfin has 12 VM tests, jellarr has per-type
   module tests plus integration tests, kanidm and syncthing have `nixosTests`.
   Anything in this space without a VM test is unverified — including the sketch
   below.

### Dependency verdict

| | depend? | why |
|---|---|---|
| nixpkgs' `syncthing.nix` | **copy, do not depend** | Not a library, but the reference implementation for readiness, pruning, secret injection and post-apply restart. |
| `kanidm-provision` + `services.kanidm.provision` | **depend** (we already do) | Canonical. Steal the server-side tracking group if we ever prune. |
| `utils.genJqSecretsReplacement` | **depend** | A nixpkgs primitive we should be using and are not. Used below. |
| `recyclarr` / `services.recyclarr` | **depend, if the *arr side grows** | Do not copy its local ownership cache. |
| `jellarr` | n/a here | Cite the migration; steal the `dump` idea. |
| `declarative-jellyfin` | **no** | Author-deprecated, pinned to eight releases of Jellyfin's internals. The cautionary tale. |
| `authentik-nix` / blueprints | **prefer, where available** | The model to ask upstreams for. Not available for MA. |
| `keycloak-config-cli` | package yes, module missing | Noted as the pattern of "packaged but unwired". |
| `declarative-runtime` | **watch, borrow design** | Only generic framework; too young, and needs an OpenTofu provider MA does not have. |
| terranix / OpenTofu directly | **no** | The state file defeats the point. |
| anything in `nix-community` | **does not exist** | Confirmed by org search. This is the gap. |

## Music Assistant, measured

All of the following is read from `music-assistant/server` **2.10.6** — the tag
`pkgs/by-name/mu/music-assistant/package.nix` builds on this flake's nixpkgs —
fetched to `/nix/store/pfsn5d6f6a8crbmy985jgjb4392wck4p-source`. Paths below are
relative to that.

### There is a plain HTTP endpoint. We do not need websockets.

`music_assistant/controllers/webserver/controller.py:320-356` is the route table:

```
GET    /info                     unauthenticated server info
POST   /api                      JSON-RPC: every API command
GET    /ws                       websocket
POST   /auth/login               { provider_id, credentials }
GET    /setup   POST /setup      first-admin creation
GET    /api-docs/openapi.json    generated from the live command registry
GET    /api-docs/commands.json   every command and its signature
```

`POST /api` takes a `CommandMessage` and returns **the bare serialized result,
not an envelope** (`controller.py:813-818`), with errors as plain HTTP status plus
a text body — `400 Invalid Command: <cmd>`, `403` on insufficient permissions,
`503 Setup required` when no user exists yet. The body is:

```json
{"message_id": "1", "command": "config/providers", "args": {"include_values": true}}
```

and a missing `message_id` is forgiven (`controller.py:780-787`). So the whole
thing is `curl` and `jq`, exactly like prowlarr, and the Python client
(`python3Packages.music-assistant-client`, which `aspects/services/audiobookshelf.nix:59`
already notes is in HA's environment) is **not needed**: it is a websocket client,
it requires Python ≥ 3.13, and the server package pins
`music-assistant-models` to 1.1.205 locally while the top-level attribute tracks
something else — a version skew we would be importing for no gain. Use `curl`.

Also useful: `GET /api-docs/commands.json` and `/api-docs/openapi.json` are
generated **from the running server's own command registry**, so the authoritative
command list for the exact version deployed is available on the box. That is the
Music Assistant equivalent of prowlarr's `/applications/schema` trick, and the
provisioner should be checked against it rather than against this document.

### Authentication is mandatory, and the bootstrap is headless

`controllers/webserver/README.md` is unambiguous: "**Mandatory authentication**:
All API access requires authentication (except Ingress)." There is no
unauthenticated-LAN mode. Running natively rather than as an HA add-on — which is
what we do, and what `music-assistant.nix`'s long `hass` comment is about — means
Ingress does not apply either.

The good news is that the bootstrap needs no browser:

**`POST /setup`** (`controller.py:1288-1363`) takes `{"username", "password",
"device_name"}`, requires username ≥ 2 and password ≥ 8 characters, creates the
first user as `UserRole.ADMIN`, **and returns a token in the same response**:

```json
{"success": true, "token": "…", "user": {…}}
```

It refuses with `400 {"success": false, "error": "Setup already completed"}` once
any user exists.

**`POST /auth/login`** (`controller.py:993-1040`) takes
`{"provider_id": "builtin", "credentials": {"username", "password"}}` and returns
the same shape; `403 Setup required` if no users, `401` on bad credentials, with
progressive-delay rate limiting.

**`auth/token/create`** (`controllers/webserver/auth.py:1085`) mints a long-lived
(1 year, no auto-renew) token.

The consequence for the design is good and slightly surprising: **we do not need
to store a token at all.** A login token is a 30-day sliding-window credential, we
get a fresh one on every run, and the provisioner is a single process that uses
it and exits. So the only persistent secret is the *password*, which can be an
ordinary generated agenix secret, and there is no provisioned-credential-written-back
problem — which is exactly the `age.secrets` cycle the cloudflare note warns
about, avoided by not creating it.

One thing to note: `config.providers.write` and `config.core.write` are reserved
to the builtin ADMIN role, so the provisioning user must be an admin, not a
`SERVICE` user.

### You cannot add a provider by writing its config. You drive a flow.

This is the finding that shapes everything, and it is specific to 2.10.x.
`controllers/config/providers.py:279-301`:

```python
@api_command("config/providers/save", required_scope=Scope.CONFIG_PROVIDERS_WRITE)
async def save_provider_config(self, provider_domain, values, instance_id=None):
    """
    Adding a new instance goes exclusively through the setup flow
    (``config/providers/setup``); this endpoint only updates an existing instance.
    """
    if instance_id is None:
        msg = "Adding a provider is only possible through the setup flow"
        raise ValueError(msg)
```

The add path is `config/providers/setup(provider_domain)`
(`controllers/config/flows.py:119-169`), which returns a `SetupFlowStep`. And here
is the part that makes the whole thing tractable:

```python
flow_module = await self._get_setup_flow_module(manifest)
if flow_module is None:
    # zero-input provider: create the instance immediately and report it
    # as an (already) finished flow
    config = await self._create_provider_instance(provider_domain, {})
    return self._synthesized_step(FlowStepType.FINISH, owner,
        step_id=..., result={"instance_id": config.instance_id})
```

A provider is "zero-input" exactly when it ships no `setup_flow.py`
(`flows.py:603-620`), and for those **one call creates and loads the instance**.
For the rest you get a step and drive it with `config/flows/submit(flow_id,
values)` until it terminates. The step shape is fully determined
(`models/setup_flow.py:525-560`):

```
{ flow_id, step_id, type, entries[], errors{}, last_step, url,
  progress_text, progress, image, expires_at, result, reason,
  translation_owner, translation_params }
```

with `type` one of `FORM`, `EXTERNAL`, `PROGRESS`, `FINISH`, `ABORT`. A `FORM`
step is submittable from a script. An `EXTERNAL` step is a URL a human must open.
That single distinction decides what we can and cannot declare.

Three more facts that matter:

- **`instance_id` is server-generated**, and for a single-instance provider it is
  *equal to the domain* (`providers.py`, `_create_provider_instance`): `chromecast`,
  `sonos`, `dlna`. Multi-instance providers get `<domain>--<shortuuid>`, which is
  not declarable — so for those the identity key has to be the declared `name`.
- **`setup` is idempotent enough to be safe to call blindly.** A non-`multi_instance`
  provider that already exists returns `ABORT` with `reason: "already_configured"`
  (`flows.py:142-144`), which the provisioner should treat as success. A deprecated
  provider aborts with `provider_retired`; one whose `depends_on` is unsatisfied
  aborts with `missing_dependency`.
- **The API path validates; a file write does not.** `_create_provider_instance`
  persists, then `await self.mass.load_provider_config(config)`, and on *any*
  exception removes the config again. So a provider whose credentials do not work
  is rejected and leaves nothing behind. The one-shot gets a real pass/fail, which
  is a feature.

### Which of our sixteen providers can actually be provisioned

`aspects/services/homeassistant/music-assistant.nix` declares sixteen providers
(plus `audiobookshelf`, appended from its own aspect). Classified by whether
`music_assistant/providers/<domain>/setup_flow.py` exists and, if it does, which
step types its `run_setup` publishes:

| provider | flow | provisionable | needs a secret |
|---|---|---|---|
| `airplay` | none | **yes**, one call | no |
| `airplay_receiver` | none | **yes**, one call | no |
| `chromecast` | none | **yes**, one call | no |
| `dlna` | none | **yes**, one call | no |
| `sonos` | none | **yes**, one call | no |
| `universal_group` | none | **yes**, one call | no |
| `lastfm_recommendations` | none | **yes**, one call | no |
| `ambient_sounds` | none | **yes**, one call | no |
| `party` | none | **yes**, one call | no |
| `smart_fades` | none | **yes**, one call | no |
| `milkdrop_visualizer` | none | **yes**, one call | no |
| `filesystem_local` | one FORM | **yes**, two calls | no |
| `vban_receiver` | one FORM | **yes**, two calls | no |
| `audiobookshelf` | one FORM | **yes**, two calls | yes (an ABS API key) |
| `hass` | one FORM | **yes**, two calls | yes (an HA long-lived token) |
| `lastfm_scrobble` | FORM then `session.external` | **no** — browser OAuth | — |
| `tidal` | `session.external_until` | **no** — OAuth device flow | — |

So **fifteen of seventeen are provisionable, and thirteen of those need no secret
at all.** That is a far better ratio than the current comment in the aspect
implies, and it is the whole case for doing this.

The two that cannot be: `tidal` is the OAuth device flow, and its own module
docstring says why it has to be interactive —

> Tidal is linked with the OAuth device flow. It has no browser callback (Tidal's
> authorize page cannot redirect back to Music Assistant), so completion is
> detected by polling the token endpoint.

— blocking in `session.external_until(...)` on a `link.tidal.com/<userCode>` URL
until a human approves. `lastfm_scrobble` is a form followed by
`session.external(url, step_id="auth")`, the same shape. Neither is a gap we can
close; both should be declared as **expected present** and *asserted*, not
created, so that a missing one is a warning rather than a silent absence.

`hass` is the interesting middle case. Its form
(`providers/hass/setup_flow.py:39-62`) is `url`, `verify_ssl`, and —

```python
# lets a user paste a long-lived token here to skip OAuth entirely
ConfigEntry(key=CONF_AUTH_TOKEN, type=ConfigEntryType.SECURE_STRING,
            required=False, advanced=True),
```

— so supplying the token bypasses `_authenticate`'s OAuth entirely. `hass` is
therefore fully provisionable *if we hold an HA long-lived access token*. We do
not, and `state.nix` records why: the token has no declarative mint and lives only
in `.storage`. See the costs section; this is the one piece of Phase-B work here.

Note also `hass_players`, which is **not** currently in our provider list: it is
zero-input but `depends_on: "hass"`, so it would abort with `missing_dependency`
until `hass` is configured. Any ordering within the desired set has to respect
`depends_on`.

### `settings.json` exists, and we are not going to write it

Music Assistant persists everything in one file. `controllers/config/controller.py:71`:

```python
self.filename = os.path.join(self.mass.storage_path, "settings.json")
```

`storage_path` is the `--data-dir` argument, and `--config` is an **alias** for it
(`__main__.py:54-61`), so the nixpkgs module's default
`extraOptions = [ "--config" "/var/lib/music-assistant" ]` puts the file at
`/var/lib/music-assistant/settings.json` — i.e.
`/var/lib/private/music-assistant/settings.json` under `DynamicUser`, which is the
path `state.nix` already snapshots. (Without that default it would be
`$HOME/.musicassistant/`; the module sets `HOME` and no `XDG_DATA_HOME`. The
default `extraOptions` is doing real work and should not be overridden without
re-adding `--config`.)

Top-level shape, from the `CONF_*` constants in `music_assistant/constants.py`
used as flat keys:

```jsonc
{
  "server_id": "<uuid4 hex>",
  "encryption_key": "<base64 Fernet key>",
  "encryption_key_migrated": true,
  "onboard_done": true,
  "providers": { "<instance_id>": { … } },
  "players":   { "<player_id>":   { … } },
  "core":      { "<domain>":      { … } }
}
```

Writing it from Nix is ruled out, for four independent reasons, any one of which
would be enough:

**1. Secrets are encrypted at rest under a key generated on first boot.**
`controller.py:226-264`:

```python
def encrypt_string(self, str_value: str) -> str:
    if str_value.startswith(ENCRYPT_SUFFIX): return str_value
    return ENCRYPT_SUFFIX + self._fernet.encrypt(str_value.encode()).decode()

def _load_or_create_encryption_key(self) -> Fernet:
    encryption_key = self.get(CONF_ENCRYPTION_KEY, "")
    if isinstance(encryption_key, str) and encryption_key:
        try: return Fernet(encryption_key.encode())
        except ValueError: LOGGER.warning("Stored encryption key is invalid; generating a new one")
    encryption_key = Fernet.generate_key().decode()
    self.set(CONF_ENCRYPTION_KEY, encryption_key)
    return Fernet(encryption_key.encode())
```

`ENCRYPT_SUFFIX = "_encrypted_"`. Every `SECURE_STRING` value is Fernet ciphertext
(AES-128-CBC plus HMAC-SHA256), under a key Nix cannot compute and did not choose.
And the failure mode is the bad kind: an invalid or non-string `encryption_key`
**does not crash startup** — a fresh key is generated and every existing secret
becomes permanently undecryptable. A Nix render that clobbered the file would
silently destroy every credential in it.

**2. The running process will overwrite you.** `_data` is held in memory and
flushed on a debounce timer (`DEFAULT_SAVE_DELAY`) and on shutdown. A write while
MA is running is lost. So this route requires stopping the unit, writing, and
restarting — three ordering problems instead of none.

**3. There is no canonical rendering.** `to_raw()` persists only values that
*differ from their default* and drops UI-only entries, so the file is sparse and
"the same configuration" has many valid encodings. There is nothing to diff
against.

**4. A file write skips everything the API does**: no `validate()`, no
`load_provider_config` with its rollback, no `update_config` on the live instance,
no dependent unload, no `PROVIDERS_UPDATED` event, no `rehydrate_provider_config`.
The provider would sit in the file and not run, and nothing would say so.

So: **never write `settings.json`.** The file is state to be backed up, which is
what `state.nix` already does, and nothing else.

### Secrets cannot be read back, so they cannot be diffed

One more constraint, and it decides the idempotency scheme for the two providers
that need a credential. `music_assistant_models`' `Config.__post_serialize__`
replaces every non-empty `SECURE_STRING` with the constant
`SECURE_STRING_SUBSTITUTE` before serialization — the string
`"this_value_is_encrypted"`, which the server's own providers test against
(`providers/lastfm_scrobble/__init__.py:77`,
`providers/listenbrainz_scrobble/__init__.py:73`). The API will also **reject
pre-encrypted input**: you hand it plaintext and the server encrypts, which
`providers/fastmcp_server/config_io/secret_handler.py` states as a rule —

> The provider performs NO encryption: plaintext is handed to MA's `save_*_config`,
> which encrypts via `Config.to_raw()` before persisting.

A reconcile loop therefore cannot ask "is the stored token the one I want?" The
answer is always `"this_value_is_encrypted"`. The fix is paperless's: key a state
file on a **hash of the declared value** and push only when it changes. Store a
`sha256` of the secret, not the secret — a sha256 of a high-entropy token is not a
meaningful disclosure, and the file lives in the unit's `StateDirectory` at 0600
either way.

(As an aside, MA itself ships the nearest thing it has to a declarative config
surface: `providers/fastmcp_server/` exposes read/write config tools, with a
`config_io/differ.py` and that `secret_handler.py`, for an LLM to drive over MCP.
It has the same ceiling we do — it cannot create a provider instance — which is
independent confirmation that the setup flow is genuinely the only create path.)

### Player configuration is a different, harder problem

`config/players/save(player_id, values)` exists
(`controllers/config/players.py:495`), but `player_id` is minted when a player is
*discovered* — by mDNS, UPnP, or the provider's own protocol. There is no
declarable player id, and the only stable handle is the player's name, which the
device reports and a user can change. So player settings are **not declarable in
the same sense**: the most that can honestly be built is a best-effort match on
name, which will silently do nothing when a speaker is renamed or offline at
activation time.

The recommendation is to leave player configuration out of scope, declare provider
instances only, and revisit if MA ever grows a stable player identity. Being
explicit about this is better than a half-working mechanism that silently misses.

## The design

### A quirk, not a capability — and the reason is the take body

The brief suggested this should be a capability, something a service TAKES
("provision me over my API"). On the evidence of `aspects/base/capability.nix` and
`aspects/base/capability-types.nix` that is the wrong half of the model, for one
decisive reason and two supporting ones.

**The decisive one: a take is a plain attrset with no `config` in scope.** A
desired-state document is not. Music Assistant's needs
`config.age.secrets."music-assistant/hass-token".path`, the loopback URL of a
sibling service, and a media path; prowlarr's needs
`config.gateway.services.<n>.url` and an env template's path. `aspects/network/cloudflare.nix:1205-1220`
already hit exactly this wall and recorded the resolution: most DNS records come
from `takes.dns.records`, but "a DKIM public key lives in a file whose path
contains the host's name, and a take is a plain attrset with no host in scope", so
the host-dependent ones are appended by the owning aspect through a NixOS option
instead. For API provisioning *every* document is host-dependent, so the
exception would be the rule.

A quirk emission is a **function of `config`** — `backup = { config, ... }: { … }`
in `state.nix` — which is precisely what is needed.

**Supporting: there is no edge here.** A capability is a relation between a giver
that can do something and a taker that can reach it, and resolution exists to pick
a carrier. The provisioner runs on the same box as the service and dials loopback,
always. `reach`, `expose`, `bindAddressFor`, `linksBetween` — none of them mean
anything for this relation. `capabilities.md`'s own rule applies: "Depend on the
smallest fixpoint that answers the question."

**Supporting: the shape is already named.** `aspects/services/samba-users.nix:34`
describes a quirk as "structured data flowing UP from many aspects into one
collector", and `aspects/services/restic.nix:180` declares `backup` as "state a
service cannot have rebuilt for it". API-only configuration is the *same category
of fact* as a backup path — something about this service that a fleet-wide
mechanism has to collect — and it belongs in the same place.

What **does** stay a capability is the part that genuinely is an edge: the
credential. An `audiobookshelf` API key presented by Music Assistant is a
`credential` between two aspects, with a giver, a taker, a generator and a scope,
and `aspects/base/capability-types.nix:797` already models exactly that. So the
split is: **the credential is a take; the document is a quirk.**

### The ownership record: there isn't one, and that is the decision

This is the biggest architectural choice available, and the survey above says it
has been made four different ways. The choice here is the fourth: **no ownership
record, and therefore no pruning.** The push is "make the declared objects exist
and match"; it is not "make the live set equal the declared set". Three reasons,
in descending order of force:

**Music Assistant creates its own entities, and pruning is structurally at war
with that.** Players arrive by mDNS and UPnP. Provider instances can be added in
the UI. nixpkgs' syncthing module had to carry
`assertion = !(cfg.overrideFolders && anyAutoAccept)` for exactly this, and MA is
the auto-accept case by construction. A prune here would delete a speaker somebody
paired last week.

**There is nowhere good to put the record.** kanidm's answer — a tracking group on
the server — has no analogue: MA has no free-form entity to label, and
`ProviderConfig` has no field we could claim. That leaves recyclarr's local cache,
which the survey is clear about: lose `/var/lib/...` and you lose both the ability
to prune and the ability to avoid duplicates.

**The gentlest policy is also the one that fits a household.** jellarr's "only
modifies explicitly configured fields" is the right scope: Nix wins on what Nix
declares, the UI wins on everything else, and nothing disappears. Jellyfin's
`forceEncodingConfig = false` default is nixpkgs saying the same thing — the
service wins, loudly.

The consequence for naming, which the survey makes a rule: **an option that does
not prune must not promise that it does.** `services.postgresql.ensureDatabases`
sets the convention ("This option will never delete existing databases"), and this
repo already follows it on the postgres quirk. So the quirk is `api-provision` and
the Music Assistant field is `ensureProviders`, not `providers`. If pruning is
ever wanted it needs a second, differently-named switch and a place to keep the
record, and that is a separate design.

### The shared half

```nix
# aspects/base/api-provision.nix  — SKETCH, NOT WRITTEN
{ den, lib, ... }:
# Configuration a service keeps only behind its own API: there is no file to
# render, so it is pushed by a one-shot unit after the service starts.
#
# The quirk collects WHAT to push (a function of `config`, because every such
# document reads a secret path or a sibling's loopback URL -- which is why this
# is a quirk and not a `takes.<cap>`; see ../network/cloudflare.nix:1205 on take
# bodies having no host in scope).
#
# This file owns only the parts that are the same for every service: the unit,
# the ordering, the readiness poll, the credential plumbing and the failure
# policy. The reconcile body is irreducibly per-service -- what makes two objects
# "the same object" is a fact about the service, not about provisioning (see
# ../services/prowlarr.nix:144 on matching an application by `implementation`
# rather than by `name`).
let
  inherit (den.lib.policy) pipe;

  norm =
    e:
    let
      name =
        e.name
          or (throw "api-provision quirk: an entry has no `name`; it names the unit and every error below");
    in
    {
      inherit name;
      # The unit that has to be up first. Defaults to the obvious guess so the
      # common case says nothing.
      unit = e.unit or "${name}.service";
      # EXACTLY ONE of these. A URL is the common case; a command is for a
      # service whose readiness is not an HTTP GET.
      readyUrl = e.readyUrl or null;
      readyCommand = e.readyCommand or null;
      readySeconds = e.readySeconds or 120;
      # THE DESIRED STATE, as a plain attrset -- NOT a rendered file.
      #
      # A secret appears in it as `{ _secret = "<path on the host>"; }` at any
      # depth, which is nixpkgs' own convention and lets
      # `utils.genJqSecretsReplacement` do the whole job: the store holds a
      # placeholder, the function hands back both the `LoadCredential` list and
      # the jq program that splices the real values in at run time. Reinventing
      # that by hand was the first draft of this and it was worse.
      desired = e.desired or null;
      path = e.path or [ ];
      reconcile =
        e.reconcile
          or (throw "api-provision quirk: '${name}' declares no `reconcile`; there is nothing generic to run");
      after = e.after or [ ];
      # `fail`  -- a rejected document fails the unit, and `nixos-rebuild`
      #            reports it. The default, because a silent non-push is worse.
      # `retry` -- keep trying. For a service whose dependency is legitimately
      #            slow or absent at activation (a speaker that is not on yet).
      onFailure = e.onFailure or "fail";
    };
in
{
  den.quirks.api-provision.description =
    "configuration a service keeps only behind its own API, reconciled by a one-shot unit after it starts";

  den.policies.api-provision-sources = _: pipe.from "api-provision" [ (pipe.collectAll (_: true)) ];

  den.aspects.api-provision = {
    includes = [ den.policies.api-provision-sources ];

    nixos =
      {
        api-provision,
        config,
        pkgs,
        lib,
        utils,
        ...
      }:
      let
        entries = map norm (lib.flatten api-provision);

        # `{ script; credentials; }` -- nixos/lib/utils.nix:398 of this flake's
        # nixpkgs. `credentials` is the LoadCredential list; `script` reads each
        # one and writes the resolved document to `output`.
        secrets =
          e:
          utils.genJqSecretsReplacement { loadCredential = true; } e.desired
            "/run/${e.name}-provision/desired.json";

        # Readiness against the API, not the unit. `systemctl is-active` is the
        # cross-unit stand-in for influxdb2's `kill -0 "$MAINPID"`: do not spin
        # for two minutes against a corpse.
        wait = e: ''
          count=0
          until ${
            if e.readyCommand != null then
              e.readyCommand
            else
              ''curl -fsS --max-time 2 ${lib.escapeShellArg e.readyUrl} >/dev/null 2>&1''
          }; do
            if [ "$count" -ge ${toString e.readySeconds} ]; then
              echo "${e.name}: API not ready after ${toString e.readySeconds}s" >&2
              exit 1
            fi
            if ! systemctl is-active --quiet ${e.unit}; then
              echo "${e.name}: ${e.unit} is not active; giving up rather than waiting out the timeout" >&2
              exit 1
            fi
            sleep 1
            count=$((count + 1))
          done
        '';

        # ── THE PAPERLESS TRICK, FOR A VALUE THE API WILL NOT READ BACK ──
        #
        # Music Assistant returns the literal "this_value_is_encrypted" for
        # every SECURE_STRING, so a reconcile loop cannot ask "is the stored
        # token the one I want". nixpkgs' paperless module answers this by
        # keying a state file on the DECLARED value and re-applying only when
        # that changed; this is the same, per top-level key of the document,
        # keyed on a sha256 of the RESOLVED sub-document so it covers a rotated
        # secret and a changed plain value alike.
        #
        # A sha256 of a high-entropy secret is not a meaningful disclosure, and
        # the value itself never reaches this file. It lives in StateDirectory
        # at 0600 and is re-derived if lost -- it is a cache, not state, and is
        # deliberately NOT an ownership record (see above).
        marker = ''
          changed() {
            local key="$1" m="$STATE_DIRECTORY/$key.sha256" now
            now=$(jq -cS --arg k "$key" '.[$k]' "$desired" | sha256sum | cut -d' ' -f1)
            [ "$(cat "$m" 2>/dev/null)" = "$now" ] && return 1
            printf '%s' "$now" > "$m"
            return 0
          }
        '';

        script =
          e:
          pkgs.writeShellApplication {
            name = "${e.name}-provision";
            runtimeInputs = [
              pkgs.coreutils
              pkgs.curl
              pkgs.jq
              pkgs.systemd
            ]
            ++ e.path;
            text = ''
              # Rendered by ExecStartPre into RuntimeDirectory (tmpfs), because
              # at this point it contains plaintext secrets and must not touch
              # the store or StateDirectory.
              desired=/run/${e.name}-provision/desired.json
              ${marker}
              ${wait e}
              ${e.reconcile}
            '';
          };

        # NOTE: `script e` is also what goes on the PATH below, which makes it
        # hand-runnable -- but only after the unit has rendered the document
        # once, because it reads `/run/<name>-provision/desired.json` rather
        # than rendering it itself. That is a deliberate split (the render needs
        # `$CREDENTIALS_DIRECTORY`, which only systemd provides) and it is the
        # one rough edge in this skeleton: run it by hand before the unit has
        # ever started and it fails on a missing file rather than saying so.
        # `systemctl start <name>-provision` is the supported form.
      in
      {
        systemd.services = lib.listToAttrs (
          map (
            e:
            lib.nameValuePair "${e.name}-provision" {
              description = "Reconcile ${e.name}'s API-only configuration";
              after = [ e.unit ] ++ e.after;
              # `requisite`, not `requires`: run AFTER it, never CAUSE it.
              # nixpkgs' syncthing-init is the precedent, and it is the only
              # nixpkgs API-pusher whose failure the service survives.
              requisite = [ e.unit ];
              wantedBy = [ "multi-user.target" ];
              # ── RE-RUN WHEN THE DECLARATION CHANGES ──
              #
              # A `RemainAfterExit` oneshot that already succeeded is not
              # restarted by activation, so without this a changed document
              # does nothing until the next reboot. The declaration is hashed
              # into the unit through the secret-replacement script, which is
              # the agreed idiom (discourse 70853).
              restartTriggers = [ (secrets e).script ];
              serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
                ExecStartPre = pkgs.writeShellScript "${e.name}-provision-render" (secrets e).script;
                ExecStart = lib.getExe (script e);
                RuntimeDirectory = "${e.name}-provision";
                RuntimeDirectoryMode = "0700";
                StateDirectory = "${e.name}-provision";
                UMask = "0077";
                LoadCredential = (secrets e).credentials;
              }
              // lib.optionalAttrs (e.onFailure == "retry") {
                Restart = "on-failure";
                RestartSec = 30;
              };
            }
          ) entries
        );

        # Runnable by hand, which is most of what makes one of these debuggable.
        environment.systemPackages = map script entries;
      };
  };
}
```

Two notes on that skeleton, both load-bearing:

`StartLimitIntervalSec` deliberately is not set for `retry`; systemd's default
start-rate limiting will eventually give up, which is the right behaviour for a
genuinely broken document. And `onFailure = "fail"` does not literally fail
`nixos-rebuild`'s *configuration* — the activation reports failed units and newer
`switch-to-configuration` exits non-zero on them, which is the honest "you find
out" behaviour. Anything stronger (pre-flight validation at activation) is not
available for a push that needs the service running.

### The per-service half, for Music Assistant

Everything below is the `reconcile` body and the `desired` document. Nothing in it
is generic, and that is the point.

```nix
# in aspects/services/homeassistant/music-assistant.nix  — SKETCH, NOT WRITTEN
#
# `den.aspects.music-assistant` gains one `api-provision` emission and one
# generated secret. No `takes.credential`: the two secrets here are its OWN
# (the provisioning password it generates, and a hand-made HA token), not an
# edge with another aspect. The one that WOULD be a `credential` is
# audiobookshelf's API key, and that belongs in ../audiobookshelf.nix beside the
# `providers` entry it already appends.
{
  # The provisioning admin's password. A plain generated secret -- the only
  # persistent credential this needs, because a login token is minted fresh on
  # every run and thrown away. NOT routing a provisioned token back into
  # `age.secrets` is the whole reason there is no cycle here; see
  # ../../network/cloudflare.nix:49.
  secrets."admin-password".generator.script = "random-hex-32";

  api-provision =
    { config, pkgs, ... }:
    {
      name = "music-assistant";
      readyUrl = "http://127.0.0.1:${toString musicAssistantPort}/info";
      # A chromecast or sonos that is not on the network yet is a legitimate
      # transient failure, not a bad document.
      onFailure = "retry";

      # ── `ensureProviders`, NOT `providers` ──
      #
      # Nothing here is ever removed from Music Assistant, so the name has to
      # say so -- the `ensure*` convention ../postgres.nix already follows, for
      # the reason `services.postgresql.ensureDatabases` states.
      #
      # A plain attrset: `_secret` leaves are paths, resolved by the collector
      # through `utils.genJqSecretsReplacement`, so the store holds a
      # placeholder and the value is read on the host.
      desired = {
        # ── ZERO-INPUT: one `config/providers/setup` call each ──
        airplay = { };
        airplay_receiver = { };
        chromecast = { };
        dlna = { };
        sonos = { };
        universal_group = { };
        lastfm_recommendations = { };
        ambient_sounds = { };
        party = { };
        smart_fades = { };
        milkdrop_visualizer = { };

        # ── ONE FORM, KEYED BY `step_id` ──
        filesystem_local.flow.user = {
          content_type = "music";
          path = "/srv/media/music";
        };

        # The turntable receiver. `sender_host` is auspc's address, which is the
        # one genuinely derived field here -- read from `fleet`
        # (../../hosts/fleet.nix), NEVER from
        # inputs.self.nixosConfigurations.auspc.config: that would make every
        # evaluation of this box evaluate auspc too, which ./prom-device.nix
        # spells out at length. The port and the stream name are the same facts
        # ../vban.nix's take already states, and should be resolved from the
        # capability rather than retyped here.
        vban_receiver.flow.user = {
          bind_port = 6980;
          vban_stream_name = "Network AUX";
          sender_host = fleet.auspc.lanAddress;
          audio_format = "S16LE";
          sample_rate = 44100;
          audio_channels = 2;
        };

        # The only entry that needs a secret, and it is PHASE B: no declarative
        # mint exists for a Home Assistant long-lived token (./state.nix), so
        # this is a hand-made static secret, and adding one needs
        # `nix run .#rekey` -- which CLAUDE.md forbids while the master key is
        # recovering. `lib.optionalAttrs` it out and the provider is skipped
        # with a warning rather than half-configured.
        #
        # The URL is LOOPBACK, not `home.<domain>`: both run on this box, so the
        # public name would send every call out through the tunnel and back
        # (../nextcloud.nix:327).
        hass.flow.user = {
          url = "http://127.0.0.1:8123";
          verify_ssl = false;
          auth_token._secret = config.age.secrets."music-assistant/hass-token".path;
        };

        # ── EXPECTED PRESENT, NEVER CREATED ──
        # Both are interactive OAuth and cannot be driven headlessly: tidal is a
        # device flow (`session.external_until` on link.tidal.com),
        # lastfm_scrobble a browser callback (`session.external`). Declared so
        # that a missing one WARNS instead of being invisible.
        tidal.assert = true;
        lastfm_scrobble.assert = true;

        # Not a provider: the credential this provisioner logs in with. Lives
        # in the same document so the collector's one secret mechanism covers
        # it, and is skipped by the loop below.
        "@admin".password._secret = config.age.secrets."music-assistant/admin-password".path;
      };

      reconcile = ''
        base="http://127.0.0.1:${toString musicAssistantPort}"

        # ── A TOKEN, MINTED FRESH AND NEVER STORED ──
        #
        # `POST /setup` creates the first admin AND returns a token, so a fresh
        # install bootstraps itself with no browser. On an install that already
        # has users it 400s and we log in instead. Either way the token is a
        # 30-day login credential this process uses once and drops, which is
        # why nothing has to write a token anywhere.
        pw=$(jq -r '."@admin".password' "$desired")
        creds() { jq -n --arg u provisioner --arg p "$pw" "$1"; }

        token=$(curl -sS -X POST "$base/setup" -H 'Content-Type: application/json' \
          --data-binary "$(creds '{username:$u, password:$p, device_name:"nixos-provision"}')" \
          | jq -r '.token // empty')

        if [ -n "$token" ]; then
          echo "music-assistant: created the first admin user 'provisioner'" >&2
          bootstrapped=1
        else
          token=$(curl -sS -X POST "$base/auth/login" -H 'Content-Type: application/json' \
            --data-binary "$(creds '{provider_id:"builtin", credentials:{username:$u,password:$p}, device_name:"nixos-provision"}')" \
            | jq -r '.token // empty')
        fi

        if [ -z "$token" ]; then
          echo "music-assistant: could not authenticate as 'provisioner'. Music Assistant" >&2
          echo "  already has users, so /setup is closed, and this password does not log in." >&2
          echo "  Create an ADMIN user named 'provisioner' in its UI with the password from" >&2
          echo "  \`nix run .#secret-view -- secrets/generated/secondpc/music-assistant/admin-password.age\`." >&2
          exit 1
        fi

        # No `curl -f` on a write: it hides the response body, and the body is
        # the only thing that says what the server objected to.
        # (../prowlarr.nix:226.)
        api() {
          local cmd="$1" args="''${2:-{\}}" resp code
          resp=$(mktemp); trap 'rm -f "$resp"' RETURN
          code=$(curl -sS -o "$resp" -w '%{http_code}' -X POST "$base/api" \
            -H "Authorization: Bearer $token" -H 'Content-Type: application/json' \
            --data-binary "$(jq -n --arg c "$cmd" --argjson a "$args" \
              '{message_id:"1", command:$c, args:$a}')")
          case "$code" in
            2*) cat "$resp" ;;
            *)  echo "music-assistant: $cmd failed (HTTP $code): $(cat "$resp")" >&2; return 1 ;;
          esac
        }

        # Clears the onboarding wizard so the UI does not greet a human with it.
        [ -n "''${bootstrapped:-}" ] && api config/onboard_complete >/dev/null || true

        existing=$(api config/providers) || exit 1

        # `@admin` is the credential above, not a provider. `depends_on` ordering
        # matters -- hass_players would abort with `missing_dependency` before
        # hass exists -- so the key order of the document is respected rather
        # than sorted, which `jq 'keys_unsorted'` is for.
        for domain in $(jq -r 'keys_unsorted[] | select(startswith("@") | not)' "$desired"); do
          want=$(jq -c --arg d "$domain" '.[$d]' "$desired")

          # Single-instance providers get `instance_id == domain`; the match is
          # on `domain` regardless, because that is the stable key and the
          # name is not (../prowlarr.nix:144).
          id=$(printf '%s' "$existing" \
            | jq -r --arg d "$domain" 'map(select(.domain == $d)) | .[0].instance_id // empty')

          # EXPECTED PRESENT, NEVER CREATED.
          if [ "$(printf '%s' "$want" | jq -r '.assert // false')" = true ]; then
            [ -n "$id" ] || echo "music-assistant: $domain is not configured, and cannot be provisioned (interactive OAuth). Add it once in the UI." >&2
            continue
          fi

          if [ -z "$id" ]; then
            step=$(api config/providers/setup "$(jq -n --arg d "$domain" '{provider_domain:$d}')") || continue

            while :; do
              case "$(printf '%s' "$step" | jq -r '.type')" in
                finish)
                  id=$(printf '%s' "$step" | jq -r '.result.instance_id // empty')
                  echo "music-assistant: added $domain ($id)" >&2
                  break ;;
                abort)
                  reason=$(printf '%s' "$step" | jq -r '.reason // "unknown"')
                  # `already_configured` is success: a non-multi-instance
                  # provider that exists aborts rather than erroring
                  # (controllers/config/flows.py:142).
                  [ "$reason" = already_configured ] && break
                  echo "music-assistant: $domain setup aborted ($reason)" >&2
                  break ;;
                form)
                  flow=$(printf '%s' "$step" | jq -r '.flow_id')
                  sid=$(printf '%s' "$step"  | jq -r '.step_id')
                  values=$(printf '%s' "$want" | jq -c --arg s "$sid" '.flow[$s] // empty')
                  if [ -z "$values" ] || [ "$values" = null ]; then
                    echo "music-assistant: $domain wants form step '$sid', which is not declared; aborting its flow" >&2
                    api config/flows/abort "$(jq -n --arg f "$flow" '{flow_id:$f}')" >/dev/null || true
                    break
                  fi
                  step=$(api config/flows/submit \
                    "$(jq -n --arg f "$flow" --argjson v "$values" '{flow_id:$f, values:$v}')") || break
                  # A FORM returned with `errors` is a rejection, not progress.
                  if [ "$(printf '%s' "$step" | jq -r '.step_id')" = "$sid" ] \
                     && [ "$(printf '%s' "$step" | jq -r '.errors | length')" != 0 ]; then
                    echo "music-assistant: $domain rejected step '$sid': $(printf '%s' "$step" | jq -c .errors)" >&2
                    api config/flows/abort "$(jq -n --arg f "$flow" '{flow_id:$f}')" >/dev/null || true
                    break
                  fi ;;
                external|progress)
                  echo "music-assistant: $domain needs a human (step type $(printf '%s' "$step" | jq -r .type), url $(printf '%s' "$step" | jq -r '.url // "-"')); not provisionable" >&2
                  api config/flows/abort "$(jq -n --arg f "$flow" '{flow_id:$f}')" >/dev/null || true
                  break ;;
                *) echo "music-assistant: $domain returned an unknown step type" >&2; break ;;
              esac
            done
          fi

          # Plain options, for both a just-created and a pre-existing instance.
          #
          # Guarded by `changed`, and the reason is specific: a SECURE_STRING is
          # returned as the literal "this_value_is_encrypted", so the live value
          # cannot be compared -- and pushing it unconditionally would call
          # `update_config` on the live provider instance on every single
          # activation. So the push is keyed on a sha256 of what WE declared,
          # which is the paperless pattern. The known cost is in the risks
          # section: this answers "did we change it", not "is it right".
          opts=$(printf '%s' "$want" | jq -c '.values // {}')
          if [ -n "$id" ] && [ "$opts" != '{}' ] && changed "$domain"; then
            api config/providers/save "$(jq -n --arg d "$domain" --arg i "$id" \
              --argjson v "$opts" \
              '{provider_domain:$d, instance_id:$i, values:$v}')" >/dev/null || true
          fi
        done
      '';
    };
}
```

### What is shared, and what never will be

| concern | shared | per-service |
|---|---|---|
| unit shape, ordering, `requisite` | ✅ | |
| readiness poll, with a liveness check and a bounded timeout | ✅ | the *predicate* (`/info` here, `/system/status` for an *arr) |
| `LoadCredential` plumbing, `StateDirectory`, `UMask` | ✅ | |
| `_secret` resolution, via `utils.genJqSecretsReplacement` | ✅ | |
| the paperless sha256 marker for unreadable values | ✅ | which keys are unreadable |
| failure policy, and re-running on a changed document | ✅ | the choice of `fail` vs `retry` |
| a hand-runnable `<name>-provision` wrapper | ✅ | |
| **authentication** | | bootstrap is wildly service-specific |
| **the identity key** — what makes two objects the same | | irreducible, and the usual bug |
| **the create path** — POST, PUT, or a flow | | irreducible |
| **schema prefill** — `/applications/schema` vs `config/flows/*` entries | | irreducible |
| **pruning** — whether Nix owns the namespace | | per-service policy |

The payoff beyond Music Assistant is concrete, and there are two other customers
already in the tree:

- **`prowlarr-connect` and the servarr `<name>-connect` unit** lose their unit
  skeleton, their readiness loop and their `api()` helper to the shared half, and
  keep only the `upsert` body — which is the only part that was ever different.
  Their `takes.credential.restartUnits` entries could also go away, because
  `LoadCredential` reads the file at start rather than holding the value in the
  environment, so a rewritten secret is picked up by the next activation without a
  restart trigger.
- **Home Assistant's `.storage` helpers** are the same problem one aspect over.
  `input_*` helpers are UI-only, HA stopped accepting YAML for them, and HA *does*
  have a websocket API that can create them — so a second emission on
  `den.aspects.homeassistant` would cover what `state.nix` currently only backs
  up. Nobody upstream has built this, which is both the opportunity and the
  warning.

### The one piece of evidence this design does not have

Every project surveyed that works has a VM test, and `declarative-runtime` states
the rule: assert the runtime state by **querying the live service API**, not the
reconciler's own state, because "eval-only or build-only success is not evidence
the reconciliation works."

nixpkgs already ships `nixosTests.music-assistant`, and its whole body is

```python
machine.wait_for_unit("music-assistant.service")
machine.wait_until_succeeds("curl --fail http://localhost:8095")
```

which is the ceiling of what exists today. Extending it — boot, let the
provisioner run, then `POST /api config/providers` and assert the instances are
there — is the only thing that would turn this document into a working mechanism,
and it is cheap: `runNixOSTest` with the aspect enabled and a document of the
eleven zero-input providers, none of which needs a secret or a network device.
**Nothing else in this plan should be implemented before that test exists**,
because every failure mode above is silent.

## What it costs

| cost | detail |
|---|---|
| a new base aspect | `aspects/base/api-provision.nix` — one quirk, one policy, one aspect, one `nixos` body. Roughly 120 lines. |
| a quirk emission per service | Music Assistant's is the document plus the reconcile body, which is the bulk of the work and is not reusable. |
| a secret | one generated `music-assistant/admin-password`. **This is Phase-B work**: a new generated secret needs `nix run .#gen-secrets` and `nix run .#rekey`, both forbidden by `CLAUDE.md` while the master key is recovering. |
| a second secret, deferred | `music-assistant/hass-token`, hand-made from Home Assistant's UI. Same Phase-B block, and the `hass` provider is cleanly skipped without it. |
| a `StateDirectory` | `/var/lib/music-assistant-provision`, one sha256 per top-level key. A cache, not state: not worth backing up, re-derived if lost, and explicitly **not** an ownership record. |
| a `RuntimeDirectory` | `/run/music-assistant-provision`, 0700, holding the resolved document with plaintext secrets for the life of the unit. tmpfs, so it does not persist. |
| a unit per provisioned service | `<name>-provision.service`, oneshot, `RemainAfterExit`. |
| a VM test | the only real evidence this works. See above; it is cheap and it is not optional. |
| one manual step, once | on an install that already has users, create an ADMIN named `provisioner` with the generated password. A fresh install needs nothing. |
| two providers stay manual, permanently | `tidal` and `lastfm_scrobble`. Asserted, not created. |
| **no** pruning | by decision, not omission — see the ownership section. A provider removed from the document stays in Music Assistant and must be removed in its UI. |
| **no** vhost, no DNS record, no OIDC client, no database | the provisioner is loopback-only and has no public surface. |
| **no** new eval cost of consequence | the document is a literal attrset plus a `fleet` read; it adds no host eval and no import-from-derivation. |

## Risks, and what is not known

- **[not evaluated]** None of this has been through an eval, and there is no VM
  test, which the section above argues is disqualifying on its own.
- **[not verified]** `utils.genJqSecretsReplacement`'s behaviour on this shape of
  document. Its signature and return set (`{ script; credentials; }`) are read
  from `nixos/lib/utils.nix:398-490`, and the `recursiveGetAttrsetWithJqPrefix`
  walk it uses to find `_secret` leaves builds a **jq path prefix from the attr
  names** — which is why the document's keys matter. `"@admin"` is a key chosen to
  sort away from provider domains and to be an invalid provider domain, but
  whether that name survives the jq-prefix generation intact is exactly the kind
  of thing to check before writing it. If it does not, the admin password goes
  back to a plain `LoadCredential` entry alongside, which costs one line.
- **The resolved document contains plaintext secrets.** It is written to
  `RuntimeDirectory` at 0700 on tmpfs, which is the same exposure as any
  `LoadCredential` consumer, but it is a file rather than a pipe and it lives for
  the life of the unit. `RemainAfterExit = true` means that is until the next
  activation, not until the script exits. Worth weighing against dropping
  `RemainAfterExit` — at the cost of the unit no longer showing as successfully
  applied in `systemctl status`, which is the entire alerting story.
- **`restartTriggers` on the render script** is what makes a changed declaration
  take effect at activation rather than at the next boot. It also means every
  evaluation that perturbs the document re-runs the push, and that a rotated
  secret's *path* changing (not its value) re-runs it too.
- **[not verified]** That `POST /setup` on a fresh native install actually
  succeeds unauthenticated from loopback. The handler is read and has no auth gate
  (`controller.py:1288`), and `has_users` is the only guard, but no request was
  made against a running server.
- **[not verified]** The exact `errors` shape a rejected FORM step returns. The
  sketch branches on `.errors | length != 0` with the same `step_id`, which is what
  `flows.py:266-290` implies, but the serialized field is in
  `music_assistant_models` and that package is not vendored in the server source —
  so `SECURE_STRING_SUBSTITUTE`, `ProviderConfig`'s field list and the lowercasing
  of `FlowStepType` are all **read from the server's uses of them, not from their
  definitions**. Check against `GET /api-docs/schemas.json` on the running box
  before implementing.
- **Secrets cannot be diffed, so drift in a secret is invisible.** If somebody
  changes the HA token in MA's UI, the sha256 marker still matches and nothing
  corrects it. The marker answers "did *we* change it", not "is it right". The only
  fix is to delete the marker, and that should be documented wherever the marker
  lives.
- **The setup flow is a stateful server-side session**, and the sketch holds one
  open across two HTTP round trips. A flow that times out between them
  (`expires_at`) leaves an abandoned session; `config/flows/abort` is called on
  every failure path above, but not on a crash. Abandoned flows are harmless but
  they accumulate until restart.
- **MA 2.10.x is young, and this design is pinned to its API.** `config/providers/save`
  gained its "update only" refusal in this line; `config/providers/get_entries`
  changed signature between 2.10.7 and `dev`; `required_scope` moved from
  `config.providers.write` to `config.providers.own` on some commands in `dev`. A
  nixpkgs bump could break the reconcile body silently — the `onFailure = "retry"`
  choice makes that a repeating log line rather than a failed activation, which is
  arguably the wrong trade for a schema break and the right one for a cold speaker.
  There is no way to have both without distinguishing error classes.
- **[not determined]** Whether `airplay_receiver`, `smart_fades` and
  `milkdrop_visualizer` tolerate being created with no values. They ship no
  `setup_flow.py`, which by `flows.py:612-619` means "needs no setup input", but
  their `__init__` may still fail to load — and a provider whose load fails is
  rolled back, so the symptom would be a provisioner that reports an error every
  run for a provider that was previously absent and fine.
- **[not determined]** Whether `filesystem_local`'s flow is genuinely one step
  here. Its `run_setup` is a plain FORM loop, but `session.mass.storage.can_hold_music_source`
  gates `finish`, and a second source overlapping an existing folder is reported to
  take an extra confirmation submit. The sketch would abort that flow with "wants
  form step 'overlap_choice', which is not declared" — visible, not silent, but it
  would need a second declared step.
- **Player configuration is out of scope and should stay there.** `player_id` is
  minted at discovery; there is no stable declarable handle. A name-matching
  mechanism would silently do nothing for an offline or renamed device, which is
  worse than not having one.
- **No pruning means the document is not a complete description.** Removing a
  provider from the Nix side changes nothing on the box, so the declaration and
  the live state can diverge in exactly one direction, silently and forever. This
  is the cost of the ownership decision and it is paid deliberately — but anyone
  reading the aspect will reasonably assume the list is exhaustive, and it is not.
  The `ensureProviders` name is the only thing warning them, and names are weak
  warnings. A `--list-unmanaged` mode on the hand-runnable wrapper, printing live
  instances the document does not mention, would be cheap and is the obvious
  follow-up. jellarr's `dump` is the same idea, done better.
- **There is no dry run.** kanidm-provision does not have one either, so this is
  consistent with the tree rather than good. The hand-runnable
  `music-assistant-provision` is the substitute, and it is not a substitute: it
  pushes.
- **The alternative that was ruled out, for the record:** writing
  `settings.json` directly. Four independent reasons are in the measurement section
  and the worst of them is that a bad `encryption_key` is accepted silently and
  destroys every stored credential. If somebody proposes this later, that is the
  sentence to read.
- **The other alternative, not ruled out and not chosen:** a flake app
  (`nix run .#sync-music-assistant`) in the shape of `sync-tunnels`, reading the
  document from outside the fixpoint. It is the right shape for a *third-party*
  API, and the wrong one here: the credential is on the host, the service is on the
  host, and an operator-run push is a thing that does not happen after a restore.
  The one thing it buys is that a broken document cannot fail an activation.
