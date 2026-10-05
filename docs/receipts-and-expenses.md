# Receipts and expenses

Four things, three edges, and the two edges that do not work are the point of
this document.

```
  paper / photo ──▶ paperless-ngx ──▶ ??? ──▶ grocy        (stock + price history)
                     (OCR, text)      │
                                      └────▶ ??? ──▶ SplitPro  (who owes whom)

  api.up.com.au ──▶ ??? ──▶ something that holds transactions
```

`paperless-ngx` is **written** (`aspects/services/paperless.nix`), wired onto
secondpc and evaluated. Both `???` on the receipt side and the one on the Up
Bank side are **designs in this document and nothing else**, because each of
them is a place where the honest answer is "this does not exist and here is
what it would cost to write".

If you read one section, read [The weakest link](#the-weakest-link).

## What was actually checked

Everything below that states a fact about upstream was read from upstream or
from this flake's own locked inputs. What was *not* done is stated too, because
an unverified claim in a document like this is worse than an absent one.

- **nixpkgs has `services.paperless`**, at
  `nixos/modules/services/misc/paperless.nix` (754 lines) — note `misc/`, not
  `web-apps/`. Find the tree with `nix eval --raw .#inputs.nixpkgs.outPath`
  and not by reading `flake.lock`'s node named `nixpkgs`: there are several
  nixpkgs nodes in that lock and the bare-named one is not the root input, which
  is the mistake `grocy.md` records making.
- **`paperless-ngx` builds at 3.3.1** in that locked nixpkgs
  (`nix eval --json '.#inputs.nixpkgs.legacyPackages.x86_64-linux.paperless-ngx.version'`).
  Its REST API advertises API version **10** as the default, with 9 still
  served.
- **The aspect evaluates.** One full-host eval of secondpc, behind the lock,
  confirms `paperless.ivymect.in` → `http://127.0.0.1:28981`, `websockets` and
  `forwardUser` both true, `api.enable` false, `extraGroups =
  [ "app-paperless-user" ]`, `paperless` in `services.postgresql.ensureDatabases`,
  `PAPERLESS_URL = https://paperless.ivymect.in`, the name present in the
  cloudflared ingress and **nothing added to `networking.firewall.allowedTCPPorts`**.
- **Up Bank's API shape** is from `developer.up.com.au` and from the
  `up-banking/api` OpenAPI document, including the webhook event schema.
- **SplitPro's lack of an API** is from the shape of `src/pages/api/` in the
  upstream tree plus its own router definitions, at the version this repo pins
  (`v2.1.5`, `_sources/generated.json`).
- **Not checked:** nothing here has been deployed, no `nixos-rebuild` was run,
  and there is **no VM test**. `api-provisioning.md`'s own rule applies —
  "Eval success proves nothing" — and that is as true of this aspect as of the
  sketches it declined to write.
- **No secret was minted.** The one thing in this document that would need a
  `rekey` is the Up Bank token, and no aspect declares it; see
  [Secrets](#secrets-declared-and-unminted).

---

# Part A — receipts

## paperless-ngx, and what it is actually for

The service does one thing extremely well and it is **not** the thing the
receipt pipeline needs most.

**What it does.** A file lands (directory watch, web upload, API POST, or an
IMAP mailbox), OCRmyPDF runs tesseract over it, and the result is a stored
original plus an archived searchable PDF plus a `content` field holding the full
OCR text. On top of that: a classifier that suggests a correspondent, document
type and tags from past decisions; regex/date matching for the same; `custom_fields`
of typed shapes (string, monetary, date, select, boolean, document link) that can
be filtered on with `?custom_field_query=`; full-text search over a Tantivy
index; per-object permissions; document versions; and a trash with a delay.

**What it does not do.** There is **no line-item extraction and no table
parser anywhere in paperless-ngx.** This was checked three ways:

- The AI features (`PAPERLESS_AI_*`) suggest *document-level* title, tags,
  correspondent and type. That is the whole of their output surface.
- `PAPERLESS_REMOTE_OCR_ENGINE` has exactly one implementation, `azureai`
  (Azure Document Intelligence). `RemoteDocumentParser._azure_ai_vision_parse`
  takes back **extracted text and a searchable PDF with an embedded text
  layer** — text, including the text that happens to sit inside a table, not
  rows with amounts attached to them.
- `custom_fields` are per-document and single-valued. There is no repeating
  group, so "line 4 of this receipt" has nowhere to live even if something
  else parsed it.

So the correct mental model is: **paperless is the place the receipt lives and
the place its text comes from. It is not a parser.** Everything downstream in
this document consumes `GET /api/documents/{id}/` and does its own parsing, or
does not happen.

## The hooks out, ranked by how declarable they are

This is the part that decides the architecture, and it maps cleanly onto
`api-provisioning.md`'s four cases.

| hook | shape | declarable from Nix? |
|---|---|---|
| **`PAPERLESS_POST_CONSUME_SCRIPT`** | a filename, invoked after a document is stored, with everything about it in the environment | **yes, completely.** It is a store path. Case 1 — "a file the service reads at startup", with no mechanism needed. |
| `PAPERLESS_PRE_CONSUME_SCRIPT` | same, but before processing; may rewrite `DOCUMENT_WORKING_PATH` | yes, same reason. Wrong hook here: it runs before OCR, so there is no text yet. |
| **REST API** (`/api/documents/`, `/api/custom_fields/`, …) | token auth | reading, yes. Writing a *configuration object* is case 4 and needs the unwritten `api-provision` mechanism. |
| **Workflows with a webhook action** | a `Workflow` row with triggers, filters, a URL, a Jinja-templated body and headers | **no.** It is a database object. Declaring it is case 4, and `aspects/base/api-provision.nix` is a sketch in `api-provisioning.md` that was never written. |
| `document_exporter` | a CLI that stops the service | yes, and deliberately off — see the aspect's `backup` note. |

**The post-consume script is the hook.** It is a store path, so it is versioned
with the system, it needs no readiness poll, no credential plumbing, no
idempotency record and no ownership marker — the four hard parts
`api-provisioning.md` identifies simply do not arise. A workflow webhook would
be strictly worse here: it buys filtering (which the script can do by reading
`DOCUMENT_TAGS`) at the cost of being a row somebody has to create in a UI and
nothing can reconcile.

What the script is handed, as environment variables (upstream moved these from
positional arguments, so anything written against the old interface is wrong):

```
DOCUMENT_ID              DOCUMENT_SOURCE_PATH     DOCUMENT_CORRESPONDENT
DOCUMENT_FILE_NAME       DOCUMENT_ARCHIVE_PATH    DOCUMENT_TAGS
DOCUMENT_TYPE            DOCUMENT_THUMBNAIL_PATH  DOCUMENT_OWNER
DOCUMENT_CREATED         DOCUMENT_DOWNLOAD_URL    DOCUMENT_ORIGINAL_FILENAME
DOCUMENT_MODIFIED        DOCUMENT_THUMBNAIL_URL   TASK_ID
DOCUMENT_ADDED
```

Note what is **not** in that list: the OCR text. `DOCUMENT_ID` is there, so the
script fetches `GET /api/documents/$DOCUMENT_ID/` and reads `content` — which
means **even the fully-declarable hook needs an API token**, and that is where
the next section bites.

**Where the script runs, which constrains what it can do.** Consumption happens
inside the celery worker, so the script inherits `paperless-task-queue`'s unit
sandbox: `ProtectSystem=strict`, `CapabilityBoundingSet=""`, `PrivateUsers`,
`ProcSubset=pid`, a `SystemCallFilter`, and `RestrictAddressFamilies` limited to
`AF_UNIX`/`AF_INET`/`AF_INET6`. The nixpkgs module pins that unit's
`PrivateNetwork = false` explicitly ("needs to talk to mail server for automated
import rules"), so outbound HTTP works. A credential for the script has to arrive
the way the unit's other configuration does — through
`services.paperless.environmentFile`, which is already an `EnvironmentFile` on
every paperless unit, or through a drop-in `LoadCredential`.

## Authentication, and one finding worth acting on

The aspect gates paperless the way `grocy.nix` does: oauth2-proxy in front
(`takes.gated.oauthProxy = true`) and the authenticated person's kanidm short
name forwarded (`forwardUser = true`), which django's `RemoteUserBackend` reads
as `HTTP_REMOTE_USER` and turns into an account on first visit.

Two things differ from grocy and both are worth knowing.

### The header here is forgeable in principle; grocy's is not

`grocy.nix` sets `REVERSE_PROXY_AUTH_USE_ENV`, which moves the middleware from
reading a request header to reading a `$_SERVER` key — and php maps an inbound
header to `HTTP_<NAME>`, so a client's `Grocy-Remote-User:` lands somewhere
the middleware never looks. That option has no analogue in django:
`PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME` must name a `HTTP_*` META key, i.e. an
inbound header, by construction.

It is safe anyway, on the browser leg, because `gateway.nix` emits

```nginx
auth_request_set $gw_person $upstream_http_x_auth_request_preferred_username;
proxy_set_header Remote-User $gw_person;
```

and `proxy_set_header` **overwrites** — a client's own `Remote-User` is
replaced, and nginx drops the header entirely when the variable is empty, so an
unauthenticated request arrives with no identity rather than with somebody
else's.

### `PAPERLESS_ENABLE_HTTP_REMOTE_USER_API` must stay false, and the reason is in this tree

Upstream split API remote-user trust out into its own flag with a warning, and
**this tree is exactly the configuration that warning is about.**

`gateway.nix` renders *two* locations for a service with `api.enable`. The UI
location carries the `proxy_set_header Remote-User` pair quoted above. The API
location does not — it rewrites the API *key* header (and the `?apikey=` query
parameter) and nothing else. So a request to `/api` through the gate keeps
whatever `Remote-User` the caller sent.

With `PAPERLESS_ENABLE_HTTP_REMOTE_USER_API = true` and an API location on the
vhost, that is authentication as any user, including a superuser, by asking.
The aspect therefore sets it **false** and does not declare `takes.nginx.api`
at all, with the reason written at the site, so that adding an API location
later cannot quietly turn it on.

**This is a general fact about the gate, not about paperless**, and it is worth
recording somewhere with more readers than this file: `forwardUser` applies to
the UI location only. Any service that trusts a forwarded identity *and* has
`api.enable` is in the same position. Today grocy is the only other
`forwardUser` taker and it has no API location, so nothing is currently wrong —
but the two options are independently settable and nothing connects them. A
reasonable follow-up is either an assertion ("`forwardUser` and `api.enable`
together require the API location to blank the header") or blanking
`Remote-User` in the API location unconditionally. Neither is done here.

### The permission gap, which is a human step and cannot be closed from Nix

Grocy's `UsersService::CreateUser` gives every auto-created user
`DEFAULT_PERMISSIONS = ['ADMIN']`, so a person who gets through the gate can
immediately use the app. **Paperless does the opposite.** A user created by
`RemoteUserBackend` gets no django groups and no model permissions, and
`PAPERLESS_ACCOUNT_DEFAULT_GROUPS` does *not* apply to them — that setting is
read by allauth's `CustomAccountAdapter.save_user`, which the remote-user path
bypasses entirely. The person signs in successfully and sees nothing.

So two manual steps survive, and the aspect names both at the `forwardUser`
take:

1. Once per install, to get a first superuser — same username the gate will
   forward, so the two are one account:
   `paperless-manage createsuperuser --username <kanidm short name>`
2. Once per person, after their first sign-in: *Settings → Users & Groups →
   them → grant permissions.*

There is no `passwordFile` in the aspect, deliberately: that is a secret, and
the whole point of the grocy-shaped arrangement is that this aspect holds none
and is therefore deployable while the master key is recovering.

### The alternative that removes step 2, and what it costs

paperless-ngx is a **django-allauth** app, so it can be a real kanidm OIDC
client:

- `PAPERLESS_SOCIALACCOUNT_PROVIDERS` — a JSON blob containing the client id
  **and secret**, so it must arrive via `environmentFile`, not `settings`
  (which lands in the unit file in the store).
- `PAPERLESS_APPS = "allauth.socialaccount.providers.openid_connect"` to
  activate the provider.
- `PAPERLESS_SOCIAL_ACCOUNT_DEFAULT_GROUPS` — applies on social signup, which
  *is* the path taken here, unlike `ACCOUNT_DEFAULT_GROUPS`.
- `PAPERLESS_SOCIAL_ACCOUNT_SYNC_GROUPS = true` — adds and removes the person
  from paperless groups **named the same as their groups in kanidm**, on every
  login. This is the thing `aspects/services/tandoor-recipes.nix` cannot do —
  Tandoor is the other allauth app here and nothing maps a claim onto its
  per-space groups, which is why the household's comment says that aspect's
  `admin` level is "the kanidm group existing, not a Tandoor role".
- `PAPERLESS_DISABLE_REGULAR_LOGIN` and `PAPERLESS_REDIRECT_LOGIN_TO_SSO` to
  make it the only door.

The trade, stated plainly: **one secret that cannot be minted today**, plus a
paperless group row that still has to be created once by hand ("Groups must
already exist in Paperless-ngx"), in exchange for per-person authorisation
becoming automatic and group changes in kanidm propagating on next login. For
one person it is not worth it. For the third person it clearly is, and the
right time to switch is the next time secrets can be generated. `splitpro.nix`
and `tandoor-recipes.nix` are both templates for the secret and the OIDC client
declaration; `kanidmClients.<name>` plus `secrets.oidc-client-secret` with
`owner = "kanidm"` is the whole shape.

## Getting a receipt in, which is less obvious than it looks

Four intake paths, and the interesting thing is which one is *not* available.

| path | works? | notes |
|---|---|---|
| **The web UI**, on a phone, over the gate | **yes, today, no extra config** | `maxBodyBytes = 50MB` on the gated take, comfortably under `cloudflare.nix`'s `tunnelBodyCap` of 100MB so the name stays on the tunnel. `websockets = true` is what makes the upload progress indicator work rather than appearing to hang. |
| **`POST /api/documents/post_document/`** | yes, with a token | multipart, field `document`, plus optional `title`/`created`/`tags`/`custom_fields`. Returns a task UUID immediately; poll `/api/tasks/?task_id=<uuid>` for the document id. The obvious target for a phone shortcut. |
| **The consumption directory**, `/var/lib/paperless/consume` | yes, locally | watched by inotify (`PAPERLESS_CONSUMER_POLLING_INTERVAL = 0`). |
| **The existing `paperwork` Nextcloud groupfolder** | **no** | `aspects/shares/paperwork.nix` is a groupfolder, and `nextcloud.nix`'s own note says a folder inside primary storage "is not declarable at all — nothing but a `files:scan` would ever notice it". Pointing paperless's consume directory at groupfolder bytes under `appdata_<instanceid>/__groupfolders/<id>` means writes paperless makes are invisible to Nextcloud's file index, and the path contains an instance id nothing in Nix knows. |

**If the consume directory is ever to be reached from another machine**, the
mechanism already in this tree is a share (`aspects/services/shares.nix`):
a share take carries a `path` for a filesystem backend, so a share aspect
pointing at `consumptionDir` would publish it over samba. Two things would then
have to change together, and both are the kind of fact that is silent when
wrong:

- `PAPERLESS_CONSUMER_POLLING_INTERVAL` must stop being 0. **inotify does not
  fire for a write made by another machine's kernel.** The symptom is a file
  sitting in the directory forever with no log line anywhere. The number is
  stated in the aspect, with that sentence beside it, precisely so the reader
  who adds the share finds it.
- the uid of files samba drops there is not `paperless`. The module has
  `consumptionDirIsPublic` (mode 777) for exactly this; it is off, which is
  right until there is a share.

None of that is written. The UI and the API cover one person with a phone.

## The missing middle: receipt text → grocy

grocy's side of this edge is **not** the problem. The API is substantial and
the write path is well-shaped (`grocy.md`'s own API section has the detail):

- `GET /api/stock/products/by-barcode/{barcode}` — does this product exist
- `GET /api/stock/barcodes/external-lookup/{barcode}` — OpenFoodFacts, optionally
  creating the product
- `POST /api/objects/products`, `POST /api/objects/product_barcodes` — create it
- `POST /api/stock/products/{productId}/add` — `{ amount, price, purchased_date,
  shopping_location_id, transaction_type: "purchase" }`, which is also what
  populates grocy's **price history**, arguably the most valuable thing a
  receipt could contribute
- `POST /api/stock/shoppinglist/add-product` — the shopping-list half
- auth is `GROCY-API-KEY`, as a header or a query parameter, and **one key is
  the whole API** — grocy keys are not scoped

And the plumbing for a machine caller already exists in this tree and needs no
new mechanism. `gateway.nix` mints a per-caller key, `nginx` rewrites it to the
service's internal key on the way through, and a request is attributable to its
caller:

```nix
# on grocy, if a bridge ever exists:
takes.gated.clients = [ "receipt-bridge" ];
takes.nginx.api = {
  enable = true;
  header = "GROCY-API-KEY";
  internalKey = true;     # grocy holds its own key; the gate swaps for it
};
```

with `age.scoped.receipt-bridge.secrets.api-key` generated automatically. The
caller must use `gateway.services.grocy.url` and **not** the loopback, because
the swap only happens on the way through the gate — the option's own
description says so.

**What is missing is the parser,** and it is missing in a specific and
unflattering way.

### `erinalbers/grocy-receipt-ocr` was evaluated and declined

`grocy.md` has the full table. The summary, because it decides this document's
recommendation too: Python/Flask, local tesseract, no login at all
(`app.secret_key` falls back to the literal `'dev-key'`), human review of every
line before any write, last commit 2025-06-09, not in nixpkgs, **no published
image** — so `splitpro.nix`'s nvfetcher-pin-by-digest pattern does not apply,
there is nothing to pin — and 2021-era pins on a `python:3.9-slim` base against
a nixpkgs with no `python39` attribute.

The disqualifier is not the packaging. It is that all 14 shipped store profiles
are **US chains** and every regex, including the fallback, is
`^(?P<barcode>\d+)\s*(?P<title>.*)…` — it **requires the receipt to print a UPC
per line**. US chains do. **Coles, Woolworths, Aldi and IGA do not.** Without a
per-line barcode it degrades to a price-and-title list that a human retypes,
which is the thing it was supposed to avoid.

### So what would actually have to do it

Four candidates, in descending order of how well they fit this tree.

**1. A per-store regex parser in a post-consume script.** The honest minimum.
A `writeShellApplication` (or a small Python/Rust program under `packages/`),
named in `settings.PAPERLESS_POST_CONSUME_SCRIPT`, that fetches
`/api/documents/$DOCUMENT_ID/`, matches `content` against a per-correspondent
pattern set, and writes a **proposal** — not a stock transaction. It fits
everything this tree already does: a store path, no API provisioning, no
ownership record, firewall untouched, credential through the existing
`environmentFile`.

What makes it work or not is the pattern set, and that is a maintenance
commitment rather than a design problem: Woolworths and Coles both reformat
their receipts, and a regex that drifts silently produces *wrong prices in
grocy's price history*, which is worse than producing nothing. Which is why the
output is a proposal.

**2. An LLM pass over the OCR text.** This is the candidate that actually
works on an arbitrary Australian receipt, and it is also the one that fails
quietly. `llama-cpp.nix` is already in this tree, so it can be local — which
removes the "every receipt to a third party" objection but not the "a confident
hallucinated quantity is indistinguishable from a correct one" objection. Note
that paperless's own AI features do **not** help: they classify documents, they
do not extract rows, so this is a separate call made by the script above and not
a flag to turn on.

**3. Donothing: barcodes, not receipts.** `grocy.md`'s standing recommendation,
and it is still the right default. A tablet on `https://grocy.ivymect.in` with
ZXing in-browser scanning, or a USB laser scanner (which is a keyboard and needs
nothing from this tree), puts *correct* products into stock with no parser at
all. The receipt then exists in paperless as the evidence, searchable by text,
and nothing joins the two automatically. **This is what happens if nothing else
is written, and it is not a failure state.**

**4. A commercial receipt-parsing API.** Azure Document Intelligence has a
prebuilt receipt model that does return line items; `PAPERLESS_REMOTE_OCR_ENGINE
= "azureai"` is *not* a route to it, because paperless uses the read model and
takes back text. It would be a direct call from the script, with a vendor key as
an agenix secret, and every household receipt leaving the house. Noted for
completeness; not recommended.

### The shape the proposal should take, if 1 or 2 is ever built

Whatever parses, it should write a **custom field on the paperless document**
and stop. Something like a `select` field `receipt-parse` with values
`unparsed` / `proposed` / `applied` / `rejected`, plus a monetary `total` and a
`lines` string holding the parsed JSON. Three reasons:

- It is idempotent for free. `?custom_field_query=["receipt-parse","exact","unparsed"]`
  is the work queue, and the field is the ownership record that
  `api-provisioning.md` spends a whole section saying there is usually nowhere
  to put. Here there is: paperless has typed custom fields on exactly the object
  the work is about.
- It fails visibly. A receipt stuck at `proposed` is a row in a filtered view,
  not a missing side effect.
- It keeps grocy clean. Nothing reaches `POST /stock/products/{id}/add` until a
  human has looked, which is the one property `grocy-receipt-ocr` got right and
  the reason it cannot silently corrupt stock.

The custom fields themselves are API objects (`/api/custom_fields/`), so
declaring them is case 4 and needs the unwritten `api-provision` mechanism — or
three clicks, once, forever. Three clicks.

## Line items → SplitPro: this one cannot be automated

Asked plainly: **does SplitPro have a usable API?** No.

What is in `src/pages/api/` at the pinned `v2.1.5`: `auth/` (NextAuth),
`trpc/`, `files/`, `healthz.ts`, `readyz.ts`, `locale.ts`, `upload.ts`. That is
the complete HTTP surface. Everything that creates or edits an expense goes
through **one** route, `/api/trpc`, dispatching to
`expenseRouter.addOrEditExpense`; authentication is a NextAuth **session**, and
there is **no API token, no personal access token and no service-account
concept** anywhere in the app.

So the capability is there and unreachable: `createExpenseSchema` does take
participants with explicit per-participant amounts, which is exactly "split
these line items unevenly". There is simply no supported way for a program to
say it.

Three unsupported ways exist and all three are bad, in the specific manner this
repo has already written down:

- **Drive tRPC with a forged or borrowed session.** The procedure name, the
  input schema and the batching envelope are all internal; tRPC's contract is
  "the client is generated from the server", so this is a client pinned to
  upstream's internals. `api-provisioning.md`'s dependency verdict on
  `declarative-jellyfin` is the same object: "Author-deprecated, pinned to eight
  releases of Jellyfin's internals. The cautionary tale."
- **Write Prisma's tables directly.** Worse, and named in the same survey —
  "jellarr *stops the service and writes SQLite*". Here it would be postgres,
  with SplitPro's own migrations as the moving target and its balance
  aggregates to keep consistent by hand.
- **Drive the browser.** A headless browser automating a UI is not a mechanism
  this tree has, and it would be the most fragile of the three.

### Therefore: the human step is here, and it is unavoidable

The pipeline ends at a **proposal a person acts on**, and the realistic best
version of it is modest:

1. The receipt is in paperless, OCR'd, searchable, with its total in a custom
   field.
2. If a parser exists, the line items are in that custom field as JSON, and the
   grocy half can be applied by a script.
3. **A person opens SplitPro and enters the shared portion**, with the receipt
   as the reference. SplitPro already stores receipt images
   (`/var/lib/splitpro/uploads`, which `splitpro.nix` backs up precisely because
   they exist nowhere else), so the artefact ends up in both places.

What can be automated around that step, without touching SplitPro's internals:
a link. Paperless's share links (`/api/share_links/`) produce a URL for a
document that needs no login, so a parsed receipt can carry a one-click
reference to paste into SplitPro's note. That is a convenience, not a pipeline.

The thing to *stop* believing is that "mark these line items for splitting" can
be made declarative. It cannot, at this version, by any route that is not a
liability.

### If that is unacceptable, the fix is upstream or a different app

- **Ask upstream for an API token.** `oss-apps/split-pro` is MIT and actively
  developed; a personal-access-token header validated in tRPC's context is a
  small change and the kind of thing `api-provisioning.md` lists under
  "`authentik-nix` / blueprints — the model to ask upstreams for".
- **Or use something with a real API.** Firefly III (see Part B) has a full
  REST API with personal access tokens and can model who-owes-whom with
  accounts and transfers, badly but programmatically. The household already
  notes that SplitPro "has no role model at all"; replacing it is a bigger
  decision than this document should make, but it is the only route to an
  automated split.

---

# Part B — Up Bank

## The API, measured

Flat, small, well-documented and JSON:API-shaped.

- **Base:** `https://api.up.com.au/api/v1`. Version 1, in beta, with a stated
  promise that endpoints are extended and never removed.
- **Auth:** a **personal access token**, `Authorization: Bearer <token>`,
  generated in the Up app (swipe right → Data sharing → Personal Access Token)
  or at `api.up.com.au`. One token, account-wide, read/write. Upstream's own
  words: "the key to your financial kingdom."
- **Endpoints that matter:** `GET /accounts`, `GET /accounts/{id}`,
  `GET /transactions`, `GET /accounts/{accountId}/transactions`,
  `GET /transactions/{id}`, `GET /categories`, `GET /tags`,
  `PATCH /transactions/{id}/relationships/category`,
  `POST|DELETE /transactions/{id}/relationships/tags`.
- **Filters on `GET /transactions`:** `filter[status]` (`HELD`/`SETTLED`),
  `filter[since]` and `filter[until]` (RFC-3339), `filter[category]`,
  `filter[tag]`. Newest first.
- **Pagination:** opaque cursors, not page numbers. `page[size]=N`, capped at
  100, and you follow `links.next` until it is null. Nothing can be computed
  from a page index, which matters for how a poller stores its position: the
  resumable thing is a **timestamp** for `filter[since]`, not a cursor.
- **Transaction attributes:** `status`, `description`, `rawText`, `message`,
  `amount`, `foreignAmount`, `roundUp`, `cashback`, `createdAt`, `settledAt`.
- **Rate limits:** a 429 with `X-RateLimit-Remaining`, and **no published
  numbers**. Upstream says "ideally with exponential backoff" and nothing more.
  So a poller has to treat the limit as unknown and back off on 429 rather than
  pace itself against a documented budget.
- **Errors:** an `errors` array with `status`, `title`, `detail` and an optional
  `source` pointing at the offending parameter or JSON pointer.

### Webhooks, and why they are not worth it here

They work, and they buy less than they appear to.

- `POST /webhooks` (maximum 10 active) returns a `secretKey` **once**.
- Every delivery carries `X-Up-Authenticity-Signature`, which is
  `HMAC-SHA256(raw request body, secretKey)` — over the **raw** body, so any
  middleware that re-serialises JSON breaks verification.
- `eventType` is one of exactly four values: `TRANSACTION_CREATED`,
  `TRANSACTION_SETTLED`, `TRANSACTION_DELETED`, `PING`.
- Delivery is retried with exponential backoff if the endpoint is unreachable or
  does not answer 200. `GET /webhooks/{id}/logs` shows what happened.
- **The decisive detail:** the event body contains a `transaction`
  *relationship* — a `{ type, id }` reference and a `links.related` URL. **The
  transaction is not embedded.** The handler must call back to the API with the
  PAT anyway.

So a webhook saves **latency and nothing else**. It costs, in this tree,
specifically:

- a **public, ungated vhost**. `cloudflare.nix` derives the tunnel's ingress
  from this host's nginx vhosts, so a receiver would need `takes.proxy` rather
  than `takes.gated` — publication without the gate — because Up cannot follow
  an oauth2-proxy redirect. That is a new unauthenticated route into the box
  whose only protection is HMAC code we wrote.
- a **second secret** (the webhook `secretKey`) which, like an audiobookshelf
  API key, **cannot be an agenix secret in the usual sense**: Up returns it once,
  at creation, so it has to be captured and placed, the same problem
  `grocy.md` records for a kanidm service-account token.
- HMAC verification over a raw body, which is the single most commonly
  mis-implemented thing in this whole document.

**Recommendation: poll.** `GET /transactions?filter[since]=<last seen settledAt>`
on a timer, following `links.next`. No inbound endpoint, no second secret, no
signature code, one credential, and the same data. If real-time ever matters —
it does not, for transaction tracking — revisit then.

## Is a bridge needed?

Two different questions hide in that one, and they have different answers.

### "Does something read Up directly?" — yes, several things, none in nixpkgs

From Up's own `community/EXAMPLES.md` and a search of what is maintained:

| project | lang | shape | state |
|---|---|---|---|
| `richwalm/upbank2firefly` | Python | **webhook handler** → Firefly III | listed by Up upstream; small |
| `MajorArkwolf/UpBankFFImporter` | Rust | once-off **or continuous**; takes an Up PAT and a Firefly PAT; detects updated transactions (a new tag or category in Up) and updates Firefly to match | 17 stars, 74 commits, README says "still a work in progress" |
| `mhasanbulli/up-actual` | Python | CLI; fetches from Up, reconciles into **Actual Budget**, applies category mappings and runs Actual's rules | small |
| `johnmee` beancount importer | Python | CLI → **beancount** | listed by Up upstream |
| `BrodieSutherland/up-to-ynab` | Python | → YNAB (hosted) | listed by Up upstream |
| `CorrosiveKid` sheets sync | Python | → a Google Sheet | listed by Up upstream |
| `jupys` Up Banking HA | YAML | Home Assistant sensors | listed by Up upstream |

**None of these is in nixpkgs** and none publishes a container image of the kind
`splitpro.nix` pins by digest. Each would be a derivation here.

### "Does the *official* importer handle Up?" — no, and it structurally cannot

`firefly-iii` **6.7.7** and `firefly-iii-data-importer` **2.3.5** both exist in
this locked nixpkgs, with NixOS modules
(`nixos/modules/services/web-apps/firefly-iii.nix` and
`…-data-importer.nix`). `actual` **26.10.0** has a module too. So the
*destination* is cheap here — the module work is done.

But the data importer "does not connect to your bank directly". It imports
CSV/CAMT.052/CAMT.053 files, or talks to **third-party aggregators**:
Nordigen/GoCardless (EU PSD2), Spectre and Salt Edge. There is **no Up
connector and no generic "arbitrary bank API" mode.** The newer route for
Australian banks is a **SimpleFIN** provider, which any SimpleFIN-capable
consumer (Actual, Firefly's importer) can read — but that is a third party in
the middle of a connection that Up already offers directly and for free, which
is a strange trade for one Australian account.

### The verdict

**No aggregator bridge is needed, because Up reads directly.** What is needed is
one small poller, and the only real question is where it writes.

Ranked by cost in this tree:

1. **Up → Firefly III, with `UpBankFFImporter` or an equivalent.** Firefly is
   packaged *and* moduled here, speaks postgres (so `takes.postgres` over the
   socket, like tandoor), has a real REST API with personal access tokens, and
   is the thing the only Rust Up importer already targets. The cost is: one new
   service aspect, one derivation for the importer, two secrets (an Up PAT and a
   Firefly PAT), and a second finance app in a house that already runs SplitPro.
2. **Up → a local store of our own.** A timer, `curl`, `jq`, and rows in the
   postgres this box already runs. Cheapest to write, and it is a new schema
   nobody else maintains — which means it is also the one that will be
   half-finished forever.
3. **Up → Actual Budget**, with `up-actual`. Actual is packaged and moduled;
   its data model is a budget rather than a ledger, and `up-actual` is Python
   and small.
4. **Up → Home Assistant sensors.** Not transaction tracking. A balance on a
   dashboard. Cheap and genuinely useful, and it answers a different question.

**Recommendation: (1) if transaction tracking is actually wanted, (4) if what is
wanted is "see the balance", and nothing at all until that is decided.** Writing
(2) is how a household ends up maintaining a ledger.

### What the aspect would look like, and the secrets it would need

Sketched, not written. The shape is unremarkable because the hard parts are not
in Nix:

```nix
# aspects/services/up-bank.nix — SKETCH, NOT WRITTEN
den.aspects.up-bank = {
  includes = [ den.aspects.agenix-rekey ];       # and firefly, if (1)

  # NO `gives.tcp`, NO `takes.gated`, NO VHOST. It is an outbound poller: a
  # timer and a credential. ../base/firewall.nix opens nothing because
  # nothing is given, which is the correct answer and not an omission.
  # A webhook receiver would need `takes.proxy` (published, NOT gated) and
  # is argued against above.

  secrets = {
    # ── CANNOT BE GENERATED. ──
    # Every other secret in this tree has a `generator.script` and is minted
    # from nothing. This one is MINTED BY UP, in Up's app, and pasted in --
    # the same category as the audiobookshelf OIDC value that has to be typed
    # into a UI, read from the other end. So no generator, and `rekey` has
    # nothing to do for it: it is `agenix -e` once, by hand.
    pat = { };
  };

  templates.env = { secrets, ... }: {
    dependencies = { inherit (secrets) pat; };
    content = { placeholders, ... }: ''
      UP_TOKEN=${placeholders.pat}
    '';
  };

  nixos = { ... }: {
    systemd.services.up-bank-sync = {
      startAt = "hourly";
      serviceConfig = {
        Type = "oneshot";
        # The position to resume from: a timestamp, not a cursor -- Up's
        # pagination is opaque cursors with no stable index, so the only
        # resumable thing is the newest `settledAt` already seen.
        StateDirectory = "up-bank";
        EnvironmentFile = /* scoped.up-bank.templates.env.path */ null;
      };
    };
  };
};
```

The two things that make it more than a curl loop, and which a VM test should
assert before it is believed: **429 with no published budget** (back off, do not
pace), and **`HELD` → `SETTLED`** (the same transaction appears twice with a
changed `status` and a newly non-null `settledAt`, so a sync keyed on "have I
seen this id" books it once and never updates it, and a sync keyed on
`filter[since]=settledAt` misses held transactions entirely until they settle).

## The join that does not exist: a transaction is not a receipt

Worth saying out loud, because the two halves of this document look like they
meet and they do not.

Up gives a **merchant, an amount, a date** and a free-text `description` /
`rawText`. A receipt gives **what was bought**. Matching one to the other is
easy and shallow: same total, same day, probably the same merchant — a fuzzy
join that will be right most of the time and will be wrong on the day two
Woolworths shops cost the same.

And once joined it adds **nothing the receipt did not already have.** Up cannot
tell you that $7.40 of a $92.10 Coles shop was somebody else's shampoo. **No
bank API can**, in Australia or anywhere; itemised purchase data is not in the
card network's message. So:

- Up Bank answers **"what did we spend"** and is the right source for that.
- Paperless answers **"on what"**, to whatever precision OCR plus a parser
  reaches.
- SplitPro answers **"who owes whom"**, and a person tells it.

Treating the Up integration as part of the receipt pipeline is the mistake to
avoid. It is a second, independent thing that happens to involve money.

---

# Which facts live where

| fact | owner | why there |
|---|---|---|
| paperless listens on 28981, exposed on nothing | `aspects/services/paperless.nix`, `gives.tcp` | one declaration drives the firewall hole (none), the resolved upstream, and what the listener binds |
| it is published at `paperless.<zone>`, gated, with websockets and a 50MB body | same, `takes.gated` | the proxy composes the name from the host's zone carrier; the aspect states only what differs |
| the database and role are both `paperless` | same, `gives.user` + `takes.postgres` | `postgres.nix` derives them from the furnished account, because peer auth checks the connecting unix user against the role |
| `Remote-User` is overwritten, and dropped when empty | `aspects/services/gateway.nix` | one implementation of the gate; a service says only that it wants an identity |
| `/api` does **not** get that treatment | `gateway.nix`'s API location | and so `PAPERLESS_ENABLE_HTTP_REMOTE_USER_API` is false, with the reason written at the setting |
| a person may reach paperless | `aspects/households/ivymectin.nix`, `members.ivy.services.paperless` | `access.nix` turns the grant into `app-paperless-user` and the gate admits it |
| what a grant *means* (one level) | the aspect's `gives.service.access` | the permission model is the service's own fact, not the household's |
| which host serves it | `aspects/hosts/secondpc/default.nix` | moving the service between machines touches one file |
| the tunnel's 100MB body cap | `aspects/network/cloudflare.nix` | one number, read by the public carrier and by `excluded` |
| a receipt's parsed line items | **nowhere.** Proposed: a custom field on the paperless document | paperless has typed fields on exactly the object the work is about, which is the ownership record `api-provisioning.md` usually cannot find a home for |
| who owes whom | SplitPro's postgres, entered by a person | there is no API, and the three ways around that are all liabilities |
| an Up access token | **nowhere.** Proposed: `den.aspects.up-bank.secrets.pat`, no generator | Up mints it, so it is pasted in rather than generated |

---

# The weakest link

Two places, named in the brief and confirmed by the research.

**1. Receipt line-item extraction does not exist and nothing off the shelf
supplies it.**

- paperless-ngx has no table or line-item parser, in any of its OCR paths,
  including the remote one.
- grocy has no receipt concept at all — `strings.pot` has no `receipt`, `ocr` or
  `invoice`, and the only "receipt" in its config is a *thermal printer*.
- The one open-source bridge, `erinalbers/grocy-receipt-ocr`, is unmaintained
  since June 2025, unpackageable without relaxing 2021 pins against untested
  code, has no authentication of its own, and — the real killer — requires a
  **UPC printed on every receipt line**, which Australian supermarkets do not
  print.

So anything that works here is **something written in this house**: a
per-store regex set that drifts when a supermarket changes its layout, or a
local LLM pass that is confidently wrong at an unknown rate. Both are
maintenance, not integration. The only approach that is *correct* rather than
*plausible* is the one that bypasses receipts entirely — barcode scanning into
grocy, with the receipt kept in paperless as searchable evidence and no
automatic link between them.

**2. SplitPro cannot be automated at all.** tRPC-only, NextAuth-session-only, no
token of any kind. The split is a human action, and the three unsupported routes
to changing that (forged session, direct database writes, browser automation)
are each a worse version of mistakes this repo has already documented. The
pipeline therefore **ends in a person**, and the best thing automation can
contribute is a correct proposal and a link to the receipt.

Up Bank, by contrast, is the easy one: a clean API, a single token, a poller,
and an existing Rust importer if Firefly III is the destination. It is also the
piece that contributes least to the receipts problem, because **no bank API
returns line items.**

---

# Secrets declared and unminted

**None.** That is the headline and it is deliberate.

`aspects/services/paperless.nix` declares **no `secrets` block and no
`templates` block**, for the same three reasons `grocy.nix` has none: the gate
authenticates the person (no OIDC client secret), peer authentication over the
unix socket authenticates the database connection (no password), and no API
token is held by the aspect. `PAPERLESS_SECRET_KEY` is generated at first start
by the nixpkgs module into `${dataDir}/nixos-paperless-secret-key.env`, which
makes it **state to back up** — it is inside the `backup` quirk's `paths` —
rather than a secret to declare.

So nothing in this change needs `nix run .#rekey` or `nix run .#gen-secrets`,
and it is deployable while the master key is mid-recovery.

Secrets that the *designs* in this document would need, none of which is
declared anywhere:

| secret | for | mintable? |
|---|---|---|
| paperless OIDC client secret | the allauth route that would remove the per-person permission step | generated (`random-hex-32`), when `rekey` is usable again |
| a paperless REST token | any bridge reading OCR text out | **no.** A row in paperless's database. The UI (My Profile → the circular arrow) or `paperless-manage drf_create_token <user>` — `rest_framework.authtoken` is in `INSTALLED_APPS`, so the CLI path exists and is `api-provisioning.md`'s case 3, not case 4. |
| a grocy API key for the bridge | writing stock | **generated by the gate**, as `<caller>/api-key`, the moment grocy names a `clients` entry. Needs a `rekey`. |
| an Up Bank PAT | reading transactions | **no.** Up mints it in its app. `agenix -e` once, by hand, no generator. |
| an Up webhook `secretKey` | only if webhooks are used, which is argued against | **no, and worse** — returned exactly once at creation, so it has to be captured and placed, like a kanidm service-account token |
| a Firefly III PAT | only under Part B option (1) | **no.** Minted in Firefly's UI. |

---

# What was written, and what was only designed

**Written, formatted, `jj file track`ed, and evaluated:**

- `aspects/services/paperless.nix` — the aspect. `gives` (service access, the
  tcp endpoint with `expose = [ ]`, the unix account), `takes.postgres` naming
  all four units, `takes.gated` with `oauthProxy`/`forwardUser`/`websockets`/
  `maxBodyBytes`, a `backup` entry covering `dataDir`, and the nixpkgs module's
  settings with the reason written at each one that is not obvious. No secrets.
- `aspects/households/ivymectin.nix` — `services.paperless = { }` and
  `members.ivy.services.paperless.access = "user"`. The grocy grant's comment
  was also corrected: it still described the LDAP application-password login
  that `grocy.nix` removed, and said the grant was *not* what let her in, which
  is now exactly backwards.
- `aspects/hosts/secondpc/default.nix` — `paperless.enable = true` and
  `den.aspects.paperless` in the includes. The grocy claim's comment was
  corrected for the same reason.

**Designed here and deliberately not written:**

- the receipt parser, in any of its four candidate forms, and the custom-field
  state machine it would key its idempotency on;
- the paperless custom fields themselves, which are API objects and would need
  the `api-provision` mechanism that `api-provisioning.md` sketches and nobody
  has written;
- `takes.nginx.api` on paperless, and the `clients` entry on grocy that would
  let a bridge call it through the gate;
- the samba share over the consumption directory, and the two settings
  (`PAPERLESS_CONSUMER_POLLING_INTERVAL`, `consumptionDirIsPublic`) that have to
  change with it;
- `aspects/services/up-bank.nix`, in any form, poller or webhook receiver;
- anything at all touching SplitPro, because there is nothing to touch.

**Follow-up worth doing independently of receipts:** the
`forwardUser` + `api.enable` interaction in `gateway.nix` is a real foot-gun
with no guard. Nothing is currently misconfigured, but the two options are
independently settable and the failure mode is silent authentication as any
user. Either assert against the combination or blank `Remote-User` in the API
location unconditionally.
