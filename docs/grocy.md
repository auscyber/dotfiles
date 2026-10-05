# Grocy

Household ERP: stock, shopping lists, recipes and meal plans, chores, tasks,
batteries, equipment. It is going in beside vikunja and donetick rather than
replacing either; the overlap is resolved with grocy's own feature flags, not
by retiring anything.

**Nothing in this document has been evaluated** — same disclaimer as
`capabilities.md`. A full-host eval needs several GB on this box and the
questions here did not need one. What *was* checked is stated where it matters:

- grocy's behaviour was read from upstream **v4.5.0** source, quoted inline.
  **The locked input now builds 4.7.1 on php85**, which moved the auth
  middleware's namespace and gave the module an `extraConfig`; the corrections
  are under "What the nixpkgs module can actually set" below, and anything in
  this document quoting 4.5.0 line numbers should be re-read against 4.7.1
  before it is relied on.
- the NixOS module is read from this flake's own locked nixpkgs, rev
  **`f20cd90e`**. Read it with `nix eval --raw .#inputs.nixpkgs.outPath` and
  NOT by looking up the `flake.lock` node named `nixpkgs`: there are four
  nixpkgs nodes in that lock and the one with the bare name is not the root
  input. An earlier version of this line cited `1cabc318` from that node, which
  is a different tree and is why the audit below went stale without anything
  changing in this repo.

## What it is for here, feature by feature

| grocy feature | use here | replaces / duplicates |
|---|---|---|
| **Stock** (products, locations, due dates, opened/frozen tracking, price history, min-stock) | yes — the reason to run it | nothing. No service here tracks what is in the house. |
| **Shopping list** (multiple lists, auto-add below min stock, print) | yes | nothing |
| **Recipes + meal plan** (ingredient fulfilment against stock, add-missing-to-shopping-list, consume-recipe) | yes | nothing |
| **Quantity units + conversions**, **product groups**, **locations**, **shopping locations** (stores) | yes, as master data under stock | nothing |
| **Userfields** (`/userfields/{entity}/{objectId}`) and user entities (`/objects/{entity}`) | available; no planned use | nothing |
| **Batteries** (charge cycles, due-soon) | yes — nothing else tracks this | nothing |
| **Equipment** (manuals attached to a thing) | marginal; keep on, costs nothing | nothing |
| **Chores** (recurring, assignment rotation) | **turn off** | duplicates `aspects/services/donetick.nix` badly — see below |
| **Tasks** (flat, due date, category, assignee) | **turn off** | a strictly weaker vikunja (`aspects/services/vikunja.nix`) |
| **Calendar** (iCal feed of due products/chores/tasks/batteries/meals) | keep on; it degrades to whatever remains enabled | nothing |
| **Barcode scanning** (USB laser scanner, or ZXing in-browser camera) | yes, and it is the thing that makes stock tracking survivable | nothing |
| **Barcode lookup plugins** (OpenFoodFacts by default) | yes, with one caveat: it is an **outbound call from the php-fpm pool** to `world.openfoodfacts.org` at lookup time | nothing |
| **Grocycode / label printing** (webhook or ESC/POS thermal) | no printer here; both flags default off | nothing |

### Why chores and tasks come off rather than donetick and vikunja

Both are off by feature flag, and the reason is identity, not taste.

- donetick is a **real OIDC client** (`aspects/services/donetick.nix`), so a
  chore is assigned to a kanidm person and the app can map groups to roles
  (`admin_groups`/`manager_groups`).
- grocy's users are rows in its own SQLite database, created on first sight of a
  proxy header, and every one of them is an **ADMIN** (see *Authentication*
  below). Chore assignment in grocy would be assignment to a user record nothing
  else in this house knows about.
- vikunja has projects, labels, kanban, attachments and per-user sharing — its
  `backup` entry exists purely because attachments are not in postgres. grocy's
  tasks are a name, a due date, a category and an assignee.

So grocy subsumes **nothing** currently running. It covers a gap nothing
covers. The flags are at *Configuration audit*, below.

## Receipts

### grocy itself has no OCR, and no receipt concept at all

Checked three ways against v4.5.0:

- `localization/strings.pot` (2472 lines) contains **no** string matching
  `receipt`, `ocr` or `invoice`.
- `README.md` mentions barcodes at length (`### Barcode readers & camera
  scanning`, `### Barcode lookup via external services`) and receipts never.
- the only "receipt" in `config-dist.php` is the *thermal printer* — printing a
  shopping list on a receipt printer (`TPRINTER_*`). That is output, not input.

The upstream path into stock is a **barcode**, not a receipt:
`/stock/products/by-barcode/{barcode}/add` and friends in `grocy.openapi.json`.

### `erinalbers/grocy-receipt-ocr`, evaluated

Evidence from the repo, not its README.

| question | answer |
|---|---|
| what it is | Python/Flask web app. 51 files, 65 KB. Upload a receipt image or PDF → OCR → a **review page** → a purchases page → one `POST /add-purchase` per confirmed line. |
| how it runs | `docker-compose.yml` with **four** services: `web` (Flask/gunicorn), `worker` (rq), `redis`, and a redundant `tesseract` container that literally runs `apt-get install … && tail -f /dev/null`. There is **no published image** — `build: context: .` only. |
| OCR engine | **local**: `pytesseract` over `tesseract-ocr` installed in the Dockerfile, `pdf2image`/`poppler-utils`, `opencv-python-headless`. **Nothing is sent off the box.** The only outbound call is grocy's own `/stock/barcodes/external-lookup/{barcode}`. |
| maintained | Created 2025-05-15, **last commit 2025-06-09**. 19 stars, 0 open issues. MIT. One author. Four weeks of commits and then nothing. |
| grocy API version pinning | **none.** It hits paths directly with `GROCY-API-KEY` and has no version check. |
| which endpoints | `GET /stock/products/by-barcode/{barcode}`, `GET /objects/{products,product_groups,locations,quantity_units,quantity_unit_conversions,product_barcodes}`, `GET /stock/barcodes/external-lookup/{barcode}`, `POST /objects/products`, `POST /objects/product_barcodes`, `POST /stock/products/{id}/add`. grocy API keys are **not scoped** — one key is the whole API. |
| silent corruption risk | **no.** The write path is review → purchases → an explicit `POST /add-purchase` per item. A human confirms every line. A misread changes a price or fails to match; it does not silently book stock. |
| its own auth | **none.** No login, `app.secret_key` falls back to the literal `'dev-key'`, and the grocy API key sits in its environment. Anything that can reach its port can write to grocy. |
| **the disqualifier** | `config/receipt_processors_default.json` ships 14 store profiles and all 14 are **US chains**. Every regex, including the fallback, is `^(?P<barcode>\d+)\s*(?P<title>.*)…` — it **requires the receipt to print the UPC per line**. US chains do; Coles, Woolworths, Aldi and IGA do not. Without a barcode on the line it degrades to a price-and-title list a human retypes. |

**Packaging cost, concretely.** Not in nixpkgs. No published image, so
`splitpro.nix`'s pattern — nvfetcher pulls the image into the store and
`imageFile` pins it by digest — **does not apply**; there is nothing to pin.
The two real options are a derivation (its pins are 2021-era: `flask==2.0.1`,
`werkzeug==2.0.1`, `pillow==8.3.1`, `opencv-python-headless==4.5.3.56`, on
`python:3.9-slim`, and **this nixpkgs has no `python39` attribute at all**, so
every pin would have to be relaxed against untested code), or building the
image here with `dockerTools`, which needs the same derivation first.

And if it did run as a container: containers here get their own network
namespace and reach host services over `den.aspects.container-bridge`
(`aspects/network/container-bridge.nix`), not `--network=host`.

**Recommendation: do not run it.** Use grocy's barcode scanning for products,
and leave receipts where they already are — `splitpro.nix` backs up its uploads
directory, which is a receipt store with a purpose (settling up) that this
would not improve. Revisit only if someone writes Coles and Woolworths
processors, and at that point the upstream is an unmaintained repo and the
regexes are the only part worth keeping.

## Authentication

### grocy has no OIDC. It has three auth classes and that is the whole list

`middleware/` in v4.5.0 is: `ApiKeyAuthMiddleware`, `AuthMiddleware` (abstract),
`BaseMiddleware`, `CorsMiddleware`, `DefaultAuthMiddleware`, `JsonMiddleware`,
`LdapAuthMiddleware`, `LocaleMiddleware`, `ReverseProxyAuthMiddleware`,
`SessionAuthMiddleware`. `config-dist.php` says it in as many words:

```php
// Either "Grocy\Middleware\DefaultAuthMiddleware", "Grocy\Middleware\ReverseProxyAuthMiddleware"
// or any class that implements Grocy\Middleware\AuthMiddleware
Setting('AUTH_CLASS', 'Grocy\Middleware\DefaultAuthMiddleware');

// Options when using ReverseProxyAuthMiddleware
Setting('REVERSE_PROXY_AUTH_HEADER', 'REMOTE_USER');
Setting('REVERSE_PROXY_AUTH_USE_ENV', false);
```

So the setting is `REVERSE_PROXY_AUTH_HEADER`, not `REVERSE_PROXY_AUTHENTICATION`,
and the default is `REMOTE_USER`.

**This is the first service here that genuinely wants
`takes.gated.oauthProxy = true`.** vikunja and donetick set it false because
they are their own OIDC clients and a forward-auth proxy would hand them one
trusted identity for everybody. grocy has no second login to bounce through, so
the proxy *is* its login, and `oauthProxy`'s default of `true` is correct
unmodified.

### SUPERSEDED: the auth is reverse-proxy, not LDAP

**grocy now authenticates through the gate**, not against kanidm's LDAP
gateway. The LDAP design below is kept because it is the record of what was
ruled out and why it would have worked, but it is no longer what the aspect
does.

What changed the answer: the gate was *already* in front of this vhost, for
publication and the group check. So the person's identity had already been
established by oauth2-proxy before grocy saw the request, and LDAP was a second
authentication path beside one that had already run. `ReverseProxyAuthMiddleware`
reads one name, finds that user or creates them, and defines
`GROCY_EXTERNALLY_MANAGED_AUTHENTICATION`, which removes grocy's own login form
and user management from the UI.

What it deletes, all of which the LDAP version needed: a bind service account
and its once-only token, a derived unit to place a readable copy of that token,
a kanidm APPLICATION plus the group permitted to hold passwords for it, a CA
bundle for the `ldaps://` leg, and **the manual step** -- each person had to
mint their own application password, because kanidm only ever mints those. The
aspect now holds no credential of any kind.

Three details that matter:

- The name comes from `X-Auth-Request-Preferred-Username`, **not**
  `X-Auth-Request-User`. oauth2-proxy's `--set-xauthrequest` emits four headers,
  each from a named claim (`getXAuthRequestHeaders` in `legacy_options.go`):
  `user`, `email`, `preferred_username`, `groups`. The `user` claim is the
  subject or the email depending on provider; `preferred_username` is kanidm's
  short name, which is `kanidm.name` and therefore the string samba, mail and
  NSS already use for that human. Keying grocy on the other one would have
  created a second account for the same person.
- The variable is `GROCY_REMOTE_USER`, **not** the conventional `REMOTE_USER`.
  The module's php location ends with `include .../fastcgi.conf` and
  `include .../fastcgi_params`, which land after our own `fastcgi_param`, and
  nginx sends duplicate parameters rather than letting the first win -- so if
  either include ever set `REMOTE_USER`, php would read theirs. Neither does
  today. But that is a fact about a file in another package, and the failure
  mode of it changing is "any visitor is admitted as somebody else", so a name
  of our own removes the question instead of answering it once.
- `REVERSE_PROXY_AUTH_USE_ENV = true`, which is the security of the whole
  arrangement rather than a detail. php maps an inbound HTTP header into
  `$_SERVER` as `HTTP_<NAME>`, so a client sending `Remote-User:` lands at
  `HTTP_REMOTE_USER` and can never appear as `REMOTE_USER` -- that key is
  writable only by the server, through a `fastcgi_param`. In header mode the
  same request would be indistinguishable from the gate's own.
- It **fails closed**. `proxy_set_header` with an empty variable drops the
  header rather than forwarding a blank one, and the middleware throws on a
  missing or empty value -- so an unauthenticated request cannot arrive as a
  nameless user, and a misconfigured gate is a 500 rather than a silent
  anonymous login.

Authorisation also moved to a better place: the gate admits the union of a
vhost's groups and `access.nix` turns a household grant into one of them, so a
person with no grant never reaches grocy. Under LDAP the gate let them through
and the application's linked group was the real check.

### LDAP, since it was asked (SUPERSEDED -- see above)

It is **upstream core**, not a contribution: `middleware/LdapAuthMiddleware.php`
ships in the tarball nixpkgs builds, and `config-dist.php` declares
`LDAP_ADDRESS`, `LDAP_BASE_DN`, `LDAP_BIND_DN`, `LDAP_BIND_PW`,
`LDAP_USER_FILTER`, `LDAP_UID_ATTR`. **No patch would be needed** — the
`patches/kanidm-provision/` precedent does not apply.

What it does: binds as the service account, searches
`(&(UID_ATTR=username)USER_FILTER)` under `BASE_DN`, reads `givenname`, `sn` and
the DN, then **re-binds as the user's DN with the submitted password**.

Could kanidm's gateway serve it? Mechanically, probably — `sso.nix` now sets
`ldapbindaddress = "127.0.0.1:3636"`, and bind-as-user is exactly what dovecot
does there. But it is the **wrong trade**:

- it puts a **password form** back in front of grocy, so grocy would be the one
  service here where a password still authenticates a person. kanidm is
  WebAuthn-first here and the intent is recorded to make passkeys the minimum
  credential type; an LDAP simple bind cannot carry a passkey.
- it needs a **bind account credential** — a new generated secret — which is
  Phase-B work while the master key is recovering (`CLAUDE.md`).
- kanidm's LDAP attributes are not AD's. `LDAP_UID_ATTR` would have to be
  whatever kanidm exposes for the short name, and `givenname`/`sn` are
  hardcoded in the middleware and may be empty.

Reverse-proxy auth costs none of that and reuses the gate that is already
there. LDAP stays unconfigured.

### What the gate actually sets, exactly

`sso.nix` runs oauth2-proxy with `setXauthrequest = true` and
`reverseProxy = true`. Upstream's flag table:

> `--set-xauthrequest` … set X-Auth-Request-User, X-Auth-Request-Groups,
> X-Auth-Request-Email and X-Auth-Request-Preferred-Username response headers
> (useful in Nginx auth_request mode)

Those are **response headers on the `auth_request` subrequest**; nginx has to
lift them with `auth_request_set` and re-send them. nixpkgs' integration lifts
exactly two:

```nginx
auth_request_set $user   $upstream_http_x_auth_request_user;
auth_request_set $email  $upstream_http_x_auth_request_email;
proxy_set_header X-User  $user;
proxy_set_header X-Email $email;
```

`X-Auth-Request-User` is `session.User`, which for the OIDC provider is the
claim named by `UserIDClaim` — and that **defaults to `email`**, not `sub`. So
`X-User` carries an email address, not `auscyber`.

`X-Auth-Request-Preferred-Username` carries `preferred_username`, which kanidm
fills with the **short name** because every client here is provisioned with
`preferShortUsername = true` (`aspects/services/kanidm.nix`, whose own comment
is "Anything matching existing users by username depends on this").

**Decision: grocy reads `X-Forwarded-Preferred-Username`**, lifted in grocy's own
vhost entry, so grocy usernames match every other app's record of the same
human. The zero-config alternative is `REVERSE_PROXY_AUTH_HEADER = "X-User"`,
which needs no vhost entry — at the price of email-address usernames. Either
works; the first is the one that does not create a second name for a person.

### Users are auto-created, so there is no per-person provisioning step

`middleware/ReverseProxyAuthMiddleware.php`:

```php
$username = $request->getHeader(GROCY_REVERSE_PROXY_AUTH_HEADER);
if (count($username) !== 1)
{
    throw new \Exception('ReverseProxyAuthMiddleware: ' . GROCY_REVERSE_PROXY_AUTH_HEADER . ' header is missing or invalid');
}
$username = $username[0];

$user = $db->users()->where('username', $username)->fetch();
if ($user == null)
{
    $user = UsersService::getInstance()->CreateUser($username, '', '', '');
}
```

So: **first page load creates the account.** Nothing has to be provisioned per
person, and nothing *can* be — grocy's users are rows in its own SQLite file.
It tries `ApiKeyAuthMiddleware` first, so **grocy's own API keys keep working**
under reverse-proxy auth, which is what makes the Home Assistant story below
possible.

**And every auto-created user is an admin.** `CreateUser` inserts the
permissions named by `GROCY_DEFAULT_PERMISSIONS`, which defaults to `['ADMIN']`.
`controllers/Users/User.php` defines 30 permission constants and they are
per-user rows in `user_permissions`, edited in grocy's own UI by an admin.
**There is no group concept and no external mapping.** A kanidm group cannot
mean anything inside grocy.

Which is why `gives.service.access` declares **one** level — the same shape and
the same honesty as splitpro's "you are in or you are out". An `admin` level
would be a lie: `app-grocy-admin` would open the same gate as `app-grocy-user`
and change nothing inside grocy, where everyone is already ADMIN.

### The spoofing failure mode, and why the port stays shut

A trusted header is a password that the network is supposed to keep secret.
Anything that can set `X-Forwarded-Preferred-Username` on a request grocy sees
**is** that user, with ADMIN. Three things have to hold, and all three are
structural rather than remembered:

1. **nginx overwrites the header.** `proxy_set_header` replaces whatever the
   client sent; an empty value *removes* it — the same behaviour the gateway
   already relies on for `Accept-Encoding`. So an unauthenticated request never
   reaches grocy with a usable name, and a session whose token lacks
   `preferred_username` gets the header stripped and grocy throws a 500 rather
   than logging anybody in. The general rule this is an exception to: nginx
   forwards all *other* client headers upstream, so the header grocy trusts must
   be one nginx explicitly sets. It is.
2. **The upstream port is never exposed.** `gives.tcp.ports.http.expose = [ ]` —
   the default, written out anyway, because here it is the whole security story
   rather than tidiness. The firewall is derived from it, so the port is shut on
   every wire: the lan, the tailnet and whatever the router forwards.
3. **nginx is the only path.** Both server blocks are nginx's, and the internal
   one binds `127.0.0.1` only.

What remains, stated plainly: **any local process on secondpc that can open a
loopback socket is grocy admin.** That is a real residual risk and nothing here
mitigates it. It is the price of header auth. Neither available mitigation
helps: `REVERSE_PROXY_AUTH_USE_ENV` only moves the forgeable value from an HTTP
header to a fastcgi param derived from the same header, and grocy's own API-key
check is accepted on that port anyway.

## Configuration audit — `config-dist.php`, setting by setting

How settings resolve: `Setting()` defines `GROCY_<name>` **once**, taking the
first of (1) `GROCY_DATAPATH/settingoverrides/<name>.txt`, (2) the environment
variable `GROCY_<name>`, (3) the literal value passed. Our config file is loaded
before `config-dist.php`, so a value set in either place wins over upstream's
default, and the environment wins over both.

Two consequences worth knowing before reading the tables:

- **Env vars are strings.** The literals `true`/`false` map to booleans and
  everything else is returned as a string. **Array-valued settings cannot be set
  from the environment** — `DEFAULT_PERMISSIONS` and `LABEL_PRINTER_PARAMS`.
- **`DefaultUserSetting()` does not consult the environment at all.** Per-user
  defaults can only be changed by editing the config file.

`ConfigurationValidator` validates `MODE`, `DEFAULT_LOCALE`, `CURRENCY`,
`CALENDAR_FIRST_DAY_OF_WEEK`, `ENTRY_PAGE`, `MEAL_PLAN_FIRST_DAY_OF_WEEK` and the
night-mode range — and **does not validate `AUTH_CLASS`**. A typo there is a PHP
fatal on every request, with nothing catching it at build time.

### Authentication

| setting | value here | why |
|---|---|---|
| `AUTH_CLASS` | `Grocy\Middleware\ReverseProxyAuthMiddleware` | the only class that can consume the gate. Not validated — a typo is a 500 on every request. |
| `REVERSE_PROXY_AUTH_HEADER` | `X-Forwarded-Preferred-Username` | default is `REMOTE_USER`. This is the header the aspect's vhost entry sets from oauth2-proxy's `X-Auth-Request-Preferred-Username`. |
| `REVERSE_PROXY_AUTH_USE_ENV` | `false` (default) | read the HTTP header, not `$_SERVER`. |
| `DISABLE_AUTH` | `false` (default) | must stay false. True means "use the default user" for everybody. |
| `MODE` | `production` (default) | any other value disables authentication *and* generates demo data. |
| `DEFAULT_PERMISSIONS` | left at `['ADMIN']` | **not settable from the environment** (array). Every auto-created user is an admin. |
| `LDAP_*` (6 settings) | unset | upstream core, deliberately unused — see above. |

### Publication

| setting | value here | why |
|---|---|---|
| `BASE_URL` | `/` (default) | **load-bearing.** grocy builds its base from `X-Forwarded-Proto` and `Host`, both of which nginx's `recommendedProxySettings` sends, so it emits `https://grocy.ivymect.in` even though the internal hop is plain http. |
| `BASE_PATH` | `''` (default) | correct **only** while grocy is at the root of its own vhost. If it is ever moved under a shared vhost this must be set, or every generated link 404s. |
| `DISABLE_URL_REWRITING` | `false` (default) | the module's vhost does the rewriting. True would put `/index.php` in every URL. |
| `ENTRY_PAGE` | `stock` (default) | validated against a fixed list; `shoppinglist` is the other sensible choice. |

### Locale and formatting

| setting | value here | why |
|---|---|---|
| `DEFAULT_LOCALE` | `en_GB` | **this is the live key.** Validated at startup. |
| `CULTURE` | — | **dead in 4.x.** Read by nothing; `LocaleMiddleware` and the validator both use `DEFAULT_LOCALE`. The nixpkgs module writes it from `services.grocy.settings.culture`, so **that option is inert**. |
| `CURRENCY` | `AUD` | money is only formatted, never converted. |
| `ENERGY_UNIT` | `kJ` | Australian labels are kilojoules. Display only. |
| `CALENDAR_FIRST_DAY_OF_WEEK` | `1` (Monday) | 0 = Sunday. |

### Feature flags — the lever for the overlap

| setting | value here | why |
|---|---|---|
| `FEATURE_FLAG_STOCK` | `true` | the point of the exercise |
| `FEATURE_FLAG_SHOPPINGLIST` | `true` | |
| `FEATURE_FLAG_RECIPES` | `true` | |
| `FEATURE_FLAG_BATTERIES` | `true` | nothing else tracks this |
| `FEATURE_FLAG_EQUIPMENT` | `true` | costs nothing |
| `FEATURE_FLAG_CALENDAR` | `true` | degrades to the remaining kinds |
| **`FEATURE_FLAG_CHORES`** | **`false`** | donetick, with real per-person OIDC identity |
| **`FEATURE_FLAG_CHORES_ASSIGNMENTS`** | **`false`** | follows the above |
| **`FEATURE_FLAG_TASKS`** | **`false`** | vikunja |
| `FEATURE_FLAG_LABEL_PRINTER`, `FEATURE_FLAG_THERMAL_PRINTER` | `false` (default) | no printer |
| `FEATURE_FLAG_DISABLE_BROWSER_BARCODE_CAMERA_SCANNING` | `false` (default) | keep camera scanning |
| `STOCK_BARCODE_LOOKUP_PLUGIN` | `OpenFoodFactsBarcodeLookupPlugin` (default) | note it makes php-fpm reach `world.openfoodfacts.org` on demand. Set empty to disable external lookups. |

### What the nixpkgs module can actually set

**Corrected after implementation.** This section was written against grocy 4.5.0
and nixpkgs `1cabc318`; the locked input is **grocy 4.7.1 on php85**, and the
module has grown an escape hatch. What it said -- "four settings and nothing
else", "no `extraConfig`", "a second definition needs `lib.mkForce`" -- was true
when it was written and is wrong now. Read from
`nixos/modules/services/web-apps/grocy.nix` in the locked tree:

- **`extraConfig` exists** (`types.lines`), and is interpolated into
  `environment.etc."grocy/config.php"` AFTER the module's own `Setting()` calls.
  So every non-secret setting goes through it, with no `mkForce` and no second
  definition of that file -- which is what the aspect does.
- **`settings.entryPage` exists** too, beside `settings.currency`,
  `settings.culture` and the two `settings.calendar.*`. Five, not four.
- `settings.culture` renders `Setting('CULTURE', ...)`. It is still **dead**, but
  for a different reason than this doc assumed: nothing in grocy 4.7.1 reads
  `CULTURE`, and `DEFAULT_LOCALE` is what selects the localisation. (Source read
  during implementation, not measured against a running instance.)
- The module's `phpEnv` sets `GROCY_CONFIG_FILE`, `GROCY_DB_FILE`,
  `GROCY_STORAGE_DIR`, `GROCY_PLUGIN_DIR` and `GROCY_CACHE_DIR`. It does **not**
  set `GROCY_DATAPATH`, and that is what makes the `settingoverrides/` directory
  reachable at all: `public/index.php` otherwise defaults the datapath to
  `<package>/data`, inside the store. Setting it in `phpEnv` is what moves it to
  the state directory the module already creates.

`phpEnv` is still where a SECRET-adjacent or path-valued variable goes, and
`settingoverrides/<NAME>.txt` is still how a secret VALUE gets in without passing
through the store -- `Setting()` reads that file per request, so a rotated bind
password needs no restart.

Two further corrections to ### LDAP, since it was asked, from the same
implementation pass, both read from grocy's source rather than measured:

- The middleware's namespace is **`Grocy\Middleware\Auth\LdapAuthMiddleware`**.
  The class named earlier in this document would now be a config error -- and it
  would be caught, because `ConfigurationValidator::CheckAuthClass` does a
  `class_exists` on `AUTH_CLASS`, so a typo is an error page rather than a silent
  fatal.
- The middleware has **no DN template**. It searches under `LDAP_BASE_DN`, reads
  the `dn` off whichever entry it finds, and re-binds as that dn with the
  submitted password. kanidm composes a returned entry's dn from the entry's rdn
  plus **the base the search asked for**, so the base must be the application's
  branch (`app=grocy,<basedn>`) or the dn grocy binds with routes to the person's
  primary credential -- which cannot be used for an application bind, so every
  login fails. The aspect takes that branch off the capability's
  `userDnTemplate` rather than recomposing the `app=` form itself.

And one thing that is better than this document feared: grocy's own
`/api/system/config` dump unsets all six `LDAP_*` constants
(`SystemApiController`), so the bind password does not leak back out through the
API.

## The API, Home Assistant, and a tablet in the kitchen

grocy's REST API is substantial — 73 paths, including `/stock`,
`/stock/volatile` (expiring/expired/missing), the whole
`by-barcode/{barcode}/{add,consume,open,transfer,inventory}` family,
`/stock/shoppinglist/*`, `/objects/{entity}` generic CRUD, `/recipes/*`,
`/chores/*`, `/batteries/*`, `/calendar/ical` and `/files/{group}/{fileName}`.
Auth is one key, minted in grocy's UI, and **not scoped** — a key is the whole
API.

**Home Assistant.** There is a working integration, `custom-components/grocy`,
giving sensors for stock, expiring/expired/overdue products, missing products,
shopping list, chores, tasks, meal plan and batteries, plus services
`add_product_to_stock`, `consume_product_from_stock`, `execute_chore`,
`complete_task`, `consume_recipe`, `track_battery` and others.

The costs are real:

- it is **HACS, not core**, so `home-assistant.extraComponents` cannot reach it.
  It would be a `buildHomeAssistantComponent` derivation under `packages/`.
- its requirement `pygrocy2` is **not in nixpkgs** — a second derivation.
- it is `config_flow: true`, so even once packaged the instance is added through
  HA's UI. One manual step, permanently.
- it needs a grocy API key, minted in grocy's UI — another manual step, and not
  an agenix secret.

If it is done, HA should point at `gateway.services.grocy.localUrl` and **not**
the public name: going through the gate would mean oauth2-proxy answering HA's
API call with a 302 to a login page. That leg is "unauthenticated and
unattributed" exactly as the option's description warns, and here that is right
— it carries grocy's own API key.

**The kitchen.** What is actually possible, separated from what sounds good:

- **A tablet on `https://grocy.ivymect.in`** is the whole thing and needs no
  integration at all. grocy's in-browser camera scanning is ZXing and
  client-side, and upstream states the constraint: "due to browser security
  restrictions, this only works when serving Grocy via a secure connection
  (`https://`)". The gate's vhost is https; the loopback upstream is not. So the
  tablet uses the public name — which works on the LAN, since the gateway already
  points every gated domain at `127.0.0.1` in `/etc/hosts` on secondpc and the
  wildcard certificate matches.
- **A USB laser scanner** attached to that tablet is upstream's own
  recommendation and needs nothing from this tree: it is a keyboard.
- **An ESPHome device** is the weakest of the three. A scanner on a UART is
  buildable, but ESPHome would publish the barcode as a *state* and something
  still has to turn that into `POST …/consume` — an HA automation calling the
  integration, i.e. it depends on everything above plus a device. It is the fun
  version and it is third in line.

## What it costs

| cost | detail |
|---|---|
| a vhost | `grocy.ivymect.in`, derived from the gate like every other service |
| a **second, internal** vhost | the php server block on `127.0.0.1:9192` — the unusual cost, forced by the nixpkgs module. See the risks. |
| an oauth2-proxy entry | derived, because `oauthProxy` is true |
| a kanidm group | `app-grocy-user`, from the household grant |
| a database | **none.** SQLite in `/var/lib/grocy/grocy.db`. No postgres role, no `takes.postgres`. |
| a secret | **none.** No OIDC client, so no client secret; no DB password. **This is not Phase-B work** — it needs no `rekey` and no `gen-secrets`, which matters while the master key is recovering. |
| a backup entry | `sqlite = [ … ]` plus `paths` |
| php-fpm | a pool with php82, pinned by upstream's platform support |

## The aspect

Shaped after `vikunja.nix`, with three deliberate differences:

- **`oauthProxy = true`** (the default, written out) — grocy's only non-password
  login is a trusted header, so the forward-auth proxy *is* its login.
- **no `takes.postgres`** — grocy is sqlite-only; nixpkgs patches
  `services/DatabaseService.php` to `return getenv('GROCY_DB_FILE')`.
- **two vhosts** — the nixpkgs module renders grocy's server block itself
  (`root`, the php location, and `enableACME` unless `nginx.enableSSL = false`),
  and php-fpm has no TCP listener for the gate to forward to. So the module's
  vhost is pointed at an internal name bound to loopback, `gives.tcp` advertises
  that port, and the gate proxies to it like any other upstream.

The public vhost needs two extra nginx lines, because nixpkgs' oauth2-proxy
integration lifts only the email-derived `X-User` and `X-Email`:

```nginx
auth_request_set $grocy_user $upstream_http_x_auth_request_preferred_username;
proxy_set_header X-Forwarded-Preferred-Username $grocy_user;
```

The household declares `grocy = { }` and the member grant is
`grocy.access = "user"`; the host adds `grocy.enable = true` and
`den.aspects.grocy`. Everything else is derived: the vhost name, the upstream
address, the oauth2-proxy entry, `app-grocy-user`, `svc-grocy`, the `/etc/hosts`
entry, and a firewall that opens nothing.

### The one manual step

Open `https://grocy.ivymect.in`, sign in through kanidm. The account is created
on that first page load and is an ADMIN. **Verify the username is the short name
and not an email or a UUID** — that is the one thing in this design asserted
rather than measured.

## Risks, and what is not known

- **[not evaluated]** None of this has been through an eval. The highest-risk
  line is the internal vhost's `listen` merging with the module's own definition
  of that virtualHost.
- **[not verified]** That `X-Auth-Request-Preferred-Username` is non-empty for
  this kanidm. Each link in the chain is read from source; the end-to-end value
  is not measured. If it is empty the symptom is a **500 from grocy**, not a
  silent admin login.
- **The nixpkgs module fights this tree**, and the workaround is a second nginx
  hop. The alternative is to drop the module and write the php-fpm pool, the
  user, the tmpfiles rules, the config file and the viewcache unit here — about
  40 lines, and the vhost would become ordinary. Worth revisiting if the
  two-vhost arrangement is awkward in practice.
- **`services.grocy.settings.culture` is inert** in grocy 4.x. Use
  `GROCY_DEFAULT_LOCALE`.
- **A local process on secondpc can forge the header** and is then grocy admin.
  Not mitigated.
- **`DEFAULT_PERMISSIONS = ['ADMIN']`** cannot be changed from the environment.
  Narrowing it means taking over the config file with `lib.mkForce` and fixing
  existing users by hand.
- **php82 is pinned by the module**, a version this tree does not otherwise
  carry.
- **[not determined]** Whether grocy's `data/settingoverrides` path is reachable
  under the nixpkgs module: the module creates the directory but never sets
  `GROCY_DATAPATH`, so it is orphaned. Harmless, since the environment path is
  used instead.
