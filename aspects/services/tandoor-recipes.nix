{
  accessLib,
  den,
  ...
}:
# Tandoor Recipes (recipes, meal plans, a shopping list and a pantry), a Django
# app behind the gate.
#
# ── IT IS A REAL OIDC CLIENT, so `oauthProxy = false` ──
#
# Same call ./vikunja.nix and ./donetick.nix make, and for the same reason:
# Tandoor has its own accounts and its own per-space permission model, so
# oauth2-proxy in front would hand it ONE trusted identity for everybody and
# bounce a single login through two providers. ./grocy.nix is `true` because
# grocy has no OIDC code at all -- there the gate IS the login. Tandoor is the
# other case: allauth's `openid_connect` provider is in the package's
# `propagatedBuildInputs`, so there is a second provider to bounce through, and
# the kanidm client below is what makes the identity real.
#
# THE OTHER OPTION WAS AVAILABLE AND IS WORSE. Tandoor also has grocy's
# mechanism: `REMOTE_USER_AUTH=1` inserts `recipes/middleware.py`'s
# `CustomRemoteUser`, which reads `HTTP_REMOTE_USER` -- exactly the header the
# gate's `forwardUser` sets -- and Django's `RemoteUserBackend` creates the
# account. So `oauthProxy = true; forwardUser = true` would work here, and it
# would throw away everything the app can do with a real token: no logout at the
# provider, no session list at `/accounts/sessions/`, no account LINKING for a
# person who already has a local login, and a trusted header as the only thing
# between a misrouted request and somebody else's recipes. grocy takes that
# trade because it has no alternative. Tandoor does.
#
# ── THE SECRET REACHES IT THROUGH AN ENVIRONMENT VARIABLE, NOT A FILE ──
#
# This is the one thing upstream's documentation will mislead you about.
# `SOCIALACCOUNT_PROVIDERS_FILE` is listed in docs/system/configuration.md as a
# path to read the provider JSON from -- and it is implemented ONLY in
# `boot.sh`, the Docker entrypoint, which `export`s the file's contents into
# `SOCIALACCOUNT_PROVIDERS` before exec'ing gunicorn. `recipes/settings.py`
# never looks at it: `grep -rn PROVIDERS_FILE` in the 2.6.13 tree hits boot.sh
# and the docs and nothing else. The nixpkgs module runs gunicorn directly, so
# on NixOS the environment variable is the only route there is.
#
# EVERY `*_FILE` IN THOSE DOCS IS THE SAME TRAP, so do not reach for one later
# and assume it works. `boot.sh` is the sole implementation of all six --
# `SECRET_KEY_FILE`, `SOCIALACCOUNT_PROVIDERS_FILE`, `S3_SECRET_ACCESS_KEY_FILE`,
# `POSTGRES_PASSWORD_FILE`, `EMAIL_HOST_PASSWORD_FILE` and
# `AUTH_LDAP_BIND_PASSWORD_FILE` -- and three of those are exactly the secrets
# this file holds. Which is why one `EnvironmentFile` carries all of them
# instead.
#
# Which means the client secret is an ENV VAR, and an env var in a systemd unit
# must come from an `EnvironmentFile` -- `systemd.services.<n>.environment`
# renders into the unit file in the nix store, world readable. So the whole
# secret-bearing half of the configuration is one agenix TEMPLATE below, and
# `extraConfig` carries only what is safe to publish.
#
# ── AND IT MUST BE SINGLE QUOTED ──
#
# `SOCIALACCOUNT_PROVIDERS` is a JSON object, so its value contains double
# quotes, and systemd's env-file parser treats a `"` inside an unquoted value
# as opening a quoted section -- it would arrive with the quotes stripped and
# `ast.literal_eval` would fail on it. Wrapped in SINGLE quotes systemd passes
# the contents through verbatim, and nothing in the rendered JSON is a single
# quote (the client id is an aspect name, the secret is hex).
let
  # ── THE PORT, ONCE ──
  #
  # Read by `gives.tcp` below -- which is what ./proxy-nginx.nix resolves the
  # upstream from, and what makes the public https address derivable -- and by
  # the service's own option, which is what makes it listen there. A `let` and
  # not `den.lib.capability.ownLinkFor` for the reason ./vikunja.nix gives: that binds
  # a capability by name across the whole host and throws when two aspects give
  # one, which on this box is every service that listens on tcp.
  #
  # NOT the module's default 8080: secondpc already opens 8080 on the LAN
  # (../hosts/secondpc/default.nix's `allowedTCPPorts`) for something else, and
  # a loopback-only app sharing a number with an exposed one is a trap for
  # whoever reads the firewall next.
  port = 8083;

  # The nixpkgs module's `stateDir`, repeated because three things here read it
  # and the module exposes no option for it.
  stateDir = "/var/lib/tandoor-recipes";

  # ── WHERE UPLOADS LAND, AND WHY IT IS STATED RATHER THAN DEFAULTED ──
  #
  # The module derives `MEDIA_ROOT` from `system.stateVersion`: at 26.05 and
  # above it is `<stateDir>/media`, and below it is `<stateDir>` itself. This
  # fleet is pinned at 23.11 (../framework/den.nix), so the default here would
  # be the data directory -- which is GHSA-g8w3-p77x-mmxh: with
  # `GUNICORN_MEDIA` on, `/media/db.sqlite3` is served to anybody. Setting it
  # explicitly is upstream's own recommended remediation, it silences the
  # module's warning, and the module's `StateDirectory` picks up the extra
  # directory because it compares against exactly this string.
  mediaRoot = "${stateDir}/media";

  # ── THE UNIX ACCOUNT, WHICH IS THE MODULE'S OWN ──
  #
  # `tandoor_recipes`, not `tandoor`: the module only creates the account when
  # `user` is left at that default, and the database and role are named after
  # whatever this aspect furnishes through `gives.user` -- so taking the
  # module's name costs nothing and saves this file from declaring a user and a
  # group. Peer authentication checks the connecting unix user against the
  # role, which is why the two have to be the same string and why it is
  # furnished rather than written twice.
  account = "tandoor_recipes";

  # ── THE allauth PROVIDER KEY, WHICH IS ALSO A URL SEGMENT ──
  #
  # `provider_id` in the JSON below and the `/accounts/oidc/<id>/login/callback/`
  # path kanidm redirects to are the SAME string: allauth reverses the callback
  # as `reverse("openid_connect_callback", kwargs={"provider_id": ...})`
  # (`socialaccount/providers/openid_connect/urls.py`), so a mismatch is a
  # callback kanidm refuses rather than a broken button. Written once here for
  # the same reason ./vikunja.nix writes its provider key once.
  providerId = "kanidm";

  # ── THE REGISTRATION, READ OFF THE LINK ──
  #
  # `clientId` and `issuer` come from ./kanidm.nix's `gives.oidc`, so neither
  # string is agreed by hand: the provider names a client after whoever asked,
  # and the issuer is derived from the domain that instance serves.
  #
  # `host = { }` is honest rather than lazy. `den.lib.capability.linkFor` only needs a
  # host to hand the giver's outer scope, and kanidm's `oidc` body reads
  # `self` and `peer` and nothing else -- so this is answerable at FILE level,
  # which is what lets the template below (whose body cannot reach a host's
  # `config`, see ./donetick.nix) interpolate it at all.
  #
  # ── WHY NOT `takes.oidc` ──
  #
  # That is the two-ended form, and it does not work yet. ./kanidm.nix's
  # `realisedFor` turns every `takes.oidc` into
  # `gateway.oidcClients.<asker> = { displayName; basicSecretFile;
  # preferShortUsername; }` -- and ./gateway.nix's `oidcClients` submodule
  # declares neither `basicSecretFile` nor `preferShortUsername` (they are
  # kanidm's own option names, which that file maps `secretFile` onto), and it
  # declares `callback` with no default. So the first aspect to take `oidc`
  # gets two unknown-option errors and a missing required one. There are none
  # in the tree today, which is why it has never fired.
  #
  # Until that is fixed this file does what ./vikunja.nix and ./donetick.nix do
  # -- holds the client secret in its own scope under the name
  # `gateway.oidcClients` derives `secretFile` from, and declares the client
  # through `kanidmClients` -- while still READING the client id and the issuer
  # off the capability rather than restating them.
  oidc =
    let
      l = den.lib.capability.linkFor den.aspects.kanidm "oidc" {
        host = { };
        peer = den.aspects.tandoor;
      };
    in
    if l != null then
      l
    else
      throw "den.aspects.tandoor: kanidm furnishes no `oidc` registration, so there is no client id or issuer to configure allauth with. `den.capabilities.oidc` is declared in ../base/capability-types.nix and answered by ./kanidm.nix's `gives.oidc`.";

  # ── THE DISCOVERY DOCUMENT, WHICH IS PER CLIENT ──
  #
  # kanidm publishes OIDC discovery under the CLIENT, at
  # `<issuer>/oauth2/openid/<client id>/.well-known/openid-configuration` --
  # there is no instance-wide document to fetch. allauth appends the
  # `.well-known` suffix itself when `server_url` has none
  # (`OpenIDConnectProvider.wk_server_url`), so this is the base and not the
  # full URL. ./vikunja.nix composes the same path by hand.
  #
  # The `/oauth2/openid/<client>` segment is kanidm's layout and arguably
  # belongs in its own `gives.oidc.issuer`; it is composed here because that
  # field currently furnishes the bare authority.
  discovery = "${oidc.issuer}/oauth2/openid/${oidc.clientId}";

  # ── THE BUCKET, PROVISIONED AND NOT YET CONSUMED ──
  #
  # Tandoor CAN store uploads in S3: `recipes/settings.py` switches the default
  # Django storage to `CachedS3Boto3Storage` the moment `S3_ACCESS_KEY` is set,
  # and `django-storages`/`boto3` are both in the package. What it then does is
  # the problem.
  #
  # With S3 on, `cookbook/serializer.py`'s `get_image` hands the BROWSER
  # `storage.url(...)`, which is either a presigned URL against
  # `S3_ENDPOINT_URL` or, if `S3_CUSTOM_DOMAIN` is set, an UNSIGNED URL under
  # that domain (`storages/backends/s3.py`: the custom-domain branch signs only
  # through a CloudFront signer, which this is not). ./garage.nix binds all
  # three of its listeners to 127.0.0.1 and deliberately publishes none of
  # them, so the first form is a URL no browser can reach and the second needs
  # anonymous reads on the bucket plus a published name to serve them from.
  #
  # A path prefix does not rescue it either: SigV4 signs the canonical URI and
  # the Host, so an `https://tandoor.<zone>/s3/...` endpoint that nginx rewrites
  # on the way to garage fails the signature, and one that does not rewrite
  # makes `s3` the bucket name.
  #
  # So this is where it stops being Tandoor's decision. Turning it on means
  # publishing garage's S3 API under a name of its own -- `proxy_set_header Host
  # $host` with no `root_domain` set keeps garage in path-style mode and the
  # signature intact -- or enabling garage's `s3_web` listener and allowing
  # anonymous reads on this bucket. ./garage.nix declines to publish anything
  # today and says why; that choice is not this file's to reverse.
  #
  # The bucket and the key are provisioned anyway, which is exactly what
  # ./garage.nix already does for `splitpro`: it costs one generated secret, it
  # is what any S3 client has to be handed, and it means flipping
  # `s3Endpoint` below is the whole change rather than the start of one.
  #
  # SET THIS to the public S3 base (`https://s3.<zone>`) once garage is
  # reachable under it, and `extraConfig` picks up the rest.
  s3Endpoint = null;

  # Garage's own region string, signed into every request's credential scope.
  # ./garage.nix's `s3_region`.
  s3Region = "garage";

  s3Bucket = "tandoor";
in
{
  den.aspects.tandoor = {
    includes = [
      den.aspects.gateway
      den.aspects.postgres
      den.aspects.agenix-rekey
      # ./restic.nix's `backup` quirk -- see the `backup` entry below.
      den.aspects.restic
      # For `garage.buckets` below. The option is garage's, not a den class, so
      # the aspect that declares it has to be on the host.
      den.aspects.garage
    ];

    # What access this service exposes, for ../persons/<name>.nix to be granted.
    # Collected by ./access.nix, which turns a grant into a kanidm group and
    # adds that group to this service's own `gated` groups.
    gives =
      { self, ... }:
      {
        # ── WHAT A GRANT HERE MEANS ──
        #
        # Two levels, like ./vikunja.nix and unlike ./grocy.nix: Tandoor has a
        # real per-space permission model (guest/user/admin groups of its own)
        # so `admin` is a distinction that means something inside the app --
        # though nothing maps the kanidm group onto it today. What the grant
        # actually controls is the kanidm group's existence; the gate admits
        # nobody on this vhost (`oauthProxy = false`) and authorisation is the
        # client's scope map plus Tandoor's own spaces.
        service = _: {
          access = {
            levels = [
              "user"
              "admin"
            ];
            defaultLevel = "user";
          };
        };

        # A TILE, BECAUSE A GRANT IS MEANT TO BE VISIBLE. ../households/
        # ivymectin.nix grants this to every resident, and ./homepage.nix
        # derives a tile's audience from those same grants -- so without a
        # widget here the grant authorised people and showed them nothing.
        #
        # No `href`: this is published through the gateway, so ./homepage.nix
        # derives the url from `gateway.services.<n>.url`. Stating one is for
        # a plain vhost (../hosts/secondpc/plex.nix says why).
        homepage-widget = _: {
          group = "Productivity";
          icon = "tandoor.svg";
          description = "Recipes";
        };

        # WHAT IT LISTENS ON, so ./proxy-nginx.nix can ASK rather than assume:
        # the upstream is resolved by composing this endpoint with the carrier
        # the two ends share, which for a service on this box is the loopback.
        #
        # NOT EXPOSED, written out rather than defaulted: ../base/firewall.nix
        # derives itself from `gives`, so an empty `expose` is what keeps this
        # port shut on every wire while leaving it visible to resolution.
        tcp = _: {
          name = self.name;
          ports.http = {
            port = port;
            meta.description = "Tandoor's gunicorn listener, which serves the app and the recipe uploads both; dialed by the gate.";
            expose = [ ];
          };
        };

        # THE ACCOUNT IT RUNS AS, furnished rather than restated. ./postgres.nix
        # derives the database and the role from this, because peer
        # authentication checks the connecting unix user against the role -- so
        # this is why the database is `tandoor_recipes` and not `tandoor`.
        user = _: {
          name = account;
          group = account;
          carrier = self.name;
        };
      };

    takes.gated = {
      # A real OIDC client -- see the header.
      oauthProxy = false;

      # Recipe imports are zip archives of a whole collection, and Tandoor's own
      # ceiling is 500MB (`MAX_ZIP_TOTAL_SIZE`). 64MiB is well past any single
      # recipe's images and comfortably under Cloudflare's 100MB proxied-body
      # cap, so this vhost does not become one ../network/cloudflare.nix has to
      # exclude from the tunnel.
      maxBodyBytes = 67108864;
    };

    # ── PEER AUTH OVER THE SOCKET, SO NO PASSWORD EXISTS ──
    #
    # `tcp` is deliberately absent. The unit runs as `tandoor_recipes` and the
    # role is named after it, so psycopg2 pointed at `/run/postgresql`
    # authenticates by uid and there is no credential to mint, place or rotate.
    # The module's own hardening already binds that socket into the unit
    # (`BindReadOnlyPaths` has `-/run/postgresql`), and `PrivateUsers = true`
    # does not break it: the service's own uid is identity-mapped, so
    # `SO_PEERCRED` outside the namespace still reads `tandoor_recipes`. That is
    # the arrangement nixpkgs' own `tandoor-recipes` test exercises.
    takes.postgres.services = [ "tandoor-recipes" ];

    # Recipes, meal plans and the shopping list are in postgres and therefore
    # already covered by ./restic.nix's dump of every provisioned database.
    # UPLOADED IMAGES are not: Tandoor writes the file under `MEDIA_ROOT` and
    # keeps a row pointing at it, so a database-only restore gives you every
    # recipe with every picture broken -- the same split ./vikunja.nix's
    # attachments and ./grocy.nix's `storage/` have.
    #
    # `mediaRoot` and not `stateDir`: the rest of the state directory is the
    # module's `manage` symlink and nothing else.
    backup = _: {
      name = "tandoor";
      paths = [ mediaRoot ];
    };

    secrets = {
      # ── THE NAME IS THE INTERFACE ──
      #
      # `gateway.oidcClients.<n>.secretFile` defaults to the path of age secret
      # `<secretScope>/<secretName>`, and both default to the client name and
      # `oidc-client-secret` -- so a client following the convention says
      # nothing about its secret at all and kanidm reads this file as
      # `basicSecretFile`. Hence `owner = "kanidm"` and the kanidm restart:
      # it is kanidm that holds this value, not Tandoor.
      oidc-client-secret = {
        owner = "kanidm";
        restartUnits = [ "kanidm.service" ];
        generator.script = "random-hex-32";
      };

      # Django's `SECRET_KEY`: signs session cookies, the CSRF token and
      # password-reset links. Upstream's default is the literal
      # `INSECURE_STANDARD_KEY_SET_IN_ENV`, which is not a placeholder anybody
      # is forced past -- the app starts happily with it.
      #
      # NOT chowned to `tandoor_recipes`: this is consumed only as a dependency
      # of the template below, which agenix renders as root.
      django-secret-key.generator.script = "random-hex-32";

      # ── THE S3 KEY'S SECRET HALF, HELD HERE AND NOT IN garage's SCOPE ──
      #
      # ./garage.nix's header is explicit about which scope this belongs in and
      # why: a template may not depend on another scope's secret (that closes
      # `age.templates` -> `age.scoped.<other>.access` -> `age.templates`, and
      # only at `gen-secrets`), so the material lives with the CONSUMER whose
      # env template interpolates it, and garage's provisioning oneshot reads
      # the file by PATH at activation -- a plain cross-scope read, which never
      # enters `age.templates`.
      #
      # `garage key import` rather than `garage key create` is what makes that
      # possible at all: the key id and secret are chosen here and installed
      # into garage, so both ends know them before the node has ever run.
      s3-secret-key.generator.script = "random-hex-32";
    };

    # ── EVERYTHING SECRET, IN ONE ENVIRONMENT FILE ──
    #
    # Read by systemd as root before it drops privileges, which is why there is
    # no `owner` here: `EnvironmentFile` does not need the service to be able
    # to read the file, and a 0400 root-owned copy is the tighter answer.
    #
    # `restartUnits` is stated rather than inferred. ../../lib/age-scoped.nix
    # infers it from `config.services.<scope>`, and this scope is named after
    # the ASPECT (`tandoor`) while the service is `tandoor-recipes` -- so the
    # inference finds nothing and a rotated secret would sit on disk unread.
    templates.env =
      { secrets, ... }:
      {
        restartUnits = [ "tandoor-recipes.service" ];
        dependencies = {
          inherit (secrets) oidc-client-secret django-secret-key s3-secret-key;
        };
        content =
          { placeholders, ... }:
          ''
            SECRET_KEY=${placeholders.django-secret-key}
            SOCIALACCOUNT_PROVIDERS='{"openid_connect":{"APPS":[{"provider_id":"${providerId}","name":"ivymect.in","client_id":"${oidc.clientId}","secret":"${placeholders.oidc-client-secret}","settings":{"server_url":"${discovery}"}}]}}'
            S3_SECRET_ACCESS_KEY=${placeholders.s3-secret-key}
          '';
        # UNCONDITIONAL, even while `s3Endpoint` is null: `S3_ACCESS_KEY` is
        # the switch `recipes/settings.py` reads, so the secret half sitting in
        # the environment unused turns nothing on -- and it means enabling S3
        # later is one `let` binding rather than a binding plus a `gen-secrets`
        # run, which ../../CLAUDE.md forbids while the master key is recovering.
      };

    # `kanidmClients.<name>`: the shared OIDC shape (short username, the scope
    # map, the landing page, the domain, and `secretFile` derived from this
    # aspect's own scope) is defaulted by `gateway.oidcClients` in ./gateway.nix,
    # so this is only what differs.
    kanidmClients.tandoor = {
      displayName = "Tandoor Recipes";

      # allauth's own callback route, and `providerId` is the URL segment it
      # reverses from -- see that binding's note. The trailing slash is Django's
      # and is not optional: `APPEND_SLASH` would redirect the callback and lose
      # the query string.
      callback = "/accounts/oidc/${providerId}/login/callback/";

      # No `groupsName`: Tandoor maps no OIDC claim onto its own groups. The
      # scope map is itself the gate -- kanidm refuses a token to anyone outside
      # the groups `scopes` below names -- and inside the app membership is a
      # space's own fact.
      # ── AND WHO MAY HAVE A TOKEN AT ALL, WHICH IS THE GRANT ──
      #
      # `scopes` REPLACES the `media-users` default (./gateway.nix), so this
      # client issues a token only to the groups a grant on tandoor produces.
      # That is the narrowing the comment above already claimed: "the scope map
      # is itself the gate" was true of a scope EVERY client shared, so being
      # in `media-users` bought all of them at once and a grant on one bought
      # nothing.
      #
      # ./access.nix owns the spelling and answers with the GRANTED levels
      # rather than the declared ones -- a scope map naming a group nobody was
      # granted is a group kanidm never provisioned, and the provisioning run
      # fails on it.
      scopes = accessLib.groupsFor "tandoor";
    };

    nixos =
      {
        config,
        lib,
        scoped,
        host ? { },
        ...
      }:
      {
        services.tandoor-recipes = {
          enable = true;

          # ── WHERE IT BINDS, DERIVED FROM `expose` RATHER THAN WRITTEN ──
          #
          # `gives.tcp.ports.http.expose` is `[ ]`, so this resolves the
          # loopback carrier's own address (../network/segments.nix furnishes
          # `127.0.0.1` beside `lo`) and the literal appears nowhere. One
          # declaration now decides three things that used to be written
          # separately: which wires ../base/firewall.nix opens, which interface
          # it opens them on, and what the service binds -- so exposing this
          # port later is one edit rather than three that can disagree.
          #
          # It also sets the module's own `ALLOWED_HOSTS` default, which
          # `extraConfig` below replaces.
          address = den.lib.capability.bindAddressFor host den.aspects.tandoor "tcp" "http";
          port = port;

          # NOT the module's own postgres. `createLocally` writes its own
          # `services.postgresql` block and its own `POSTGRES_*` -- which is the
          # job `takes.postgres` above already does through ./postgres.nix, with
          # the ordering, the extensions and the one cluster every service on
          # this box shares.
          database.createLocally = false;

          # ── EVERY NON-SECRET SETTING ──
          #
          # `extraConfig` lands in `systemd.services.tandoor-recipes.environment`
          # and therefore in the unit file in the nix store, so nothing here may
          # be secret. The three that are come from `environmentFile` below.
          #
          # VALUES ARE `toString`ed BY THE MODULE, and `extract_bool` in
          # `recipes/settings.py` is `bool(int(os.getenv(...)))` -- so a flag
          # has to be 1 or 0. A nix `false` would `toString` to the EMPTY
          # STRING and `int("")` raises, taking the whole app down at import;
          # `true` happens to give "1". Written as integers so neither trap is
          # reachable.
          extraConfig = {
            DB_ENGINE = "django.db.backends.postgresql";
            # A directory, not a host: psycopg2 reads a leading `/` as the unix
            # socket directory, which is what makes peer auth apply.
            POSTGRES_HOST = "/run/postgresql";
            POSTGRES_USER = account;
            POSTGRES_DB = account;
            # No POSTGRES_PASSWORD, and that is the point of the socket.

            # See `mediaRoot` above: explicit because this fleet's stateVersion
            # puts the module's default on top of the data directory.
            MEDIA_ROOT = mediaRoot;

            # ── WHO MAY ARRIVE, AND UNDER WHICH NAME ──
            #
            # Django rejects a request whose Host is not listed, and nginx
            # forwards the original Host (`recommendedProxySettings`), so the
            # public name is the one that has to be here. The loopback pair is
            # for anything on the box that dials the upstream directly.
            ALLOWED_HOSTS = "tandoor.${config.gateway.domain},localhost,127.0.0.1";

            # Scheme included, which Django requires since 4.0. Without it
            # every POST from the UI is a 403 behind the proxy. Composed from
            # the same `config.gateway.domain` as `ALLOWED_HOSTS` above rather
            # than from a second source that could disagree with it.
            CSRF_TRUSTED_ORIGINS = "https://tandoor.${config.gateway.domain}";

            # ── SIGN IN WITH kanidm, AND ALMOST NOTHING ELSE ──
            #
            # `SOCIAL_PROVIDERS` is what loads the allauth provider app at all;
            # the credentials for it are in the environment file.
            SOCIAL_PROVIDERS = "allauth.socialaccount.providers.openid_connect";

            # Straight to kanidm rather than through allauth's "continue to
            # provider" interstitial.
            SOCIALACCOUNT_LOGIN_ON_GET = 1;

            # ── HIDDEN, NOT DISABLED, AND THAT IS DELIBERATE ──
            #
            # `HIDE_LOGIN_FORM` takes the password form out of the UI while
            # leaving `/accounts/login/?form=1` as break-glass;
            # `SOCIALACCOUNT_ONLY = 1` would remove local authentication
            # entirely. Upstream recommends the former and so does this box's
            # shape: Tandoor needs a superuser for `/admin/` and for space
            # administration, and kanidm being down must not be the thing that
            # locks the household out of its own recipes. ./vikunja.nix makes
            # the opposite call (`local.enabled = false`) because Vikunja has
            # no comparable admin surface.
            HIDE_LOGIN_FORM = 1;

            # The local registration form, which nothing should reach. Social
            # signup is governed separately, below.
            ENABLE_SIGNUP = 0;

            # ── NO `SOCIAL_DEFAULT_ACCESS`, AND THAT IS NOT AN OMISSION ──
            #
            # It reads like the answer to "a new person signs in and has no
            # space", and in this configuration it would do nothing at all.
            # The setting is consumed in exactly one place --
            # `AllAuthSocialSignupForm.signup` in `cookbook/forms.py` -- and
            # allauth only calls a form's `signup()` hook when the signup FORM
            # is used. On the auto-signup path (`SOCIALACCOUNT_AUTO_SIGNUP`,
            # which defaults to 1) `socialaccount/internal/flows/signup.py`
            # calls `save_user(request, sociallogin, form=None)` and the hook
            # never runs. So setting it while leaving auto-signup on is a line
            # that looks like policy and is dead.
            #
            # Turning auto-signup OFF to make it live costs a signup form on
            # first login whose fields come from `ACCOUNT_SIGNUP_FIELDS`
            # (email twice, username), which is not obviously better than the
            # alternative. So the alternative is stated instead: SPACE ACCESS
            # IS AN INVITE LINK, minted in the UI by whoever owns the space.
            # That is a manual step per person and it is not declarable --
            # an `InviteLink` is a row in Tandoor's own database, the same
            # class of thing as grocy's API key (../../docs/grocy.md).
            #
            # The FIRST person to sign in is the one who creates the space and
            # owns it; everybody after needs a link from them.

            # ── gunicorn SERVES THE UPLOADS ──
            #
            # Nothing else can. The module configures no web server, nginx
            # cannot read the files (the unit's `UMask = "0066"` leaves them
            # 0600 under `tandoor_recipes`), and with `MEDIA_ROOT` set to its
            # own subdirectory this no longer serves the data directory. It is
            # also what upstream's own container does.
            #
            # UNAUTHENTICATED, like every Tandoor deployment: `django.conf.urls
            # .static` has no login check, and this vhost has no `auth_request`
            # in front of it either (`oauthProxy = false`). Recipe images are
            # readable by anyone who knows the URL. That is upstream's model and
            # the reason S3 with signed, expiring URLs is worth having later.
            GUNICORN_MEDIA = 1;

            # Australian labels are kilojoules, the same display-only choice
            # ./grocy.nix makes.
            KJ_PREF_DEFAULT = 1;
          }
          # ── THE S3 HALF, INERT UNTIL THERE IS A PUBLIC ENDPOINT ──
          #
          # `S3_ACCESS_KEY` is the switch: `recipes/settings.py` moves the
          # default storage to S3 the moment it is non-empty, which is why this
          # is all-or-nothing rather than a set of harmless variables. See
          # `s3Endpoint` above for what has to be true first.
          #
          # The key ID is NOT a secret -- it is the username half -- so it
          # belongs here and only `S3_SECRET_ACCESS_KEY` is in the template.
          // lib.optionalAttrs (s3Endpoint != null) {
            S3_ACCESS_KEY = config.garage.buckets.${s3Bucket}.keyId;
            S3_BUCKET_NAME = config.garage.buckets.${s3Bucket}.bucket;
            S3_ENDPOINT_URL = s3Endpoint;
            S3_REGION_NAME = s3Region;
            # Presigned, expiring URLs rather than public objects. The whole
            # reason to prefer S3 over `GUNICORN_MEDIA` above.
            S3_QUERYSTRING_AUTH = 1;
          };
        };

        # ── THE SECRETS, AS AN EnvironmentFile ──
        #
        # The nixpkgs module has no option for this, so it is written onto the
        # unit directly. `serviceConfig` merges by attribute and the module
        # defines no `EnvironmentFile`, so there is nothing to force here.
        #
        # ORDER DOES NOT MATTER because the keys are disjoint: everything in
        # `extraConfig` above is a different variable from the three in the
        # template. Overriding one of those from here would depend on whether
        # NixOS emits `Environment=` before or after `EnvironmentFile=`, which
        # is not a thing to rely on.
        systemd.services.tandoor-recipes.serviceConfig.EnvironmentFile = [
          scoped.tandoor.templates.env.path
        ];

        # The bucket ./garage.nix reconciles against the running node, in the
        # shape its header prescribes: the declaration is here, the enforcement
        # is its `garage-provision` oneshot, and the key id defaults to
        # `GK${s3Bucket}`.
        #
        # `read` and `write` and not `owner`: an uploader has no business
        # deleting the bucket or changing its website configuration.
        garage.buckets.${s3Bucket}.secretKeyFile = scoped.tandoor.secrets.s3-secret-key.path;

        # Ordering against postgres (and against the database existing) comes
        # from `takes.postgres.services` above. This is the same five seconds
        # ./vikunja.nix gives a Django-shaped app whose first start runs
        # migrations.
        systemd.services.tandoor-recipes.serviceConfig.RestartSec = "5s";
      };
  };
}
