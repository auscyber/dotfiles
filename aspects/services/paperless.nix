{
  accessLib,
  den,
  ...
}:
# paperless-ngx: scan a receipt in, get a searchable PDF and its OCR text out.
#
# This is the INTAKE half of ../../docs/receipts-and-expenses.md. It is written
# as an aspect because it is unambiguous on its own: a nixpkgs module, a
# postgres role, a gated vhost and nothing speculative. The three things that
# would consume what it produces -- grocy stock lines, SplitPro shares, Up Bank
# transactions -- are NOT here, because none of them is unambiguous yet and the
# document says exactly why for each.
#
# ── WHAT IT CAN AND CANNOT EXTRACT, STATED UP FRONT ──
#
# Document-level OCR text, plus whatever regex-matched `custom_fields`, tags and
# correspondents a workflow assigns. That is all. Paperless-ngx has NO
# line-item or table parser: `RemoteDocumentParser` (the only non-tesseract
# path) speaks to Azure Document Intelligence and still returns text plus a
# searchable PDF, not rows. So "split line 4 of this receipt" is not a question
# this service can answer, and nothing downstream should be written as though
# it could. See the document's own section on the weakest link.
#
# ── NO SECRETS, WHICH IS LOAD BEARING RIGHT NOW ──
#
# Like ./grocy.nix and for the same reason: the gate authenticates the person
# and peer authentication authenticates the database connection, so there is no
# OIDC client secret, no database password and no API token in this file.
# Nothing to `rekey` and nothing to `gen-secrets`, which is what makes it
# deployable while the master key is mid-recovery (../../CLAUDE.md).
#
# `PAPERLESS_SECRET_KEY` is the one credential the service needs and the nixpkgs
# module generates it at first start into `${dataDir}/nixos-paperless-secret-key.env`
# -- so it is state to back up (it is under `dataDir`, see `backup` below) and
# not a secret to declare.
#
# THE ALTERNATIVE WAS REAL OIDC and it was declined for now, not overlooked.
# paperless-ngx is a django-allauth app and `PAPERLESS_SOCIALACCOUNT_PROVIDERS`
# would make it a proper kanidm client with `PAPERLESS_SOCIAL_ACCOUNT_SYNC_GROUPS`
# mapping a kanidm group onto a paperless group of the same name. That buys
# automatic per-person authorisation -- which, as the `forwardUser` note below
# records, is the one thing this arrangement cannot do. It costs a client secret
# (unmintable today) and a paperless group row that still has to be created by
# hand. The document weighs the two.
let
  # ── THE PORT, ONCE ──
  #
  # Read by `gives.tcp` below -- which is what ./proxy-nginx.nix resolves the
  # upstream from and what makes the public https address derivable -- and by
  # the module's own `GRANIAN_PORT`. The module's default is this same 28981;
  # written out because two readers must agree and a default agreed by
  # coincidence is not an agreement.
  port = 28981;

  # The module's own default, repeated here because `backup` below reads it.
  #
  # `mediaDir` and `consumptionDir` were bound here too, justified as "`backup`
  # reads them" -- it does not. It takes `dataDir` whole, and both of those live
  # under it, so naming them bought nothing and the comment claiming otherwise
  # was the only thing keeping them alive.
  dataDir = "/var/lib/paperless";

  # ── THE UNIX ACCOUNT, WHICH IS THE MODULE'S OWN ──
  #
  # `paperless`, the module's `defaultUser`: it only creates the account (and
  # puts it in redis's group, which the celery workers need) when `user` is left
  # alone. ./postgres.nix derives the database and the role from whatever
  # `gives.user` furnishes, and peer authentication checks the connecting unix
  # user against the role -- so the three strings have to be one string, and
  # taking the module's saves declaring a user and a group here. Same move as
  # ./tandoor-recipes.nix's `tandoor_recipes`.
  account = "paperless";

  # ── ONE ACCESS LEVEL, AND IT IS HONESTY RATHER THAN LAZINESS ──
  #
  # Paperless has a real permission model -- django groups, per-object view and
  # change -- and NOTHING HERE CAN REACH IT. Under remote-user authentication
  # django's `RemoteUserBackend` creates the account and allauth's
  # `CustomAccountAdapter.save_user` (which is what reads
  # `PAPERLESS_ACCOUNT_DEFAULT_GROUPS`) never runs, so a person arrives with no
  # groups and no permissions at all. A second level here would open the same
  # gate and change nothing inside the app, exactly as ./grocy.nix's would.
  #
  # The consequence is a human step, named precisely rather than papered over --
  # see `forwardUser` below.
  accessLevel = "user";
in
{
  den.aspects.paperless = {
    includes = [
      den.aspects.gateway
      den.aspects.postgres
      # ./restic.nix's `backup` quirk -- see the `backup` entry below.
      den.aspects.restic
    ];

    gives =
      { self, ... }:
      {
        # WHAT A GRANT HERE MEANS. One level, for the reason `accessLevel`
        # above gives. It IS what authorises the login: the gate admits the
        # union of a vhost's groups and ./access.nix turns a grant at this
        # level into one of them, so a person with no grant never reaches
        # paperless at all.
        service = _: {
          access = {
            levels = [ accessLevel ];
            defaultLevel = accessLevel;
          };
        };

        # WHAT IT LISTENS ON, so ./proxy-nginx.nix can ASK rather than assume.
        # `expose = [ ]` -- the gate is the way in, so ../base/firewall.nix
        # opens nothing on any wire and the same declaration is what the
        # listener binds (see `address` in the nixos block).
        tcp = _: {
          name = self.name;
          ports.http = {
            inherit port;
            meta.description = "Paperless' own web listener, UI and REST API together, dialed by the gate over the loopback.";
            expose = [ ];
          };
        };

        # THE ACCOUNT EVERY paperless-* UNIT RUNS AS, furnished rather than
        # restated, because ./postgres.nix reads it to name the role.
        user = _: {
          name = account;
          group = account;
          carrier = self.name;
        };
      };

    # ── THE DATABASE: THE SERVER THIS BOX ALREADY RUNS ──
    #
    # NOT `database.createLocally`, which would have the module declare its own
    # `services.postgresql` block beside ./postgres.nix's -- the same mistake
    # ./tandoor-recipes.nix records. The role and the database are both
    # `paperless`, derived from `gives.user` above, and the connection is the
    # unix socket under peer authentication, so there is no password anywhere.
    #
    # ALL FOUR UNITS, not just the web one. `paperless-scheduler` runs
    # `migrate` in its `preStart` before anything else touches the schema, so
    # if that one is not ordered after the database the first boot fails in a
    # way the others would not explain.
    takes.postgres.services = [
      "paperless-scheduler"
      "paperless-task-queue"
      "paperless-consumer"
      "paperless-web"
    ];

    takes.gated = {
      # ── THE GATE IS THE LOGIN ──
      #
      # oauth2-proxy in front, as ./grocy.nix does, and NOT because paperless
      # cannot do OIDC -- it can, it is an allauth app. Because doing it that
      # way needs a client secret that cannot be minted today. See the header.
      oauthProxy = true;

      # ── AND THE IDENTITY IT ESTABLISHED ──
      #
      # ../base/capability-types.nix's `forwardUser` sets `Remote-User` on the
      # proxied request from oauth2-proxy's `preferred_username` -- kanidm's
      # SHORT name, which is what every other service in this house already
      # calls that person. Django's `RemoteUserBackend` reads it as
      # `HTTP_REMOTE_USER` and creates the account on first visit.
      #
      # IT FAILS CLOSED on the browser leg, and that is what makes trusting a
      # header acceptable here: ./gateway.nix emits `proxy_set_header
      # Remote-User $gw_person`, `proxy_set_header` OVERWRITES, and nginx drops
      # the header entirely when the variable is empty. So a client sending its
      # own `Remote-User:` cannot be believed and an unauthenticated request
      # arrives with no identity rather than somebody else's.
      #
      # ── WHAT IT CANNOT DO, AND THE ONE HUMAN STEP ──
      #
      # UNLIKE grocy, whose `UsersService::CreateUser` hands every new account
      # `DEFAULT_PERMISSIONS = ['ADMIN']`, a paperless account created this way
      # has NO django groups and NO model permissions, and
      # `PAPERLESS_ACCOUNT_DEFAULT_GROUPS` does not apply to it (that setting
      # is read by allauth's adapter, which `RemoteUserBackend` bypasses). The
      # person signs in successfully and then sees nothing.
      #
      # So, once per person, after their first sign-in, by somebody who is
      # already a superuser:
      #
      #   Settings -> Users & Groups -> <them> -> grant permissions
      #
      # and once per install, to get the FIRST superuser -- there is no
      # `passwordFile` here, deliberately, because that is a secret and this
      # aspect has none:
      #
      #   paperless-manage createsuperuser --username <kanidm short name>
      #
      # Same username as the gate will forward, so the superuser account and
      # the remote-user account are one account. The wrapper is on every
      # system's PATH (`environment.systemPackages` in the nixpkgs module).
      #
      # ../../docs/receipts-and-expenses.md argues this is the step worth
      # paying an OIDC client secret to remove, once secrets can be minted.
      forwardUser = true;

      # ── THE STATUS SOCKET ──
      #
      # paperless-web is granian with `--ws` and the Angular frontend opens
      # `/ws/status` to follow a consumption task. nixpkgs' own
      # `configureNginx` block proxies that location with `proxyWebsockets`;
      # the gate puts one location in front of the whole vhost, so saying it
      # once here covers it. Without this an upload appears to hang: the
      # document consumes fine and the UI never hears that it did.
      websockets = true;

      # ── BIG ENOUGH FOR A SCAN, SMALL ENOUGH FOR THE TUNNEL ──
      #
      # nginx's own default is 1MB, which a multi-page colour scan passes
      # immediately, and the failure is a 413 from the proxy rather than
      # anything paperless says. 50MB is comfortably under
      # ../network/cloudflare.nix's `tunnelBodyCap` of 100MB, so this vhost
      # stays ON the tunnel -- a limit ABOVE that cap takes the name off the
      # tunnel through `excluded`, which is the right behaviour and not what is
      # wanted here.
      maxBodyBytes = 50 * 1000 * 1000;
    };

    # ── WHAT A GRANT ON PAPERLESS DOES, DECLARED WHERE THE GATE IS ──
    #
    # The gate is paperless's login too, so a grant is one group added to the
    # vhost's allowed set -- the same shape as ./grocy.nix, which is the point:
    # two services wanting the same realisation write the same line rather than
    # each inventing one. ./access.nix used to write it for both from a single
    # central `mapAttrs`, which is why it could only ever realise a gate.
    #
    # It does NOT grant django permissions: those are the human step the
    # `forwardUser` note above spells out. The gate decides who gets in,
    # paperless decides what they see.
    gated.paperless.extraGroups = accessLib.groupsFor "paperless";

    # ── WHAT HAS TO GO OFF THE BOX ──
    #
    # The originals and the archived PDFs under `mediaDir`, which exist nowhere
    # else, and the rest of `dataDir`: the tantivy search index (rebuildable
    # with `document_index reindex`, but slowly), the classifier model, and
    # `nixos-paperless-secret-key.env` -- which is NOT rebuildable, because
    # regenerating it invalidates every session and every stored password
    # reset.
    #
    # The document metadata is in postgres and therefore already covered by
    # ./restic.nix's dump of every provisioned database. The same split as
    # ./tandoor-recipes.nix's `mediaRoot` and ./vikunja.nix's attachments: rows
    # in the dump, bytes in `paths`, and a database-only restore gives you every
    # document with nothing behind it.
    #
    # NOT `services.paperless.exporter`, which is the module's other answer.
    # Its unit `Conflicts` all four paperless services, so every run stops the
    # whole stack for the length of a full re-export -- a sensible trade for a
    # migration and a poor one for a nightly. Left off (its own default) so
    # there is one backup mechanism on this box rather than two.
    backup = _: {
      name = "paperless";
      paths = [ dataDir ];
    };

    nixos =
      {
        config,
        host ? { },
        ...
      }:
      {
        services.paperless = {
          enable = true;

          # ── THE ADDRESS IS RESOLVED, NOT WRITTEN ──
          #
          # Same as ./grocy.nix: `gives.tcp.ports.http.expose = [ ]` is the one
          # declaration that decides which wires ../base/firewall.nix opens
          # (none) and what this listener binds, and the loopback carrier
          # (../network/segments.nix) furnishes the string.
          address = den.lib.capability.bindAddressFor host den.aspects.paperless "tcp" "http";
          inherit port;

          # ── THE PUBLIC NAME, ASKED FOR RATHER THAN RESTATED ──
          #
          # Setting `domain` makes the module set `PAPERLESS_URL`, which is what
          # django reads for `ALLOWED_HOSTS` and `CSRF_TRUSTED_ORIGINS` -- so
          # without it the login POST is a 403 on `Origin checking failed`.
          #
          # Read off the gateway entry instead of composing
          # `"paperless.${config.gateway.domain}"` by hand, which is what
          # ./tandoor-recipes.nix and ./vikunja.nix do. The entry's `domain` is
          # already `<name>.<gateway.domain>` and it is the string nginx's
          # `server_name` and cloudflare's ingress are both built from, so
          # asking it is the only form that cannot drift from the vhost that
          # actually answers.
          #
          # `configureNginx` STAYS OFF (its default): the module would render a
          # second vhost on 80/443 for this same name beside the gate's.
          domain = config.gateway.services.paperless.domain;

          settings = {
            # ── AUTHENTICATION: THE PROXY SAID WHO THIS IS ──
            #
            # See `forwardUser` above for the mechanism and for the one thing
            # this does not do.
            PAPERLESS_ENABLE_HTTP_REMOTE_USER = true;

            # The django `META` key, which is upstream's own default. Stated
            # because it is the other half of the gate's `Remote-User` header
            # and the two have to agree, and because the `HTTP_` prefix is the
            # fact worth reading here: django names an INBOUND HEADER, where
            # ./grocy.nix's `REVERSE_PROXY_AUTH_USE_ENV` reads a server
            # variable a client provably cannot write. That option has no
            # analogue in django, so paperless's version of this is forgeable
            # in principle and safe only because the gate overwrites the header
            # -- which is why the next setting is false.
            PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME = "HTTP_REMOTE_USER";

            # ── FALSE, AND THIS IS THE SETTING WITH TEETH ──
            #
            # Upstream splits the API out from the UI precisely so that a
            # reverse-proxy configuration cannot extend remote-user trust to
            # the API by accident, and this tree is exactly the configuration
            # it is warning about. ./gateway.nix renders TWO locations for a
            # service with `api.enable`: the UI location carries the
            # `proxy_set_header Remote-User $gw_person` pair, and the API
            # location does NOT -- it rewrites the API KEY header and nothing
            # else. So a request to `/api` through the gate keeps whatever
            # `Remote-User` the caller sent.
            #
            # With this true, that is authentication as any user by asking.
            # With it false, `/api` needs a django REST token and the forged
            # header means nothing. This aspect also does not set
            # `takes.nginx.api`, so no API location exists today; the setting
            # is here so that adding one later cannot quietly turn this on.
            #
            # The consequence for the bridge that would read receipts out of
            # here: it needs a token, a token is a row in paperless's own
            # database, and the only ways to get one are the UI (My Profile ->
            # the circular arrow) or `paperless-manage drf_create_token <user>`
            # -- `rest_framework.authtoken` is in `INSTALLED_APPS`, so the CLI
            # path exists. ../../docs/receipts-and-expenses.md treats that as a
            # CLI to drive from a one-shot rather than an API to provision,
            # which is the cheaper of ../../docs/api-provisioning.md's four
            # cases.
            PAPERLESS_ENABLE_HTTP_REMOTE_USER_API = false;

            # Nothing for a person to type: the gate already asked. This does
            # NOT cover `/admin/` or local credentials on the API, which is
            # upstream's own caveat -- the gate covers both, because it is in
            # front of the whole vhost.
            PAPERLESS_DISABLE_REGULAR_LOGIN = true;

            # Who django believes about `X-Forwarded-For`, which is what keeps
            # a failed-login log line attributable to a real client rather than
            # to nginx. The same resolved loopback the listener binds, so there
            # is no address literal in this file.
            PAPERLESS_TRUSTED_PROXIES =
              den.lib.capability.bindAddressFor host den.aspects.paperless "tcp"
                "http";

            # ── THE DATABASE, OVER THE SOCKET ──
            #
            # The four keys `database.createLocally` would have set, set here
            # instead because the server comes from ./postgres.nix. No
            # `PAPERLESS_DBPASS`, and that is the point of peer auth.
            #
            # `PrivateNetwork` on `paperless-scheduler` and
            # `paperless-consumer` is `cfg.database.createLocally` in the
            # nixpkgs module -- so leaving that false is also what leaves those
            # two units with a network namespace, which the mail-import rules
            # and the LLM index need.
            PAPERLESS_DBENGINE = "postgresql";
            PAPERLESS_DBHOST = "/run/postgresql";
            PAPERLESS_DBNAME = account;
            PAPERLESS_DBUSER = account;

            # ── OCR ──
            #
            # `eng` alone: the `package` option's `apply` rebuilds tesseract
            # with exactly the languages named here plus `equ`/`osd`/`eng`, and
            # each extra language costs both a rebuild and CPU on every page.
            PAPERLESS_OCR_LANGUAGE = "eng";

            # `skip` would leave a receipt photographed as a JPEG untouched.
            # `auto` skips pages that already carry enough embedded text --
            # which is right for a digital tax invoice arriving by mail and for
            # a photographed paper receipt in the same inbox.
            PAPERLESS_OCR_MODE = "auto";

            # Australian receipts are `23/10/2026`. Upstream's default is
            # already `DMY`; written because the whole value of this field for
            # a receipt is that the date paperless guesses is the date on the
            # paper, and `MDY` silently turns every day under 13 into the
            # wrong month.
            PAPERLESS_DATE_ORDER = "DMY";

            # ── NO LLM, AND NO REMOTE OCR ──
            #
            # Both default off; stated because the temptation to reach for them
            # is exactly what this service is being installed for. They do not
            # help: `PAPERLESS_AI_*` suggests document-level title, tags,
            # correspondent and type, and `PAPERLESS_REMOTE_OCR_ENGINE` has one
            # implementation (`azureai`) which returns text and a searchable
            # PDF. NEITHER RETURNS LINE ITEMS, so turning either on would send
            # every receipt to a third party and still leave the parsing
            # problem exactly where it was.
            #
            # `false` here also keeps `TIKTOKEN_CACHE_DIR` out of the units:
            # the module sets it on `cfg.settings.PAPERLESS_AI_ENABLED or true`,
            # and `or` fires on a MISSING attribute -- so NOT stating this is
            # what turns the cache dir on.
            PAPERLESS_AI_ENABLED = false;

            # ── THE CONSUMPTION DIRECTORY IS WATCHED BY inotify ──
            #
            # Zero means "use inotify and do not poll", which is the module's
            # default and is correct for a local filesystem. It would be WRONG
            # the day this directory becomes a network mount -- inotify does
            # not fire for a write made by another machine's kernel -- and
            # ../../docs/receipts-and-expenses.md's intake section is about
            # exactly that, so the number is stated where the reader of that
            # section will look for it.
            PAPERLESS_CONSUMER_POLLING_INTERVAL = 0;

            # Filed by date and correspondent rather than by surrogate id, so
            # the media tree a restic restore hands back is legible without the
            # database. `-none` suffixes are dropped rather than written out.
            PAPERLESS_FILENAME_FORMAT = "{created_year}/{correspondent}/{title}";
            PAPERLESS_FILENAME_FORMAT_REMOVE_NONE = true;
          };

          # NO `configureTika`. Tika and Gotenberg exist to turn office
          # documents and email bodies into PDFs; a receipt is an image or a
          # PDF already, and enabling them adds a JVM and a headless chromium
          # to this box for nothing.
        };
      };
  };
}
