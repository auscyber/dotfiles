{
  accessLib,
  den,
  lib,
  ...
}:
# grocy (household ERP: stock, shopping lists, recipes and meal plans,
# batteries, equipment). It goes in BESIDE ./vikunja.nix and ./donetick.nix
# rather than replacing either -- the overlap is resolved with grocy's own
# feature flags below, not by retiring anything. See ../../docs/grocy.md for the
# feature-by-feature reasoning.
#
# ── THREE THINGS HERE ARE UNLIKE EVERY OTHER SERVICE IN THIS TREE ──
#
#   * NO DATABASE. grocy is sqlite-only upstream (nixpkgs patches
#     `services/DatabaseService.php` to `return getenv('GROCY_DB_FILE')`), so
#     there is no `takes.postgres` and the db goes off the box through
#     ./restic.nix's `sqlite` rather than a dump.
#   * IT HAS NO LOGIN OF ITS OWN. grocy has no OIDC at all --
#     `middleware/Auth/` is ApiKey, Default, Ldap, ReverseProxy, Session and
#     nothing else -- so the gate is the login: `oauthProxy = true` puts
#     oauth2-proxy in front and `forwardUser = true` hands grocy the
#     authenticated person's short username, which is the whole of what
#     `ReverseProxyAuthMiddleware` needs.
#
#     THIS WAS LDAP FIRST, binding against kanidm's own gateway with a
#     per-person application password. That worked and cost more: a bind
#     service account and its token, a readable copy of that token placed by a
#     derived unit, a kanidm APPLICATION plus the group allowed to hold
#     passwords for it, a CA bundle for the `ldaps://` leg, and a manual step
#     per person (kanidm only ever mints an application password; nobody can
#     declare one). Every one of those is gone. The gate was already in front
#     of this vhost for publication and the group check, so the identity was
#     already there to be used -- LDAP was a second authentication path beside
#     one that had already run.
#   * TWO VHOSTS. The nixpkgs module renders grocy's server block itself (root,
#     the php location, the cache headers) and php-fpm has no TCP listener for
#     the gate to forward to. So the module's vhost is bound to LOOPBACK on
#     `port` below, `gives.tcp` advertises it, and the gate proxies to it like
#     any other upstream.
let
  # ── THE PORT, ONCE ──
  #
  # Read by two things that must agree: `gives.tcp` below, which is what
  # ./proxy-nginx.nix resolves the upstream from and what makes the public https
  # address derivable, and the internal server block's `listen`.
  #
  # NOT EXPOSED, written out rather than defaulted: a plain-http server block
  # carrying session cookies is one the firewall (../base/firewall.nix, which
  # derives itself from `gives`) has to leave shut on every wire.
  port = 9192;

  # The module's own default, repeated here because the backup entry reads it.
  dataDir = "/var/lib/grocy";

  # The name of the module's server block. Not a public name and not resolvable
  # anywhere: it is the only block listening on `127.0.0.1:${port}`, so it is
  # that socket's default server whatever `Host` the gate forwards.
  internalHost = "grocy-upstream";

  # NO `settingoverrides` DIRECTORY AND NO `GROCY_DATAPATH`. Both existed to
  # carry ONE secret -- the LDAP bind token -- into `Setting()` through the only
  # one of its three sources that reads a file. With the gate supplying the
  # identity there is no secret in this service at all: nothing to generate,
  # nothing to rekey, nothing to place and nothing to rotate.
  # ── ONE ACCESS LEVEL, AND THAT IS HONESTY RATHER THAN LAZINESS ──
  #
  # Every user grocy creates is an ADMIN (`DEFAULT_PERMISSIONS` is `['ADMIN']`,
  # an array, and arrays cannot come from the environment), and grocy has no
  # group concept for a kanidm group to map to. An `admin` level would open the
  # same gate and change nothing inside the app.
  accessLevel = "user";
in
{
  den.aspects.grocy = {
    includes = [
      den.aspects.gateway
      # ./restic.nix's `backup` quirk -- see the `backup` entry below.
      den.aspects.restic
    ];

    gives =
      { self, ... }:
      {
        # WHAT A GRANT HERE MEANS. One level, for the reason the `application`
        # note above gives: inside grocy everyone is already ADMIN, so a second
        # level would be a kanidm group that opens the same door.
        #
        # It IS what authorises the login now, which it was not under LDAP: the
        # gate admits the union of a vhost's groups, and ./access.nix turns a
        # grant at this level into one of them. So a person with no grant does
        # not reach grocy at all, where before the gate let them through and
        # the application's linked group was the real check.
        service = _: {
          access = {
            levels = [ accessLevel ];
            defaultLevel = accessLevel;
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
          icon = "grocy.svg";
          description = "Shopping and stock";
        };

        # WHAT IT LISTENS ON, so ./proxy-nginx.nix can ASK rather than assume.
        # The listener is nginx's own internal server block, not php-fpm's --
        # php-fpm is on a unix socket and the gate cannot forward to one.
        tcp = _: {
          name = self.name;
          ports.http = {
            port = port;
            meta.description = "nginx's own internal server block for grocy -- php-fpm is on a unix socket the gate cannot forward to, so this is what it dials instead.";
            expose = [ ];
          };
        };

        # THE ACCOUNT THE php-fpm POOL RUNS AS, furnished rather than restated.
        # `nginx` as the group is the nixpkgs module's choice and is load-bearing
        # -- it is how nginx reads the pool socket and the override files.
        user = _: {
          name = "grocy";
          group = "nginx";
          carrier = self.name;
        };
      };

    takes.gated = {
      # ── THE GATE IS THE LOGIN ──
      #
      # `true`, unlike ./vikunja.nix and ./donetick.nix: those decline the
      # forward-auth proxy because they are real OIDC clients and would bounce
      # one login through two providers. grocy cannot be an OIDC client -- it
      # has no OIDC code at all -- so there is no second provider to bounce
      # through, and oauth2-proxy is the only thing that can establish who is
      # asking.
      oauthProxy = true;

      # ── AND THE IDENTITY IT ESTABLISHED, WHICH IS THE WHOLE MECHANISM ──
      #
      # `ReverseProxyAuthMiddleware` reads one header, looks a user up by it
      # and creates them if absent (`UsersService::CreateUser`). So this take
      # is grocy's entire authentication: the gate supplies the name, the name
      # is kanidm's short username, and the account appears on first visit.
      #
      # It FAILS CLOSED, which is the property that makes trusting a header
      # acceptable here. The middleware throws when the variable is missing or
      # empty, and the gate drops the header rather than forwarding a blank one
      # -- so an unauthenticated request cannot arrive as a nameless user, and
      # a misconfigured gate is a 500 rather than a silent anonymous login.
      forwardUser = true;
    };

    # ── WHAT A GRANT ON GROCY DOES, DECLARED WHERE THE GATE IS ──
    #
    # The gate IS grocy's login (see `takes.gated` above), so the realisation
    # of a grant here is one group added to the vhost's allowed set. ./access.nix
    # used to write this for every service in the tree from one line; it is
    # grocy's own mechanism, so it is declared next to the take that creates it.
    #
    # `gated.<n>` is the forward onto `gateway.services.<n>` (./gateway.nix),
    # and `extraGroups` ADDS to that option's default rather than replacing it
    # -- a grant widens who may in, it does not restate `humanGroups ++
    # [ "svc-grocy" ]`.
    gated.grocy.extraGroups = accessLib.groupsFor "grocy";

    # Stock, recipes, barcodes and price history, all of it in one sqlite file
    # that nothing else can rebuild.
    #
    # Through `sqlite` and not `paths`: php holds the database open, so a
    # file-level copy is a copy of a torn page cache. ./restic.nix runs
    # `VACUUM INTO` and snapshots that instead, and the live files are excluded
    # so a restore cannot pick the wrong one.
    #
    # `storage/` is in `paths` because it is the other half of the state:
    # product pictures and recipe images are files on disk with only a row
    # pointing at them, exactly like vikunja's attachments.
    backup = _: {
      name = "grocy";
      sqlite = [ "${dataDir}/grocy.db" ];
      paths = [ "${dataDir}/storage" ];
    };

    # ── NO `secrets` BLOCK, AND NOTHING TO REKEY ──
    #
    # grocy has no OIDC client (no client secret), no database password and now
    # no bind credential either: the gate authenticates the person and grocy is
    # told who they are. The LDAP version needed a kanidm service-account token,
    # which could not be an agenix secret at all -- kanidm returns one exactly
    # once, at creation -- and so needed a derived unit to place a readable copy.
    #
    # So this aspect holds no credential of any kind, which also means it is
    # deployable while the master key is mid-recovery (../../CLAUDE.md): no
    # `rekey`, no `gen-secrets`, nothing to mint by hand.

    nixos =
      {
        config,
        lib,
        host ? { },
        ...
      }:
      {
        services.grocy = {
          enable = true;

          # THE INTERNAL NAME. The public one is the gate's and is derived from
          # `gives.tcp` above; this is only what the module calls its own server
          # block.
          hostName = internalHost;

          # No ACME and no forceSSL on the internal block: it is loopback-only
          # plain http and the certificate for the public name is the gateway's.
          nginx.enableSSL = false;

          inherit dataDir;

          settings = {
            currency = "AUD";
            # INERT in grocy 4.x -- `GROCY_CULTURE` is read by nothing in the
            # source (`LocaleMiddleware` and the validator both use
            # `DEFAULT_LOCALE`, set in `extraConfig` below). Written anyway so
            # the module's option does not say something different from the one
            # that works.
            culture = "en_GB";
            calendar.firstDayOfWeek = 1;
            entryPage = "stock";
          };

          # ── EVERYTHING THE MODULE HAS NO OPTION FOR ──
          #
          # Appended verbatim to `/etc/grocy/config.php`, which is loaded BEFORE
          # `config-dist.php`, so each `Setting()` here wins over upstream's
          # default. No `mkForce` and no second definition of that file: this is
          # the module's own escape hatch.
          #
          # Nothing secret is in here, and now nothing secret exists: the gate
          # authenticates the person, so there is no bind password to keep out
          # of a world-readable `/etc/grocy/config.php`.
          extraConfig = ''
            // The live locale key. `CULTURE` above is dead in 4.x.
            Setting('DEFAULT_LOCALE', 'en_GB');

            // Australian labels are kilojoules. Display only -- grocy converts
            // nothing.
            Setting('ENERGY_UNIT', 'kJ');

            // ── AUTHENTICATION: THE PROXY SAID WHO THIS IS ──
            //
            // Validated at startup (`ConfigurationValidator::CheckAuthClass`
            // does a `class_exists`), so a typo is a config error page rather
            // than a fatal on every request. Note the namespace: 4.x moved the
            // middleware under `Grocy\Middleware\Auth\`.
            //
            // The middleware reads one name, finds that user or creates them,
            // and defines `GROCY_EXTERNALLY_MANAGED_AUTHENTICATION` -- which is
            // what takes grocy's own login form and user management out of the
            // UI. There is nothing here for a person to type, which is the
            // point: the gate already asked.
            Setting('AUTH_CLASS', 'Grocy\Middleware\Auth\ReverseProxyAuthMiddleware');

            // ── FROM `$_SERVER`, NOT FROM A REQUEST HEADER ──
            //
            // `REVERSE_PROXY_AUTH_USE_ENV` switches the middleware from
            // `$request->getHeader(...)` to `$_SERVER[...]`, and that choice is
            // the security of this whole arrangement. php maps an inbound HTTP
            // header into `$_SERVER` as `HTTP_<NAME>`, so a client sending
            // `Remote-User:` lands at `HTTP_REMOTE_USER` and can NEVER appear
            // as `REMOTE_USER` -- that key is writable only by the server, via
            // the `fastcgi_param` below. In header mode the same request would
            // be indistinguishable from the gate's own.
            //
            // ── A NAME NOTHING ELSE CAN DEFINE ──
            //
            // NOT `REMOTE_USER`, which is upstream's default and the
            // conventional CGI slot. The module's php location ends with
            // `include .../fastcgi.conf` and `include .../fastcgi_params`,
            // which land AFTER our own `fastcgi_param` -- and nginx sends
            // duplicate parameters rather than letting the first win, so if
            // either include ever set `REMOTE_USER` the value php read would be
            // theirs. Neither does today; nginx's distributed `fastcgi_params`
            // sets `REMOTE_ADDR` and `REMOTE_PORT` and leaves `REMOTE_USER` to
            // configurations using `auth_basic`.
            //
            // That is a fact about a file in another package, checked once and
            // then relied on forever by something whose failure mode is "any
            // visitor is admitted as somebody else". A name of our own cannot
            // be redefined by an include we do not control, so the question
            // stops being one.
            Setting('REVERSE_PROXY_AUTH_HEADER', 'GROCY_REMOTE_USER');
            Setting('REVERSE_PROXY_AUTH_USE_ENV', true);

            // API routes still accept an API key first (the middleware tries
            // `ApiKeyAuthMiddleware` before looking at the identity at all), so
            // the gate's own key rewrite for `/api` keeps working and a machine
            // caller needs no person.

            // ── THE OVERLAP, TURNED OFF BY FLAG ──
            //
            // Chores go to ./donetick.nix and tasks to ./vikunja.nix, both of
            // which are real OIDC clients -- so an assignment there names a
            // kanidm person, where a grocy assignment would name a row in
            // grocy's own sqlite that nothing else in this house knows about.
            Setting('FEATURE_FLAG_CHORES', false);
            Setting('FEATURE_FLAG_CHORES_ASSIGNMENTS', false);
            Setting('FEATURE_FLAG_TASKS', false);
          '';
        };

        # ── THE INTERNAL SERVER BLOCK, BOUND TO LOOPBACK ──
        #
        # The module defines this virtualHost's root, locations and headers; the
        # one thing it has no option for is where it listens. Defining `listen`
        # replaces nginx's default 80/443 pair for this block and nothing else,
        # which is what makes the gate's resolved upstream reachable and leaves
        # the port off every wire.
        # ONE DEFINITION OF THIS VHOST, not two. `${internalHost}` is a
        # DYNAMIC attribute name, and two `services.nginx.virtualHosts.${x}.…`
        # paths in one attrset literal are "dynamic attribute already defined"
        # -- Nix cannot see that they would have merged. So `listen` and the
        # php location below are siblings under a single key.
        services.nginx.virtualHosts.${internalHost} = {
          # ── THE ADDRESS IS RESOLVED, NOT WRITTEN ──
          #
          # `gives.tcp.ports.http.expose` is `[ ]` -- nothing off this box
          # reaches grocy, the gate is the way in -- and that one declaration is
          # now what decides all three of: which wires ../base/firewall.nix
          # opens (none), which interface it would open them on, and what this
          # block binds. `127.0.0.1` was a literal here, agreed by hand with an
          # `expose` that meant the same thing a few lines above.
          #
          # The loopback carrier furnishes the address
          # (../network/segments.nix), so even that string is not in this file.
          listen = [
            {
              addr = den.lib.capability.bindAddressFor host den.aspects.grocy "tcp" "http";
              inherit port;
            }
          ];

          # ── WHERE THE IDENTITY CROSSES INTO php ──
          #
          # The gate sets `Remote-User` on the proxied request (its
          # `forwardUser` take, implemented in ./gateway.nix). This turns that
          # header into the `GROCY_REMOTE_USER` server variable, which is the
          # key the middleware reads and the one a client cannot forge: php
          # prefixes inbound headers with `HTTP_`, so a request carrying
          # `Grocy-Remote-User:` lands at `HTTP_GROCY_REMOTE_USER` and can never
          # appear here. Only the server writes this key. See the setting's own
          # note for why the name is ours rather than `REMOTE_USER`.
          #
          # `$http_remote_user` is EMPTY when the gate forwarded no identity,
          # and an empty `REMOTE_USER` is what makes the middleware throw
          # rather than authenticate somebody -- so the failure mode of a
          # broken gate is a 500, not an anonymous session.
          #
          # Written into the module's own php location, whose `extraConfig` is
          # `types.lines` and therefore merges.
          locations."~ \\.php$".extraConfig = ''
            fastcgi_param GROCY_REMOTE_USER $http_remote_user;
          '';
        };

        # NO `phpEnv` ADDITIONS. `GROCY_DATAPATH` was here to make the
        # `settingoverrides` directory live, which existed only to carry the
        # LDAP bind password, and `LDAPTLS_CACERT` was here because php's
        # openldap has no CA file by default and the `ldaps://` bind would not
        # verify. Neither has anything left to do.
      };
  };
}
