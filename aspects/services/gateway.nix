{
  den,
  lib,
  ...
}:
# One place to say how a service is reached: which vhost and path, whether the
# browser leg goes through oauth2-proxy, and who may call its API under which
# key.
#
# The API-key half exists so callers are distinguishable. A service knows
# exactly one key -- `<svc>-internal`, which is also the key provisioned INTO
# the service -- while every caller gets a `<caller>-<svc>` key of its own.
# nginx rewrites the caller's key to the internal one before proxying, so the
# service is none the wiser, the access log records which caller it was, and a
# key can be rotated or revoked for one caller without touching the rest.
let
  # `vhosts` in ./nginx.nix is the model: an aspect writes the key next to the
  # service it belongs to, and it lands in the central option.
  mkForward =
    fromClass: path:
    {
      class,
      aspect-chain,
    }:
    den.batteries.forward {
      each = lib.singleton true;
      fromClass = _: fromClass;
      intoClass = _: "nixos";
      intoPath = _: path;
      fromAspect = _: lib.head aspect-chain;
      adaptArgs = lib.id;
    };

  # Lets a service aspect declare its own kanidm OAuth2 client, group or person
  # instead of them all piling up in one file.
  kanidmProvision = mkForward "provision" [
    "services"
    "kanidm"
    "provision"
  ];

  # The same provisioning, forwarded ONE LEVEL DEEPER, per entity kind.
  #
  # `kanidmProvision` above targets `provision`, so its class content is
  # `{ systems = ...; }` -- and den class content merges one level deep, so two
  # aspects both writing `systems` means the second REPLACES the first's whole
  # tree. That silently deleted three of four oauth2 clients, and the workaround
  # was for every service to write `services.kanidm.provision.systems.oauth2.<n>`
  # through the raw `nixos` class in six separate files.
  #
  # Forwarding at leaf depth fixes it at the source: class content becomes
  # `{ <entity name> = ...; }`, so the one-level merge collides only when two
  # aspects declare the SAME entity -- which is exactly when you want a
  # collision. `kanidmProvision` stays for the things that are declared once
  # (`provision.enable`, `idmAdminPasswordFile`).
  # Keyed by the DECLARING ASPECT's own name, so a service never writes its name
  # twice. `mkForward` already threads `aspect-chain`, which is what makes this
  # possible: the forward knows who is writing it.
  #
  # It lands on `gateway.oidcClients.<aspect>` -- an option with the shared OIDC
  # shape as DEFAULTS (below), not a path into kanidm's own options. So a client
  # is the three facts that differ (display name, callback path, secret) and the
  # rest is defaulted and typed, instead of every service restating twenty-five
  # lines and a function call.
  # The escape hatch, for an aspect whose client is NOT named after it
  # (`homeassistant` declares `home-assistant`, `sso` declares `oauth2-proxy`)
  # or which declares more than one. Same option, same defaults, name given.
  kanidmClients = mkForward "kanidmClients" [
    "gateway"
    "oidcClients"
  ];

  kanidmGroups = mkForward "kanidmGroups" [
    "services"
    "kanidm"
    "provision"
    "groups"
  ];

  kanidmPersons = mkForward "kanidmPersons" [
    "services"
    "kanidm"
    "provision"
    "persons"
  ];

  gatedServices = mkForward "gated" [
    "gateway"
    "services"
  ];

  # ── HOW A CALLER CONSUMES THE KEY IT PRESENTS ──
  #
  # The holder's own facts, folded from `takes.credential`
  # (../base/capability-types.nix's `credential.taker`). They are the
  # CALLER's and only the caller's: a key read out of the environment is held
  # for the life of the process, so the holder is the one end that knows which
  # unit has to be restarted when the file is rewritten and under which prefix
  # it reads it.
  #
  # Hoisted to file level for the reason ./kanidm.nix gives: a fold over
  # `den.aspects` and the host's own inclusion set needs no host fixpoint,
  # while the same expression inside a class body can only be forced by a full
  # host evaluation.
  callerFactsFor = host: den.lib.capability.contributions host "credential";
in
{
  den.aspects.gateway = {
    includes = [
      kanidmProvision
      kanidmClients
      kanidmGroups
      kanidmPersons
      gatedServices
      den.aspects.nginx
      den.aspects.agenix-rekey
    ];

    nixos =
      {
        config,
        lib,
        pkgs,
        host ? { },
        ...
      }:
      let
        inherit (lib) mkOption types;

        # The entry submodule shadows `config` with its own; this keeps a handle
        # on the host's, for the `enable` default below.
        outer = config;

        # Templates stay at agenix's own default path. Writing them into a
        # directory of our own would race systemd-tmpfiles on boot, and this
        # one is created before any unit starts.
        mapFile = name: "${config.age.templateDir}/gateway/${name}.map";

        # nginx's `include` errors on a missing literal path but is content
        # with a glob that matches nothing -- which is what the build-time
        # `nginx -t` sees, before agenix has ever run. Bracketing the first
        # character makes the pattern a glob while still matching exactly one
        # name.
        globFile =
          name: mapFile "[${lib.substring 0 1 name}]${lib.substring 1 (lib.stringLength name) name}";

        # nginx lowercases header names and turns '-' into '_' to build $http_*;
        # the same sanitising keeps generated variable names legal.
        nginxName = lib.replaceStrings [ "-" "." ] [ "_" "_" ];
        # Both the header AND the `?apikey=` query parameter, concatenated:
        # exactly one is populated in practice, so the joined value equals
        # whichever form the caller used and matches the table either way.
        # Homepage's *arr widgets call `{url}/api/v3/{endpoint}?apikey={key}`,
        # so a header-only map never sees their key and they fall through to the
        # SSO gate and get an HTML login page back.
        #
        # Both forms are rewritten on the way out: servarr reads `?apikey=`
        # FIRST and only then the header, so rewriting the header alone leaves
        # the app authenticating against the caller's key and rejecting it.
        headerVar = header: "$http_${nginxName (lib.toLower header)}$arg_apikey";

        # nginx wants a trailing slash on both sides of a prefix proxy, but
        # writing one in every aspect is noise -- so accept either spelling.
        slashed = p: if lib.hasSuffix "/" p then p else "${p}/";

        entries = lib.filter (e: e.enable) (
          lib.mapAttrsToList (
            name: svc:
            svc
            // {
              inherit name;
              subpath = slashed svc.subpath;
              api = svc.api // {
                subpath = slashed svc.api.subpath;
              };
            }
          ) config.gateway.services
        );
        gated = lib.filter (e: e.api.enable) entries;

        # ── WHO IS A PRINCIPAL, DERIVED FROM BOTH ENDS OF THE EDGE ──
        #
        # A gated service is one in its own right, and so is anything some
        # service NAMES as a client: the key a caller presents belongs to the
        # (caller, service) pair, and the callee is the end that says the pair
        # exists. So the account set is what `api.clients` adds up to, and a
        # caller nothing calls has none.
        #
        # Read off `gateway.services` rather than from
        # `den.lib.capability.contributions host "gated"`, which carries the same `api`
        # blocks: this way it covers every route into `api.clients` -- a
        # `takes.gated` translated by ./proxy-nginx.nix, a literal entry, or
        # ./service-accounts.nix's minted registry -- where the capability fold
        # sees only the first.
        gatedNames = map (e: e.name) gated;
        principals = lib.unique (gatedNames ++ lib.concatMap (e: e.api.clients) gated);

        callerFacts = callerFactsFor host;
        factsFor = key: callerFacts.${key} or { };

        # Aspects that said how they would read a key, which is only meaningful
        # if some service admits them -- see the assertion below.
        declaredCallers = lib.unique (
          lib.concatMap
            (key: lib.attrNames (lib.filterAttrs (_: v: v != null && v != [ ] && v != "") (factsFor key)))
            [
              "description"
              "envPrefix"
              "restartUnits"
            ]
        );

        # Exactly two keys are in play for a service: the one it ACCEPTS
        # (`<svc>-internal`, handed to the service itself) and the one it
        # PRESENTS when calling something else (`account-<svc>`, the credential
        # of its service account). A caller that is not a service of its own --
        # homepage, seerr, soularr -- has only the second.
        #
        # One credential per principal, not per (caller, service) pair: the same
        # account key appears in every map whose service admits that account, so
        # access is granted by map membership rather than by minting another
        # secret, and revoking one service's access never rotates a key the
        # account still needs elsewhere.
        # Scoped by WHOSE credential it is, not by what consumes it: a key lives
        # under the principal that holds it, so `sonarr/api-key` is sonarr's to
        # present and `sonarr/internal` is sonarr's to accept. Everything that
        # needs one -- the nginx maps, the env files -- takes it as an agenix
        # DEPENDENCY, which references the one secret rather than minting a copy
        # per consumer. So a key shared by several readers is one `.age` file.
        internalSecret = e: "${e.name}/internal";
        accountSecret = account: "${account}/api-key";

        accounts = lib.attrNames config.gateway.serviceAccounts;

        # Services this account may call, which is what its key has to open.
        callableBy = account: lib.filter (e: lib.elem account e.api.clients) gated;

        # An account's key is presented differently depending on the target, so
        # the attribution table needs one entry per distinct scheme it uses.
        prefixesFor = account: lib.unique (map (e: e.api.headerPrefix) (callableBy account));

        randomKey = "random-hex-32";

        nginxOwned = {
          owner = config.services.nginx.user;
          restartUnits = [ "nginx.service" ];
          symlink = false;
        };
      in
      {
        options.gateway.domain = mkOption {
          type = types.nullOr types.str;
          # SET BY THE ZONE'S OWN ASPECT, not here and not per host.
          # ../network/cloudflare.nix's `publish-ivymect-in` carries the zone and
          # writes this through its own class content, so including that aspect
          # both makes a host publish under the zone and tells the gateway which
          # zone it is. Null when a host includes no zone, which is the honest
          # value for a machine that publishes nothing.
          default = null;
          example = "ivymect.in";
          description = ''
            Zone the gated services are published under. Service aspects name
            only their subdomain, so the zone stays a host-level decision and
            an aspect can move between hosts unchanged.
          '';
        };

        options.gateway.humanGroups = mkOption {
          type = types.listOf types.str;
          default = [ ];
          example = [ "media-users" ];
          description = ''
            Groups whose people may reach every gated service through a browser.
            Folded into each service's `groups`, because a per-service machine
            group on its own would lock out the humans the service is for.
          '';
        };

        options.gateway.authCacheTtl = mkOption {
          type = types.nullOr types.str;
          default = "1m";
          example = "30s";
          description = ''
            How long an oauth2-proxy verdict is reused before asking again.
            Without this every request -- including each of the dozens a single
            page load makes -- costs a subrequest, and with
            `skip-jwt-bearer-tokens` that means a kanidm round trip per API call.

            The cost is revocation lag: a removed group or a revoked token stays
            usable for up to this long. Set to null to disable caching.
          '';
        };

        options.gateway.acmeHost = mkOption {
          type = types.nullOr types.str;
          default = outer.gateway.domain;
          defaultText = "gateway.domain";
          description = "`useACMEHost` for every vhost the gateway creates.";
        };

        options.gateway.serviceAccounts = mkOption {
          default = { };
          description = ''
            Machine principals that may call a gated API. DERIVED, not
            declared: an entry exists for every gated service and for every
            name some service lists in `api.clients`, because the key a caller
            presents belongs to the (caller, service) pair and the callee is
            the end that says the pair exists. A caller nothing calls has no
            account, and a caller a service names has one without being
            written down twice.

            How the holder READS its key -- `envPrefix`, `restartUnits`, and
            the display name -- is the caller's own fact and comes from its
            `takes.credential` (../base/capability-types.nix). Defining an
            entry here still works and is what ./service-accounts.nix's minted
            registry does, but it is no longer how a caller joins.

            These are NOT kanidm service accounts: kanidm-provision's schema
            covers persons, groups and oauth2 clients only, with no way to
            declare a service account or mint it an API token. So the account
            is modelled here and its credential is an agenix-generated key --
            same attribution and same per-caller rotation, resolved at nginx
            rather than at the IdP.
          '';
          type = types.attrsOf (
            types.submodule {
              options = {
                # ── WHERE A TOKEN LANDS, IF THIS ACCOUNT NEEDS ONE ──
                #
                # Null for the usual case: a gateway caller presents an
                # agenix-generated API key at nginx and never talks to kanidm
                # at all. Set it when something authenticates AS this account
                # against kanidm itself -- ../services/mail.nix's LDAP bind is
                # the first -- and the patched provisioner mints a read-only
                # token into that path.
                #
                # A PATH AND NOT A SECRET: the value cannot be read back from
                # kanidm, so it is not something this repo can hold or rekey.
                # The file is written on the host by the provisioning run, and
                # whatever reads it has to be able to, which is a question
                # about that reader's user and not about this option.
                apiTokenFile = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  example = "/var/lib/kanidm-provision/mail-ldap.token";
                  description = "Where to mint this account's kanidm API token, for something that binds AS it.";
                };

                description = mkOption {
                  type = types.str;
                  default = "";
                };
                restartUnits = mkOption {
                  type = types.listOf types.str;
                  default = [ ];
                  example = [ "homepage-dashboard.service" ];
                  description = ''
                    Units to restart when this account's key file changes. A
                    consumer that reads the key from its environment holds it for
                    the life of the process, so without this the file is rewritten
                    and the service keeps presenting the key it started with --
                    which looks exactly like the key being wrong.
                  '';
                };
                envPrefix = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  example = "HOMEPAGE_VAR_";
                  description = ''
                    When set, every key this account holds is rendered into one
                    env file as `<prefix><SERVICE>_KEY`. Consumers that read
                    credentials from the environment (and are read by systemd as
                    root, so a DynamicUser unit can still use them) point their
                    `environmentFile` at it.
                  '';
                };
              };
            }
          );
        };

        # ── APPLICATIONS, WHICH ARE WHAT A PERSON'S APP PASSWORD BELONGS TO ──
        #
        # Beside `serviceAccounts` and for the same reason: upstream's schema
        # has no entity for them, so they go in through `extraJsonFile` below,
        # and ../../patches/kanidm-provision/application-passwords.patch is
        # what reads them. DERIVED, like the accounts: ../services/kanidm.nix
        # writes one per consumer that said `takes.ldap.application`, so a
        # service's people can hold a credential for it without any file
        # naming the application twice.
        #
        # Why an application at all: kanidm's LDAP gateway serves
        # AUTHENTICATION binds and no password material, so a person logging
        # into a service through the directory binds with an application
        # password -- per person, per application, revocable on its own, and
        # never their primary credential.
        options.gateway.applications = mkOption {
          default = { };
          description = "kanidm applications, for the per-person per-application passwords their members hold.";
          type = types.attrsOf (
            types.submodule {
              options = {
                displayName = mkOption {
                  type = types.str;
                  description = "What kanidm's Apps tile calls this.";
                };

                # kanidm's own access rule: only members of the linked group
                # may hold a password for the application. So this is the group
                # whose people can log into that service at all, and there is
                # no way to provision the application without it.
                linkedGroup = mkOption {
                  type = types.str;
                  description = "Only members of this group may hold an application password for it.";
                };
              };
            }
          );
        };

        # ── A PERSON'S SSH PUBLIC KEYS, FOR kanidm TO SERVE ──
        #
        # Beside `applications` and `serviceAccounts` and in through the same
        # `extraJsonFile` channel, because upstream's `provision.persons.<n>`
        # carries `displayName`, `legalName` and `mailAddresses` and nothing
        # else -- ../../patches/kanidm-provision/ssh-keys.patch is what makes
        # the key set declarable.
        #
        # DERIVED, like the others: ../services/kanidm.nix fills this from
        # `den.persons.<n>.sshKeys`, which is where a human's keys are already
        # declared once for every account that trusts them
        # (../security/main-ssh-key.nix). So a key is written in one place and
        # reaches both the machines with local accounts AND, through kanidm,
        # the machines without.
        #
        # KEYED BY TAG. kanidm addresses an ssh key by tag and replaces a key
        # of the same tag, which is what makes provisioning them idempotent;
        # the tag is derived from the key itself rather than from its position,
        # so reordering the list renames nothing.
        options.gateway.personSshKeys = mkOption {
          default = { };
          description = "SSH public keys per kanidm person, keyed by tag. Derived, not declared here.";
          type = types.attrsOf (types.attrsOf types.str);
        };

        # The shared shape of a kanidm OAuth2 client, as DEFAULTS rather than as
        # twenty-five lines repeated per service. All seven clients here differ
        # in three facts -- a display name, a callback path and where the secret
        # is -- and agreed on everything else, which meant everything else could
        # drift between them.
        #
        # Written through the `kanidmClient` class, which keys this by the
        # declaring aspect's own name; `kanidmClients.<name>` for a client not
        # named after its aspect. Rendered to
        # `services.kanidm.provision.systems.oauth2` below.
        options.gateway.oidcClients = mkOption {
          default = { };
          description = "kanidm OAuth2 clients, declared by the service aspects that own them.";
          type = types.attrsOf (
            types.submodule (
              { name, config, ... }:
              {
                options = {
                  displayName = mkOption {
                    type = types.str;
                    description = "What kanidm's Apps tile calls this.";
                  };

                  callback = mkOption {
                    type = types.str;
                    example = "/auth/openid/kanidm";
                    description = ''
                      Path on this service's own vhost that kanidm redirects
                      back to. The one genuinely per-app fact: every app's OIDC
                      library picks its own.
                    '';
                  };

                  subdomain = mkOption {
                    type = types.nullOr types.str;
                    default = name;
                    description = ''
                      The vhost this service is published at, as
                      `<subdomain>.<domain>`. Defaults to the client name, which
                      is what the gateway does; override where the published
                      name differs (`cloud` for nextcloud, `home` for
                      home-assistant). `null` publishes at the bare domain.
                    '';
                  };

                  domain = mkOption {
                    type = types.str;
                    default = outer.gateway.domain;
                    defaultText = "gateway.domain";
                    description = ''
                      Taken from THIS host's gateway, so a client cannot carry a
                      stale literal and a service stays portable. The hardcoded
                      `sso.ivymect.in` and `homepage.ivymect.in` this replaced
                      are why it is a default and not a required argument.
                    '';
                  };

                  landingSubdomain = mkOption {
                    type = types.nullOr types.str;
                    default = null;
                    example = "homepage";
                    description = ''
                      Send kanidm's Apps tile to a DIFFERENT service's root --
                      for a client whose own vhost has nothing to serve at `/`.
                      Stated as a subdomain rather than a URL so the domain
                      stays derived and no literal can go stale.
                    '';
                  };

                  landing = mkOption {
                    type = types.str;
                    default =
                      if config.landingSubdomain != null then
                        "https://${config.landingSubdomain}.${config.domain}/"
                      else
                        "${config.origin}/";
                    defaultText = "the service's own root, or `landingSubdomain`'s";
                    description = "Where kanidm's Apps tile sends you.";
                  };

                  secretScope = mkOption {
                    type = types.str;
                    default = name;
                    description = "The `age.scoped` scope holding this client's secret. Defaults to the client name.";
                  };

                  secretName = mkOption {
                    type = types.str;
                    default = "oidc-client-secret";
                    description = "The secret's name within that scope.";
                  };

                  secretFile = mkOption {
                    type = types.nullOr types.str;
                    default =
                      let
                        key = "${config.secretScope}/${config.secretName}";
                      in
                      outer.age.secrets.${key}.path or null;
                    defaultText = "the path of age secret `\${secretScope}/\${secretName}`, or null if there is none";
                    description = ''
                      The client secret, read by kanidm as itself. Derived from
                      `secretScope`/`secretName`, so a service that follows the
                      convention says nothing about it at all and a `public`
                      client -- which declares no such secret -- gets `null`
                      without having to say so.

                      Derived rather than passed in because a `scoped` argument
                      is not available to a forwarded class body targeting this
                      option: it falls through to `_module.args.scoped` and
                      fails. `config.age.secrets` carries the same path and is
                      readable here.
                    '';
                  };

                  groupsName = mkOption {
                    type = types.bool;
                    default = false;
                    description = ''
                      Put the user's group names in the token. Needed only by an
                      app that maps groups to its own roles -- oauth2-proxy's
                      `allowed_groups`, home-assistant's `auth_oidc.roles`,
                      nextcloud's user_oidc. An app doing its own authorisation
                      does not need them: the scope map is itself the gate,
                      since kanidm refuses a token to anyone outside the group.

                      `groups_name`, not `groups`: kanidm offers `groups` (uuid
                      AND spn), `groups_spn` and `groups_name`, and only the
                      last yields bare names like `media-users`. The SPN forms
                      arrive as `media-users@auth.ivymect.in` and never match an
                      app's role map.
                    '';
                  };

                  public = mkOption {
                    type = types.bool;
                    default = false;
                    description = "A client that cannot keep a secret -- a browser or mobile app doing PKCE.";
                  };

                  scope = mkOption {
                    type = types.str;
                    default = "media-users";
                    description = ''
                      The group kanidm will issue a token to at all -- the
                      first gate. The blanket one; `scopes` below is how a
                      client narrows it to the people actually granted.
                    '';
                  };

                  # ── PER-SERVICE AUTHORISATION, WHICH `scope` ALONE CANNOT DO ──
                  #
                  # Every client in this tree shared `media-users`, so "the
                  # scope map is itself the gate" -- which four of these
                  # clients' own comments say -- was one gate for all of them:
                  # in `media-users` meant a token for vikunja, donetick,
                  # tandoor and splitpro alike, and a grant on one of them
                  # bought nothing.
                  #
                  # A LIST, because a grant has levels: `app-tandoor-user` and
                  # `app-tandoor-admin` are two groups and either should get a
                  # token. ../services/kanidm.nix renders one `scopeMaps`
                  # entry per name.
                  #
                  # The DEFAULT is the old behaviour, which is what makes this
                  # a narrowing a client opts into rather than a flag day:
                  # a client that states nothing still scopes `media-users`.
                  scopes = mkOption {
                    type = types.listOf types.str;
                    default = [ config.scope ];
                    defaultText = "[ scope ]";
                    example = [
                      "app-tandoor-user"
                      "app-tandoor-admin"
                    ];
                    description = ''
                      Every group kanidm will issue this client a token to.
                      Stating it REPLACES `scope`, which is the point: a
                      client scoped on its own grant groups is reachable only
                      by the people granted it.

                      Each name must be a group that EXISTS -- kanidm's
                      provisioning run fails on a scope map naming one that
                      does not -- which is why `accessLib.groupsFor` answers
                      with the granted levels rather than the declared ones.
                    '';
                  };

                  origin = mkOption {
                    type = types.str;
                    readOnly = true;
                    default = "https://${
                      if config.subdomain == null then config.domain else "${config.subdomain}.${config.domain}"
                    }";
                    defaultText = "https://\${subdomain}.\${domain}";
                  };
                };
              }
            )
          );
        };

        options.gateway.services = mkOption {
          default = { };
          description = "Services published through the nginx gateway.";
          type = types.attrsOf (
            types.submodule (
              {
                name,
                config,
                ...
              }:
              {
                options = {
                  enable = mkOption {
                    type = types.bool;
                    default = outer.services.${name}.enable or true;
                    defaultText = "the service's own `enable`, or true when there is no such service";
                    description = ''
                      Turning a service off takes its vhost, its locations and
                      its keys with it, rather than leaving a 502 behind.
                    '';
                  };
                  domain = mkOption {
                    type = types.str;
                    default = "${name}.${outer.gateway.domain}";
                    defaultText = "\${name}.\${gateway.domain}";
                    example = "arr.ivymect.in";
                    description = ''
                      Virtual host this service is served under. Left alone it
                      is the service's own name under `gateway.domain`, which is
                      what keeps an aspect portable between hosts; name a whole
                      domain to share one vhost between several services.
                    '';
                  };
                  subpath = mkOption {
                    type = types.str;
                    default = "/";
                    example = "/prowlarr";
                    description = ''
                      Location prefix on that vhost. A trailing slash is added
                      if absent.
                    '';
                  };
                  url = mkOption {
                    type = types.str;
                    readOnly = true;
                    default = "https://${config.domain}${lib.removeSuffix "/" config.subpath}";
                    defaultText = "https://\${domain}\${subpath}";
                    description = ''
                      Where a caller reaches this service. Callers must use this
                      rather than the upstream: the per-caller key is only
                      swapped for the service's own key on the way through
                      nginx, so a loopback shortcut would present a key the
                      service has never heard of.
                    '';
                  };
                  localUrl = mkOption {
                    type = types.str;
                    readOnly = true;
                    default = "${config.upstream}${lib.removeSuffix "/" config.subpath}";
                    defaultText = "\${upstream}\${subpath}";
                    description = ''
                      Where this service is reachable on the host itself,
                      bypassing nginx. For the rare call that cannot carry a key
                      -- an app validating a callback URL, say -- where going
                      through the gate would fail for want of a credential.
                      Prefer `url`: this leg is unauthenticated and unattributed.
                    '';
                  };
                  upstream = mkOption {
                    type = types.str;
                    example = "http://127.0.0.1:8989";
                    description = "Scheme, host and port of the backend.";
                  };
                  banner = mkOption {
                    type = types.nullOr types.str;
                    default = null;
                    example = "Read-only: managed by Nix";
                    description = ''
                      Text injected into the app's own UI, for a service whose
                      settings are declared in Nix and whose in-app editors
                      therefore do nothing (or worse, appear to work until the
                      next activation overwrites them).
                    '';
                  };
                  # Named for the mechanism rather than the goal: a service
                  # with `oauthProxy = false` is still behind single sign-on
                  # if it is its own OIDC client (vikunja, donetick), which is
                  # exactly why the old name `sso` was wrong.
                  oauthProxy = mkOption {
                    type = types.bool;
                    default = true;
                    description = "Put the browser leg behind oauth2-proxy.";
                  };
                  forwardUser = mkOption {
                    type = types.bool;
                    default = false;
                    description = ''
                      Forward the authenticated person's short username to the
                      upstream as `Remote-User`, for a service whose own
                      authentication is "trust the proxy". See the note on the
                      location below for which claim it comes from and why.
                    '';
                  };
                  extraGroups = mkOption {
                    type = types.listOf types.str;
                    default = [ ];
                    example = [ "app-vikunja-admin" ];
                    description = ''
                      Groups to ADD to the default `groups`, for an aspect that
                      wants to widen access without restating what the default
                      already grants.

                      This exists because a module default is discarded the
                      moment anything defines the option, so ./access.nix --
                      which adds one `app-<svc>-<level>` group per grant -- had
                      to restate `humanGroups ++ [ "svc-<name>" ]` verbatim and
                      was thereby coupled to this file's default. Contributing
                      `extraGroups` instead keeps the default in one place.

                      Define `groups` directly to REPLACE the default rather
                      than add to it, which is what narrowing access needs.
                    '';
                  };
                  groups = mkOption {
                    type = types.listOf types.str;
                    default = outer.gateway.humanGroups ++ [ "svc-${name}" ] ++ config.extraGroups;
                    defaultText = "gateway.humanGroups ++ [ \"svc-\u2039name\u203a\" ] ++ extraGroups";
                    description = ''
                      kanidm groups allowed to reach this service. oauth2-proxy
                      checks them for a browser session and, with
                      `skip-jwt-bearer-tokens`, for a machine caller's bearer
                      token too -- so authorisation is one rule enforced in one
                      place for both kinds of caller.
                    '';
                  };
                  # ── THE SERVICE'S OWN BODY LIMIT ──
                  #
                  # Here rather than as a literal in each vhost, because it is
                  # the same fact the public carrier states as `maxBodyBytes`
                  # and ../network/cloudflare.nix's `excluded` reconciles the
                  # two: a vhost accepting more than the tunnel passes is taken
                  # OFF the tunnel rather than failing mid-upload.
                  #
                  # ../services/nextcloud.nix's `10G` and
                  # ../base/celler/server.nix's `15g` are both this, written out
                  # in `extraConfig` today.
                  maxBodyBytes = mkOption {
                    type = types.nullOr types.int;
                    default = null;
                    example = 10 * 1000 * 1000 * 1000;
                    description = ''
                      Largest request body this service accepts, in bytes.
                      Rendered as `client_max_body_size`. Null leaves nginx's
                      own default, which is 1MB.
                    '';
                  };

                  websockets = mkOption {
                    type = types.bool;
                    default = false;
                  };
                  api = {
                    enable = mkOption {
                      type = types.bool;
                      default = false;
                      description = ''
                        Serve this service's API on its own location, gated by an
                        API key instead of a browser session.
                      '';
                    };
                    subpath = mkOption {
                      type = types.str;
                      # removeSuffix, not plain concatenation: `subpath` is "/"
                      # for a service at the root of its own vhost, and "/" +
                      # "/api" is `//api` -- a location nginx will never match,
                      # so every API call falls through to the UI location and
                      # gets sent to oauth2-proxy instead of the key map.
                      default = "${lib.removeSuffix "/" config.subpath}/api";
                      defaultText = "\${subpath}/api";
                    };
                    header = mkOption {
                      type = types.str;
                      default = "X-Api-Key";
                      example = "Authorization";
                      description = "Request header the key is read from.";
                    };
                    headerPrefix = mkOption {
                      type = types.str;
                      default = "";
                      example = "Bearer ";
                      description = ''
                        Scheme prefix the header value carries, for services
                        that take their key as `Authorization: Bearer <key>`
                        rather than a bare header (qBittorrent 5.1+).
                      '';
                    };
                    sessionFallback = mkOption {
                      type = types.bool;
                      default = !config.api.internalKey;
                      defaultText = "!internalKey";
                      description = ''
                        Let a browser SESSION satisfy the API location as well as
                        a key, via an `auth_request` subrequest.

                        Off by default for a service that holds its own key,
                        because a subrequest does NOT inherit the query string --
                        only headers -- so `?apikey=` can only ever be matched in
                        the main request. Such a service embeds its key in its own
                        web UI, so the key check covers the browser too and
                        nothing is lost. Needed only where the UI drives its API
                        with a cookie and no key, as qbittorrent's does.
                      '';
                    };
                    internalKey = mkOption {
                      type = types.bool;
                      default = config.api.internalEnv != null;
                      defaultText = "internalEnv != null";
                      description = ''
                        Whether the service holds a key of its own for the
                        gateway to swap in. When false the caller's key is
                        validated and then STRIPPED, and the service is left
                        trusting loopback -- which is how a service whose key
                        can only be set from a world-readable store file avoids
                        having one there.
                      '';
                    };
                    clients = mkOption {
                      type = types.listOf types.str;
                      default = [ ];
                      example = [ "prowlarr" ];
                      description = ''
                        Callers that get their own key for this service. Each
                        caller has ONE key of its own -- `<caller>/api-key`, see
                        `accountSecret` above -- which nginx rewrites to this
                        service's `<name>/internal` on the way through. So a
                        request is attributable to its caller, and revoking one
                        caller does not rotate anyone else's.

                        NOT a key per (caller, service) pair: an earlier
                        description here said `<caller>-<service>`, which no
                        code has ever minted.
                      '';
                    };
                    internalEnv = mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      example = "SONARR__AUTH__APIKEY";
                      description = ''
                        Environment variable that hands the internal key to the
                        service itself. When set, a one-line env file is rendered
                        for the service's `environmentFiles`.
                      '';
                    };
                    owner = mkOption {
                      type = types.str;
                      default = name;
                      description = "Unix user that reads the generated env file.";
                    };
                  };
                };
              }
            )
          );
        };

        config = lib.mkIf (entries != [ ]) {
          # DERIVED FROM `principals`, so neither end of an edge is declared
          # twice. ../hosts/secondpc/media.nix held `seerr` and `homepage` by
          # hand for exactly the reason its comment gave -- "callers of the
          # gated APIs that are not gated services themselves, so nothing
          # registers them automatically" -- which is one file naming the
          # caller, another naming the callee, and nothing checking the two
          # agreed.
          #
          # `mkDefault` on the two string-valued leaves, because they are not
          # mergeable: ./service-accounts.nix's minted registry and any one-off
          # override are definitions of the same leaf, and a derived string
          # that could not be overridden would be a conflict rather than a
          # default. `restartUnits` is a list and merges, so a caller may add
          # to it from anywhere.
          gateway.serviceAccounts = lib.genAttrs principals (account: {
            description = lib.mkDefault (
              let
                d = (factsFor "description").${account} or "";
              in
              if d != "" then
                d
              else if lib.elem account gatedNames then
                "${account} service"
              else
                ""
            );
            restartUnits = (factsFor "restartUnits").${account} or [ ];
            envPrefix = lib.mkDefault ((factsFor "envPrefix").${account} or null);
          });

          # ── THE CHECK NOW RUNS THE OTHER WAY ──
          #
          # "every named client is a declared service account" went
          # tautological the moment the account set was derived FROM the named
          # clients. What can still disagree is the far end: an aspect stating
          # how it would read a gateway key that no service admits. That used
          # to be silent -- the facts were simply never read -- and it is the
          # same drift this derivation closes, seen from the caller's side.
          #
          # WHAT THAT COSTS: a misspelled name in `api.clients` used to fail
          # this assertion and now mints an account of that name instead. It is
          # not checkable from here, because the three legitimate kinds of
          # caller are indistinguishable by name -- a gated service, an aspect
          # with a `takes.credential`, and a key minted into
          # ./service-accounts.json, which is deliberately NOT an aspect. A
          # check against the aspect graph would reject exactly the third.
          assertions = map (name: {
            assertion = lib.elem name principals;
            message = ''
              den.aspects.${name} declares `takes.credential` facts for a gateway
              key, but no gated service on this host names it in
              `api.clients` -- so it has no service account and nothing reads
              them. Name it as a client of whatever it calls, or drop the take.
            '';
          }) declaredCallers;

          # `age.scoped.<principal>` rather than raw `age.secrets`: the scope
          # supplies the `<principal>/` prefix, and for a scope named after a
          # real service it also infers that service's owner, group and
          # restartUnits -- so sonarr restarts when the key it accepts changes.
          age.scoped = lib.mkMerge (
            map (account: {
              ${account} = {
                # `service = null` disables inference, and it has to be off here:
                # inferring would read `services.<name>` while that service's own
                # `environmentFiles` is being computed from a template that
                # depends on this very secret. That is the loop lib/age-scoped.nix
                # documents, and this is the escape hatch it prescribes --
                # explicit `settings` instead of a derived owner and restart.
                service = null;
                secrets.api-key.generator.script = randomKey;
              };
            }) accounts
            ++ map (e: {
              ${e.name} = {
                service = null;
                # Named literally rather than looked up, so nothing is forced.
                settings.restartUnits = [ "${e.name}.service" ];
                secrets.internal.generator.script = randomKey;
              };
            }) (lib.filter (e: e.api.internalKey) gated)
          );

          age.templates =
            # Per-service rewrite table: caller key -> internal key. A key absent
            # from a service's table is a 401 there, so a key minted for one
            # service is useless against another.
            lib.listToAttrs (
              map (
                e:
                lib.nameValuePair "gateway/${e.name}.map" (
                  nginxOwned
                  // {
                    dependencies =
                      lib.listToAttrs (
                        map (client: lib.nameValuePair client config.age.secrets.${accountSecret client}) e.api.clients
                      )
                      // lib.optionalAttrs e.api.internalKey {
                        internal = config.age.secrets.${internalSecret e};
                      };
                    content =
                      { placeholders, ... }:
                      let
                        target = if e.api.internalKey then placeholders.internal else "1";
                      in
                      lib.concatMapStrings (client: ''
                        "${e.api.headerPrefix}${placeholders.${client}}" "${target}";
                      '') e.api.clients
                      # The service's own key maps to itself, so its web UI --
                      # which embeds that key in the page -- keeps working.
                      + lib.optionalString e.api.internalKey ''
                        "${e.api.headerPrefix}${placeholders.internal}" "${target}";
                      '';
                  }
                )
              ) gated
            )
            # One global caller table, so a single log_format can name whoever
            # made the call regardless of which service it was aimed at.
            // lib.optionalAttrs (accounts != [ ]) {
              "gateway/callers.map" = nginxOwned // {
                dependencies = lib.listToAttrs (
                  map (a: lib.nameValuePair a config.age.secrets.${accountSecret a}) accounts
                );
                content =
                  { placeholders, ... }:
                  lib.concatMapStrings (
                    account:
                    lib.concatMapStrings (prefix: ''
                      "${prefix}${placeholders.${account}}" "${account}";
                    '') (prefixesFor account)
                  ) accounts;
              };
            }
            # One env file per account that asked for one, holding every key
            # that account holds.
            // lib.listToAttrs (
              map (
                account:
                lib.nameValuePair "gateway/account-${account}.env" {
                  inherit (config.gateway.serviceAccounts.${account}) restartUnits;
                  dependencies.key = config.age.secrets.${accountSecret account};
                  content = { placeholders, ... }: ''
                    ${config.gateway.serviceAccounts.${account}.envPrefix}KEY=${placeholders.key}
                  '';
                }
              ) (lib.attrNames (lib.filterAttrs (_: a: a.envPrefix != null) config.gateway.serviceAccounts))
            )
            # The service's own copy of its internal key, as an env file.
            // lib.listToAttrs (
              map (
                e:
                lib.nameValuePair "gateway/${e.name}.env" {
                  owner = e.api.owner;
                  restartUnits = [ "${e.name}.service" ];
                  dependencies.internal = config.age.secrets.${internalSecret e};
                  content = { placeholders, ... }: ''
                    ${e.api.internalEnv}=${placeholders.internal}
                  '';
                }
              ) (lib.filter (e: e.api.internalEnv != null) gated)
            );

          # Map bodies are globbed in rather than written inline: the key values
          # are secrets, and a glob matching nothing keeps the build-time
          # `nginx -t` happy before agenix has ever run.
          # nginx's own option rather than a hand-written proxy_cache_path: it
          # emits the directive in the right place and the directory comes from
          # the unit's `CacheDirectory`, which a hand-rolled path does not get.
          # Through the module's options rather than raw directives: `lines`
          # concatenates every definition, so a raw `map_hash_bucket_size` is a
          # duplicate the moment anything else emits one, whereas an int option
          # merges. Every key in the tables below is 64 hex characters, and a
          # `Bearer` service's carry a 7-character prefix on top -- past nginx's
          # 64-byte default bucket, reported at startup as "could not build
          # map_hash". Powers of two only.
          services.nginx.mapHashBucketSize = lib.mkDefault 256;
          services.nginx.mapHashMaxSize = lib.mkDefault 4096;

          services.nginx.proxyCachePath.gateway-auth = lib.mkIf (config.gateway.authCacheTtl != null) {
            enable = true;
            keysZoneName = "gateway_auth";
            keysZoneSize = "4m";
            maxSize = "32m";
            inactive = "10m";
          };

          # `commonHttpConfig`, NOT the append variant: that one is emitted after
          # the server blocks, and everything defined here -- the log format, the
          # key maps, the cache zone -- is referenced from inside them. nginx
          # parses in order, so defining them later is simply "unknown log format
          # gw_api" / "unknown variable $gw_key_sonarr" at startup.
          services.nginx.commonHttpConfig = lib.mkMerge [
            (lib.mkIf (gated != [ ]) ''
              log_format gw_api '$remote_addr $gw_caller "$request" $status $body_bytes_sent $request_time';

              map "$http_x_api_key$http_authorization$arg_apikey" $gw_caller {
                  default "-";
                  include ${globFile "callers"};
              }

                ${lib.concatMapStrings (e: ''
                  map ${headerVar e.api.header} $gw_key_${nginxName e.name} {
                      default "";
                      include ${globFile e.name};
                  }
                '') gated}
            '')
          ];

          # Fold the auth cache onto the subrequest location the oauth2-proxy
          # module writes. `extraConfig` is `types.lines`, so this appends to
          # what that module already put there rather than replacing it.
          services.nginx.virtualHosts = lib.mkMerge [
            (lib.mapAttrs (_: es: {
              useACMEHost = config.gateway.acmeHost;
              forceSSL = true;

              # THE LARGEST LIMIT ANY SERVICE ON THIS NAME ASKED FOR. Several
              # entries can share one vhost (the *arr apps under `arr.<zone>`),
              # and `client_max_body_size` is per server block -- so the maximum
              # is the only answer that does not break the service that needs
              # the most.
              extraConfig =
                let
                  lims = lib.filter (x: x != null) (map (e: e.maxBodyBytes) es);
                in
                lib.optionalString (lims != [ ]) ''
                  client_max_body_size ${toString (lib.foldl' lib.max 0 lims)};
                '';
              locations = lib.listToAttrs (
                lib.concatMap (
                  e:
                  [
                    (lib.nameValuePair e.subpath {
                      proxyPass = "${e.upstream}${e.subpath}";
                      proxyWebsockets = e.websockets;
                      # sub_filter cannot match through gzip, so the upstream has
                      # to be asked for plaintext before anything can be injected.
                      # The banner text is XML-escaped, not interpolated raw: an
                      # apostrophe in it ('qBittorrent's settings...') closes
                      # nginx's single-quoted string early, which nginx reports
                      # as an "unexpected" token on the following word. Escaping
                      # also keeps it from injecting markup into the page.
                      #
                      # `sub_filter_types` is deliberately absent: text/html is
                      # already in its default, and naming it again is a
                      # "duplicate MIME type" warning at startup.
                      # ── WHO THE PERSON IS, WHERE THE SERVICE ASKED FOR IT ──
                      #
                      # `X-Auth-Request-Preferred-Username` and NOT
                      # `-User`. oauth2-proxy's `--set-xauthrequest` emits four
                      # headers, each from a named claim
                      # (`getXAuthRequestHeaders` in its `legacy_options.go`):
                      # `user`, `email`, `preferred_username` and `groups`. The
                      # `user` claim is the subject or the email depending on
                      # provider, where `preferred_username` is kanidm's SHORT
                      # name -- `auscyber`, which is `kanidm.name` and so the
                      # string samba, mail and NSS already use for that human.
                      # A service keyed on the other one would create a second
                      # account for the same person.
                      #
                      # `proxy_set_header` OVERWRITES, which is the security
                      # property this depends on: a client that sends its own
                      # `Remote-User` has it replaced, and if the variable is
                      # empty nginx drops the header entirely rather than
                      # forwarding a blank one -- so a request that did not
                      # authenticate arrives with no identity at all and a
                      # service trusting this header fails closed.
                      #
                      # The nixpkgs oauth2-proxy integration already sets
                      # `$user`/`$email` on this vhost's `/` location; this is a
                      # third `auth_request_set` beside them, and `extraConfig`
                      # is `types.lines`, so the two merge.
                      extraConfig =
                        lib.optionalString e.forwardUser ''
                          auth_request_set $gw_person $upstream_http_x_auth_request_preferred_username;
                          proxy_set_header Remote-User $gw_person;
                        ''
                        + lib.optionalString (e.banner != null) ''
                          proxy_set_header Accept-Encoding "";
                          sub_filter_once on;
                          sub_filter '</body>' '<div style="position:fixed;bottom:0;left:0;right:0;z-index:99999;padding:6px 12px;font:600 13px/1.4 system-ui,sans-serif;text-align:center;color:#1b1b1b;background:#f5c451;box-shadow:0 -1px 4px rgba(0,0,0,.3)">${lib.escapeXML e.banner}</div></body>';
                        '';
                    })
                  ]
                  ++ lib.optional (e.api.enable && e.api.sessionFallback) (
                    lib.nameValuePair "= /gw-auth-${nginxName e.name}" {
                      # Either credential will do: a valid key short-circuits to
                      # 204, anything else falls through to oauth2-proxy. Without
                      # this, an app whose own web UI drives its API from the
                      # browser -- qbittorrent's does, for everything -- 401s on
                      # every request, because a browser carries a session and
                      # not a key.
                      #
                      # `if` here is the safe form: it holds only a `return`, and
                      # proxy_pass sits outside it.
                      proxyPass = "${config.services.oauth2-proxy.nginx.proxy}/oauth2/auth${
                        lib.optionalString (e.groups != [ ])
                          "?allowed_groups=${lib.concatMapStringsSep "," lib.escapeURL e.groups}"
                      }";
                      extraConfig = ''
                        internal;
                        auth_request off;
                        # Named explicitly: the default span name is just
                        # this location's own path ("/gw-auth-${e.name}"),
                        # which reads as the check having happened but not
                        # who it actually asked -- oauth2-proxy, via the
                        # proxy_pass two lines up.
                        otel_span_name "gw-auth ${e.name} -> oauth2-proxy";
                        if ($gw_key_${nginxName e.name} != "") { return 204; }
                        proxy_set_header X-Scheme       $scheme;
                        proxy_set_header Content-Length "";
                        proxy_pass_request_body         off;
                      '';
                    }
                  )
                  ++ lib.optional e.api.enable (
                    lib.nameValuePair e.api.subpath {
                      proxyPass = "${e.upstream}${e.api.subpath}";
                      # A longer prefix than the UI location, so it wins.
                      extraConfig = ''
                        # An API location must not inherit the vhost's login
                        # redirect: a caller presenting a bad or absent key then
                        # gets a 302 chain ending in kanidm's HTML login page,
                        # which it dutifully parses as the API response. That is
                        # unreadable as a failure -- say 401 and mean it.
                        error_page 401 =401 @gatewayApiUnauthorized;

                        ${
                          if e.api.sessionFallback then
                            "auth_request /gw-auth-${nginxName e.name};"
                          else
                            ''
                              auth_request off;
                              if ($gw_key_${nginxName e.name} = "") { return 401; }
                            ''
                        }
                        # ── `Remote-User` IS CLEARED HERE, UNCONDITIONALLY ──
                        #
                        # The UI location sets it from the authenticated claim
                        # and leans on `proxy_set_header` OVERWRITING a
                        # client-supplied one -- its own comment calls that "the
                        # security property this depends on". This location did
                        # not set it at all, so nginx forwarded whatever the
                        # caller sent: a service trusting `Remote-User` would
                        # authenticate a request to `${e.api.subpath}` as any
                        # user the caller cared to name, with the key check
                        # passed and nothing in the logs to show it.
                        #
                        # Nothing is currently misconfigured -- no entry sets
                        # both `forwardUser` and `api.enable` -- but the two are
                        # independently settable, so this was one option away.
                        #
                        # Cleared rather than asserted against, and cleared for
                        # every API location rather than only the `forwardUser`
                        # ones, because an API caller proves itself with a KEY:
                        # a forwarded browser identity has no meaning on this
                        # path even when the pair is deliberate. An empty value
                        # makes nginx omit the header entirely, so the upstream
                        # sees no identity at all and a service trusting it
                        # fails closed.
                        proxy_set_header Remote-User "";
                        proxy_set_header ${e.api.header} ${
                          if e.api.internalKey then "\"${e.api.headerPrefix}$gw_key_${nginxName e.name}\"" else "\"\""
                        };
                        ${lib.optionalString e.api.internalKey ''
                          # The query parameter must be rewritten as well as the
                          # header: servarr reads `?apikey=` FIRST and only then
                          # falls back to X-Api-Key, so leaving the caller's key
                          # in the query means the app authenticates against THAT
                          # and rejects it. `$args` is writable and proxy_pass
                          # picks the change up, and the capture keeps every
                          # other parameter intact.
                          if ($args ~ "^(.*)apikey=[^&]*(.*)$") {
                              set $args "$1apikey=$gw_key_${nginxName e.name}$2";
                          }
                        ''}
                        access_log /var/log/nginx/api.log gw_api;
                      '';
                    }
                  )
                ) es
              );
            }) (lib.groupBy (e: e.domain) entries))
            (lib.mkIf (config.gateway.authCacheTtl != null) (
              lib.genAttrs (lib.unique (map (e: e.domain) (lib.filter (e: e.oauthProxy) entries))) (_: {
                locations."= /oauth2/auth".extraConfig = ''
                  proxy_cache gateway_auth;
                  proxy_cache_key "$host$http_authorization$http_cookie";
                  proxy_cache_valid 200 204 ${config.gateway.authCacheTtl};
                  # Denials are cached far more briefly: a newly granted group
                  # should take effect quickly, a revoked one is bounded above.
                  proxy_cache_valid 401 403 5s;
                  proxy_cache_use_stale error timeout;
                '';
              })
            ))

            # A 403 from oauth2-proxy means AUTHENTICATED but not authorised -- a
            # valid session whose token lacks the group. nginx only wires 401 to
            # the login redirect, so without this a stale or under-privileged
            # session is a permanent blank 403 with nothing explaining it.
            # Deliberately not a redirect: sending 403 back to /start loops,
            # because the login succeeds and the group check fails again.
            (lib.genAttrs (lib.unique (map (e: e.domain) (lib.filter (e: e.oauthProxy) entries))) (_: {
              # `=403` forces the final status, so the handler below can return
              # 200. A handler that returns 403 itself re-enters this same
              # error_page and nginx aborts the request with "rewrite or internal
              # redirection cycle" -- i.e. a 500 on every gated service.
              extraConfig = ''
                error_page 403 =403 @gatewayForbidden;

                # oauth2-proxy's reply to the auth subrequest carries the session
                # cookie -- the whole ID token -- plus the X-Auth-Request-*
                # headers. With a groups claim that overruns nginx's 4k/8k
                # default, which nginx reports as "upstream sent too big header",
                # fails the subrequest with 502, and the client sees a 500.
                proxy_buffer_size 16k;
                proxy_buffers 8 16k;
                proxy_busy_buffers_size 32k;
              '';
              locations."@gatewayApiUnauthorized".extraConfig = ''
                auth_request off;
                default_type application/json;
                return 401 '{"error":"unauthorized","detail":"missing or unrecognised API key"}';
              '';
              locations."@gatewayForbidden".extraConfig = ''
                auth_request off;
                default_type text/html;
                return 200 '<!doctype html><meta charset=utf-8><title>Not authorised</title><body style="font:16px/1.5 system-ui,sans-serif;max-width:34em;margin:4em auto;padding:0 1em"><h1>Not authorised</h1><p>You are signed in, but your account is not in a group permitted to reach this service.<p><a href="https://${config.services.oauth2-proxy.nginx.domain}/oauth2/sign_out?rd=https%3A%2F%2F${config.services.oauth2-proxy.nginx.domain}%2Fsigned-out">Sign out</a> and try another account.</body>';
              '';
            }))
          ];

          # Anything on this host that calls a gated service has to use the
          # public name so nginx can swap its key -- homepage's widgets are the
          # obvious case. Without this they leave the box and come back through
          # hairpin NAT, which most routers do not do. The wildcard certificate
          # still matches, so TLS verifies.
          networking.hosts."127.0.0.1" = lib.unique (map (e: e.domain) entries);

          # A vhost can carry several services, so it admits the union of their
          # groups; the per-service API locations narrow it again by key.
          services.oauth2-proxy.nginx.virtualHosts = lib.mapAttrs (_: es: {
            allowed_groups = lib.unique (lib.concatMap (e: e.groups) es);
          }) (lib.groupBy (e: e.domain) (lib.filter (e: e.oauthProxy) entries));

          # The service accounts themselves. They go in through `extraJsonFile`
          # rather than a typed option because upstream's schema has no entity
          # for them -- that is what patches/kanidm-provision adds. A side
          # effect worth knowing: setting `extraJsonFile` also switches off the
          # module's entity assertions, which is the only reason the group
          # memberships below are accepted at all (the module cannot see that a
          # service account named `prowlarr` exists).
          #
          # `apiTokens` NOW HAS A CONSUMER, which is what this note said it was
          # waiting for: ../services/mail.nix binds to kanidm's LDAP gateway as
          # a service account, and a bind needs a credential. Still minted only
          # where one is ASKED for (`apiTokenFile`), so an account nothing
          # authenticates as keeps having no token sitting on disk.
          #
          # The provisioner writes the value to that path at 0600 on the host
          # and never again -- kanidm returns a token once, at creation -- so
          # the file is the only copy and `readWrite = false` keeps it to
          # reads, which is all a directory bind does.
          services.kanidm.provision.extraJsonFile = pkgs.writeText "gateway-service-accounts.json" (
            builtins.toJSON {
              # `camelCase` in the JSON, which is what the patched provisioner
              # deserialises (`#[serde(rename_all = "camelCase")]`).
              applications = lib.mapAttrs (_: a: {
                inherit (a) displayName linkedGroup;
              }) config.gateway.applications;

              # Deep-merged into the persons the typed options already generate
              # -- the module composes the two with `yq '. *+ load(...)'` -- so
              # this adds keys to a person rather than redefining them.
              persons = lib.mapAttrs (_: keys: { sshPublicKeys = keys; }) config.gateway.personSshKeys;

              serviceAccounts = lib.listToAttrs (
                map (
                  account:
                  lib.nameValuePair account (
                    {
                      displayName =
                        let
                          d = config.gateway.serviceAccounts.${account}.description;
                        in
                        if d == "" then account else d;
                      entryManagedBy = "idm_admins";
                    }
                    // lib.optionalAttrs (config.gateway.serviceAccounts.${account}.apiTokenFile != null) {
                      apiTokens.default = {
                        file = config.gateway.serviceAccounts.${account}.apiTokenFile;
                        readWrite = false;
                      };
                    }
                  )
                ) accounts
              );
            }
          );

          # One kanidm group per service, holding the machine callers that may
          # reach it. Declared here rather than per-aspect so a service cannot
          # name clients without the group that authorises them existing.
          services.kanidm.provision.groups = lib.listToAttrs (
            map (
              e:
              lib.nameValuePair "svc-${e.name}" {
                members = e.api.clients;
              }
            ) entries
          );
        };
      };
  };
}
