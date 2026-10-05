{
  den,
  lib,
  capability,
  caps,
  ...
}:
# One postgres, many services, and no service file that has to know what the
# others asked for.
#
# A service aspect declares its database on the `postgres` QUIRK and includes
# this aspect; the collection below turns every declaration into
#
#   * `ensureDatabases` / `ensureUsers`             -- the database and its role
#   * the union of `extensions` and preloaded libs  -- server-global, so unioned
#   * ordering on each consumer's own systemd units -- see `services`
#   * a role password for the consumers that need one -- see `passwordFile`
#
# The alternative -- every service poking `services.postgresql.ensureDatabases`
# directly -- works, but those options are LISTS, so each service has to know
# what the others already put there (./nextcloud.nix's comment used to spell
# that coupling out against ./homeassistant/default.nix). It also breaks
# outright for `shared_preload_libraries`, which is one comma-joined string.
#
# Quirk, not a plain option, because this is structured data flowing UP from
# several aspects to one consumer, which is exactly what a quirk is for (see
# ../base/celler/server.nix for the other instance of this shape in-tree).
#
# Connections are PEER-authenticated over the unix socket: a service running as
# its own unix user reaches its own database with no password anywhere. A
# container cannot use that -- peer auth checks the CONNECTING process's uid,
# which is the image's -- so a containerised consumer declares `tcp = true` and
# brings its own password secret. ./splitpro.nix is the one that does.
let
  inherit (den.lib.policy) pipe;

  # Quirk emissions arrive as a LIST of raw values -- there is no submodule to
  # carry defaults -- so normalise here, once, and let a missing key be a
  # missing key rather than a thrown attribute deep in a map.
  # ── WHO ASKED, AND FOR WHAT, FOLDED AT FILE LEVEL ──
  #
  # `capability.contributions` collects every taker's `takes.postgres`: one
  # entry per taker for `services`, and one value for the three `takerMerged`
  # keys below -- `extensions` and `preload` concatenate and dedupe through
  # their own type, and `settings` is server-global, so two takers defining one
  # key is a module-system conflict rather than a merge.
  #
  # The database and role are NAMED AFTER THE ASKER'S ACCOUNT, resolved through
  # its `user` capability rather than taken from its aspect name, because peer
  # authentication checks the connecting unix user against the role. That is why
  # `homeassistant` gets `hass` and why `grafana.nix` no longer needs `dbName`.
  #
  # A function of the aspect graph and the host entity, so it needs no host
  # fixpoint; the class body only consumes it.
  takerDbsFor =
    host:
    let
      c = capability.contributions host "postgres";
      askers = capability.takersAt host "postgres";
    in
    map (
      {
        name,
        aspect,
        entity,
      }:
      let
        account = capability.linkFor aspect "user" {
          host = entity;
          peer = entity;
        };
        nm = if account == null then name else account.name;
      in
      {
        database = nm;
        owner = nm;
        services = c.services.${name} or [ ];
        tcp = c.tcp.${name} or false;
        # ── WHERE THIS TAKER ARRIVES FROM, RESOLVED ──
        # Dialled from the taker's own entity: a container's `127.0.0.1` is
        # itself, so it reaches this server over whatever wire joins the two,
        # and that wire's address is where to LISTEN and its prefix what to
        # ADMIT. A taker on this host itself stays on the loopback and socket.
        via =
          let
            l = capability.dial "tcp" { name = "postgres"; } entity;
          in
          if entity.name == host.name || l == null then
            null
          else
            {
              inherit (l) address;
              inherit (l.carrier) prefix;
            };
        # Unioned across every taker, so each db record carries the same set and
        # the `lib.unique` below is idempotent.
        inherit (c) extensions preload settings;
        # ── THE ROLE PASSWORD IS THE EDGE'S CREDENTIAL ──
        #
        # A SIDE, not a path: `pathIn` needs the scope's fixpoint and this is
        # computed outside it, so the body applies `scoped` and gets the path.
        # That keeps the expensive dependency in the one line that cannot avoid
        # it -- which is the whole "smallest fixpoint that answers the question"
        # rule.
        #
        # `giver`, because this is the SERVER's copy: the role-password unit
        # runs as postgres and reads its own materialisation, while the asker
        # reads the plain-named sibling in its own scope.
        # A REALISATION CAN COME BACK NULL -- if no capability in the closure
        # declares one -- so this reads the result rather than assuming a set.
        # Reading `.giver` off null is `expected a set but found null`, pointing
        # at `systemd.services` rather than at the edge that produced it.
        passwordSide =
          let
            r =
              if !(c.tcp.${name} or false) then
                null
              else
                capability.realise "postgres" {
                  giver = den.aspects.postgres;
                  taker = aspect;
                  inherit host;
                  peer = aspect;
                  # ONE MACHINE, POSSIBLY TWO ENTITIES. `postgres` has `reach =
                  # "host"`, so an asker gets its own box's server; a container
                  # taker is still its own end of the edge.
                  giverHost = host;
                  takerHost = entity;
                };
          in
          if r == null then null else r.giver;

        # THE WHOLE REALISATION, for the implement step below. `credential`'s
        # `realise` produces the two scope ENTRIES -- the taker's copy under the
        # plain name its own templates read, and the giver's sibling inheriting
        # its `rekeyFile` -- and something has to install them. Reading
        # `passwordSide.pathIn scoped` without installing them is
        # `attribute 'db-password-postgres' missing`, which is a path to a
        # secret nothing declared.
        credential =
          if !(c.tcp.${name} or false) then
            null
          else
            capability.realise "postgres" {
              giver = den.aspects.postgres;
              taker = aspect;
              inherit host;
              peer = aspect;
              giverHost = host;
              takerHost = entity;
            };
        passwordFile = null;
      }
    ) askers;

  # ── THE BRIDGE, READ OFF THE CARRIER THAT OWNS IT ──
  #
  # Not literals: ../network/container-bridge.nix declares the gateway and the
  # subnet because they are its deployment facts, and a host whose daemon is
  # configured differently says so there. This file only needs to know where
  # a containerised consumer arrives from.
  norm = d: {
    inherit (d) database;
    owner = d.owner or d.database;
    extensions = d.extensions or [ ];
    preload = d.preload or [ ];
    settings = d.settings or { };
    tcp = d.tcp or false;
    # The carrier a taker arrives over, resolved, or null for the host's own
    # loopback and socket.
    via = d.via or null;
    passwordFile = d.passwordFile or null;
    # The consumer's own systemd units, ordered behind the database existing.
    services = d.services or [ ];
  };

  # A conflicting setting is a throw, not a merge. These are server-global
  # (`cron.database_name` names exactly ONE database), so two services quietly
  # disagreeing is the failure this whole aspect exists to prevent.
  mergeSettings = lib.foldl' (
    acc: d:
    lib.foldlAttrs (
      acc': k: v:
      if acc' ? ${k} && acc'.${k} != v then
        throw "postgres quirk: setting '${k}' is claimed twice with different values ('${toString acc'.${k}}' and '${toString v}'); it is server-global, so only one service may set it"
      else
        acc' // { ${k} = v; }
    ) acc d.settings
  ) { };
in
{
  den.quirks.postgres.description = "databases services need provisioned, with the extensions, preloads and unit ordering they require";

  den.policies.postgres-databases = _: pipe.from "postgres" [ (pipe.collectAll (_: true)) ];

  # ── HOISTED: WHO ASKED, AND FOR WHAT ──
  #
  # A function of `den.aspects` plus the host's inclusion set, so it is
  # answerable without instantiating a host. The class body below only consumes
  # it. ../../docs/capabilities.md has the measurement behind that rule.
  #
  # The database and role are NAMED AFTER THE ASKER'S ACCOUNT -- resolved
  # through its `user` capability, not its aspect name, because peer
  # authentication checks the connecting unix user against the role. That is why
  # `homeassistant` gets `hass`.
  den.aspects.postgres = {
    includes = [ den.policies.postgres-databases ];

    # ── WHAT THIS SERVICE GIVES AND TAKES, ON THE ASPECT ──
    #
    # Not in a parallel `den.serviceTypes.postgres` entry: "the serviceTypes
    # also should contain the aspect, it is like an abstraction over an aspect"
    # -- and once it is typed on the aspect
    # (../base/capability-types.nix's `den.schema.aspect`) the abstraction has
    # nothing left to hold.
    #
    # WHAT A TAKER GETS, stated here rather than pushed by each consumer: a
    # database and a role NAMED AFTER THE ASKER, over the socket. The provider
    # knows who is asking -- that is what the edge carries -- so a consumer
    # writes `takes.postgres = { }` and reads `postgres.database` /
    # `postgres.user` back, exactly as an attachment reads an address it never
    # announced.
    #
    # This is the half the quirk cannot do. Today each consumer types its
    # database name TWICE -- once for the quirk that creates it, once for the
    # app that connects -- and nothing checks the two agree; ./donetick.nix and
    # ./vikunja.nix both do it. Derived from the asker they cannot disagree.
    gives =
      { self, ... }:
      {
        # A SERVICE THE HOUSE OFFERS -- to its own machine only. `reach =
        # "host"` below is what says so, and it is the fact the household file
        # used to restate: auspc using secondpc's database is exactly the
        # failure that option exists to prevent. No `access`: a database role
        # is not a kanidm group.
        service = _: { };

        # ── A DIALABLE ENDPOINT ──
        #
        # The server had none: `gives.postgres` carries the database, the role
        # and how it is reached, but no port -- so a consumer that needed an
        # ADDRESS had nowhere to resolve one and ../services/splitpro.nix
        # hardcoded `127.0.0.1:5432`, which only worked because its container
        # ran with the host's network. Given here, the address comes from
        # whichever carrier the consumer can use: the loopback for a service
        # on the box, the bridge for one in a namespace.
        #
        # NOT EXPOSED: the firewall is derived from this
        # (../base/firewall.nix), and nothing off this machine dials the
        # database -- `reach = "host"` below says the same thing to
        # resolution.
        tcp = _: {
          name = self.name;
          ports.postgres = {
            port = 5432;
            meta.description = "The PostgreSQL wire protocol, dialed by this box's own services -- over the loopback for a unit, over the bridge for one in a namespace.";
            expose = [ ];
          };
        };

        # The account the server itself runs as, so a credential of ITS own can
        # resolve an owner for the server's side.
        user = _: {
          name = "postgres";
          group = "postgres";
          carrier = self.name;
        };

        postgres =
          { peer, resolve, ... }:
          let
            # NAMED AFTER THE ASKER'S ACCOUNT, NOT ITS ASPECT. `homeassistant`
            # asks and gets `hass`, because the account is what peer
            # authentication checks: the role has to equal the connecting unix
            # user or the socket rejects it. Deriving from the aspect name
            # instead would have quietly provisioned a database called
            # `homeassistant` that nothing connects to -- and `grafana.nix` had
            # the same fact written twice as `dbName`.
            account = resolve "user" peer;

            # DOES THIS ASKER NEED A PASSWORD? Only a TCP taker does. A
            # containerised consumer cannot use peer auth -- the socket
            # authenticates the connecting process's uid and the container's
            # uid is the image's -- so it asks for tcp, and that is the only
            # case where a credential has to exist.
            #
            # Read off the asker's own take, so nothing is minted for the five
            # consumers that use the socket and need no secret at all.
            overTcp = (peer.takes.postgres.tcp or false);
          in
          {
            database = account.name;
            user = account.name;
            # The characteristic a consumer FILTERS ON: `takes.postgres = {
            # over = "unix"; }` keeps only socket-reachable providers. Furnished
            # by the giver because how it is reached is the server's fact, not
            # the asker's.
            over = if overTcp then "tcp" else "unix";
            # THE SERVER ITSELF, not a pool in front of it. ../services/pgbouncer.nix
            # gives the same capability with this true, so a consumer that
            # cannot survive transaction pooling filters on it.
            pooled = false;
            carrier = self.name;
          }
          # ── THE ROLE PASSWORD IS AN EDGE CREDENTIAL ──
          #
          # So `postgres` is a narrowing of `credential` for the takers that
          # need one, and `credential`'s own `realise` produces the two owned
          # materialisations -- the server's copy owned by `postgres`, the
          # asker's by its own account. ./donetick.nix and ./splitpro.nix each
          # wrote `passwordFile = scoped.<self>.secrets.db-password.path` by
          # hand, which is a path the server also had to know.
          // lib.optionalAttrs overTcp {
            # THE TAKER'S OWN SCOPE, because the taker is the one with
            # templates and a template may not reach across scopes -- see the
            # `scope` option in ../base/capability-types.nix. An aspect's scope
            # defaults to its own name; `secretScope` overrides it.
            scope = if (peer.secretScope or null) != null then peer.secretScope else peer.name;
            secretName = "db-password";
          };
      };

    # THE SERVER LISTENS ON A SOCKET, and peer authentication over it is what
    # makes the common case credential-free: a service running as its own unix
    # user reaches its own database with no password anywhere.
    #
    # `unix` NARROWS NO ADDRESS (../base/capability-types.nix), so this take can
    # only ever resolve to this host's own loopback. That is what makes "auspc
    # must use its own postgres, never secondpc's" unrepresentable rather than
    # guarded -- and why `reach` below is belt and braces rather than the only
    # thing standing between a host and someone else's database.
    takes.unix = { };

    # Where it RUNS is also as far as it is visible. A database is not borrowed
    # across machines.
    reach = "host";

    nixos =
      {
        config,
        pkgs,
        lib,
        postgres,
        scoped,
        host ? { },
        ...
      }:
      let
        # ── THE EDGE SIDE: PROVISION FROM `takers` ──
        #
        # This is the half that makes the service model load-bearing rather than
        # a second description of the same system. A consumer that declares
        # `uses.postgres = { }` -- naming NOTHING -- shows up here as a taker,
        # and the database and role are named after it, because the provider
        # knows who is asking. That is the "ask, do not announce" relation
        # ./postgres.nix's own `gives.postgres` states.
        #
        # What it removes: every current consumer writes its database name TWICE
        # -- once for the quirk that creates it, once for the app that connects
        # -- with nothing checking the two agree. ../services/donetick.nix and
        # ../services/vikunja.nix both do it. Derived from the taker's identity
        # they cannot disagree, and a renamed service cannot strand a role.
        #
        # ── PROVISIONED FROM THE TAKERS, through the capability graph ──
        #
        # Was a walk over `config.den.services` for a spawned `postgres`
        # instance on this host, then over its `takers` -- two derived fields and
        # a cascade, to answer a question that is "which aspects on this host
        # declare `takes.postgres`".
        #
        # `capability.contributions` folds every taker's contribution by the
        # discipline ../base/capability-types.nix declares for each key: one
        # entry per taker for `services`, `lib.unique` for `extensions` and
        # `preload`, and a THROW for `settings`, which is server-global and so
        # cannot be a merge.
        #
        # `takerDbs` is hoisted to file level: it is a function of the aspect
        # graph and the host entity, so it needs no host fixpoint.
        # APPLYING `scoped` IS ALL THE BODY ADDS. Everything else about who asked
        # and what they asked for was decided at file level.
        takerDbs = map (
          d:
          removeAttrs d [ "passwordSide" ]
          // {
            passwordFile = if d.passwordSide == null then null else d.passwordSide.pathIn scoped;
          }
        ) (takerDbsFor host);
        dbs = (map norm (lib.flatten postgres)) ++ (map norm takerDbs);

        # ── THE SCOPE IS CREATED AT THE IMPLEMENT PLACE ──
        #
        # One `.age` source per TCP edge, materialised twice: the taker's copy
        # under the plain name (owned by its own account, read by its own
        # templates) and this server's sibling for the role-password unit.
        # Installed into the TAKER's scope, because a template may not depend on
        # another scope's secret -- see `scope` in ../base/capability-types.nix.
        #
        # The entries themselves are not written here: they come from the
        # capability definition, which is where a credential's implementation
        # details belong.
        credentialScopes = lib.listToAttrs (
          map (d: lib.nameValuePair d.credential.scope { inherit (d.credential) secrets; }) (
            lib.filter (d: d.credential != null) (takerDbsFor host)
          )
        );
        preload = lib.unique (lib.concatMap (d: d.preload) dbs);
        extNames = lib.unique (lib.concatMap (d: d.extensions) dbs);
        pwDbs = lib.filter (d: d.passwordFile != null) dbs;
        viaDbs = lib.filter (d: d.via != null) dbs;

        # `postgresql-setup` is a separate oneshot from `postgresql` itself and
        # is the unit that actually runs ensureDatabases/ensureUsers -- so a
        # service ordered against `postgresql.target` alone reaches a LIVE
        # SERVER THAT HAS NO DATABASE yet, and fails at startup in whatever way
        # that particular service fails. ./homeassistant/default.nix and
        # ./nextcloud.nix each worked this out and wrote the same lines by hand;
        # naming the units on the quirk means a new consumer cannot forget.
        #
        # A TCP consumer additionally waits on its role password: the role
        # exists after setup but cannot authenticate until it has one.
        consumerOrdering = lib.concatMap (
          d:
          map (unit: {
            ${unit} = {
              after = [
                "postgresql.target"
                "postgresql-setup.service"
              ]
              ++ lib.optional (d.passwordFile != null) "postgresql-role-passwords.service";
              requires = [ "postgresql-setup.service" ];
              wants = lib.optional (d.passwordFile != null) "postgresql-role-passwords.service";
            };
          }) d.services
        ) dbs;

        # `ensureUsers` creates a role with NO password -- it is built for peer
        # auth over the socket, and nixpkgs removed its password options on
        # purpose (a password in the store is world-readable). A TCP consumer
        # therefore needs its role's password set out of band, which is this.
        #
        # NOT one merged agenix template holding every ALTER ROLE statement,
        # which is the obviously tidier shape and was tried: the template would
        # live in THIS aspect's scope while each password belongs to the
        # consuming service's, and a template depending on another scope's
        # secret closes a loop -- `age.templates` -> `age.scoped.<svc>.access`
        # -> `age.templates` -> infinite recursion. It surfaces when secrets are
        # GENERATED, not at ordinary eval, so it is easy to introduce and
        # confusing to diagnose. ../security/age-scope.nix documents the
        # underlying coupling ("a template implicitly depends on every secret in
        # its scope"). Each password is therefore read from its own file here.
        #
        # Idempotent and re-run on every activation, so rotating a secret is
        # just a redeploy. The password goes in through a psql VARIABLE rather
        # than string interpolation -- one containing a quote would otherwise be
        # a syntax error at best and an injection at worst.
        rolePasswords = lib.optional (pwDbs != [ ]) {
          postgresql-role-passwords = {
            description = "Set passwords for postgres roles that authenticate over TCP";
            after = [
              "postgresql.target"
              "postgresql-setup.service"
            ];
            requires = [ "postgresql-setup.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              User = "postgres";
              Group = "postgres";
            };
            script = lib.concatMapStringsSep "\n" (d: ''
              pw=$(tr -d '\n' < ${lib.escapeShellArg d.passwordFile})
              ${lib.getExe' config.services.postgresql.package "psql"} \
                --no-psqlrc --quiet --set=ON_ERROR_STOP=1 \
                --set=pw="$pw" \
                --command "ALTER ROLE ${d.owner} WITH LOGIN PASSWORD :'pw';"
            '') pwDbs;
          };
        };
      in
      {
        # ── THE EDGE'S CREDENTIAL SCOPES, INSTALLED ──
        #
        # One `.age` source per TCP edge, materialised twice: the taker's copy
        # under the plain name (owned by its own account, read by its own
        # templates) and this server's sibling for the role-password unit. The
        # ENTRIES come from `credential`'s `realise`, which is where a
        # credential's implementation details belong; this only installs them.
        age.scoped = credentialScopes;

        services.postgresql = {
          enable = true;

          # Pinned, not left to the stateVersion default: postgres does not
          # upgrade its data directory on its own, so a version bump is a
          # dump-and-restore, not an activation. 17 because splitpro requires
          # 17.x or 18.x and nothing else here needs 18.
          #
          # Overridable so a host that already has a populated data directory at
          # another major can say so rather than be pointed at a cluster it
          # cannot read. If this box already runs postgres for home-assistant or
          # nextcloud, CHECK the on-disk version before deploying -- postgresql
          # refuses to start on a data directory written by a different major,
          # which fails closed but still fails.
          #
          # `mkOverride 900`, NOT `mkDefault`. nixpkgs' postgresql module does
          # not leave `package` at its option default -- it DEFINES it, with
          # `mkDefault`, from `system.stateVersion` (nixos/modules/services/
          # databases/postgresql.nix, "when changing the default, make it
          # conditional on system.stateVersion"). Two definitions at priority
          # 1000 are two definitions, not a default and an override, so
          # `mkDefault` here made `services.postgresql.package` "defined multiple
          # times while it's expected to be unique" (17.11 against 15.19). That
          # only fires when something FORCES the option -- the role-password unit
          # below does, as does ./restic.nix's pg_dump -- so it was a latent
          # `system.build.toplevel` failure that ordinary option reads did not
          # show. 900 beats nixpkgs' 1000 and still loses to a plain definition
          # on a host, which is exactly the intent above.
          package = lib.mkOverride 900 pkgs.postgresql_17;

          ensureDatabases = lib.unique (map (d: d.database) dbs);

          # `ensureDBOwnership` asserts that the role's name matches a database
          # in `ensureDatabases` (nixpkgs postgresql.nix), so it can only be
          # claimed when owner and database agree. A service whose unix user is
          # named differently from its database gets the role without ownership
          # and has to be granted it once by hand -- which is why `owner`
          # defaults to the database name.
          ensureUsers = map (d: {
            name = d.owner;
            ensureDBOwnership = d.owner == d.database;
          }) dbs;

          extensions = ps: map (e: ps.${e}) extNames;

          settings =
            mergeSettings dbs
            // lib.optionalAttrs (preload != [ ]) {
              shared_preload_libraries = preload;
            }
            # ── WHERE IT LISTENS, DERIVED FROM WHO DIALS IT ──
            #
            # The loopback always; the container bridge only when a taker says
            # it is in a namespace. That is `bindAddressFor`'s rule --
            # "../base/capability.nix: the union of the addresses its
            # consumers dial" -- applied by hand here, because the general
            # version has never had a consumer.
            #
            # Untouched when nobody asked: the default is localhost, and
            # widening it on a machine with no containerised consumer would
            # open the database to a bridge for nothing.
            // lib.optionalAttrs (viaDbs != [ ]) {
              # `mkForce`, because nixpkgs' own module defines
              # `listen_addresses = "localhost"` at NORMAL priority rather
              # than as a default -- so widening it is a replacement and a
              # plain definition is a conflict, not a merge. The loopback is
              # therefore kept explicitly in the value: this replaces
              # nixpkgs' answer rather than adding to it.
              listen_addresses = lib.mkForce (
                lib.concatStringsSep "," ([ "127.0.0.1" ] ++ lib.unique (map (d: d.via.address) viaDbs))
              );
            };

          # A TCP consumer needs scram over loopback. Left off entirely when no
          # service asked for it, so the socket stays the only way in.
          #
          # The bridge subnet gets its own line for the same reason it gets a
          # listener: a container's source address is on that subnet, so
          # `127.0.0.1/32` would refuse it after it had routed successfully --
          # which is the confusing half of this failure rather than the
          # obvious one.
          authentication = lib.mkIf (lib.any (d: d.tcp) dbs) (
            lib.mkAfter (
              ''
                host all all 127.0.0.1/32 scram-sha-256
                host all all ::1/128     scram-sha-256
              ''
              + lib.concatMapStrings (p: "host all all ${p} scram-sha-256\n") (
                lib.unique (map (d: d.via.prefix) viaDbs)
              )
            )
          );
        };

        systemd.services = lib.mkMerge (rolePasswords ++ consumerOrdering);
      };
  };

  # POSTGRES AS A SERVICE DEFINITION, so a database connection becomes an EDGE
  # rather than a quirk.
  #
  # ../services/postgres.nix currently collects `postgres = _: { database; owner;
  # extensions; preload; settings; tcp; }` from five consumers through a quirk
  # pool. That is already the provider-side half of a fiber -- it is bidirectional
  # in substance (consumers declare parameters, the provider configures itself
  # from its dependents) and it even gets the three merge kinds right, including
  # a THROW when two consumers disagree on a server-global setting. What a quirk
  # cannot do is the other direction, and three things need it:
  #
  #   * TELLING A CONSUMER *WHICH* SERVER. Note the correction: "a quirk flows
  #     one way" was the justification written here first, and it is FALSE.
  #     den resolves a config-dependent emission inside the consuming fixpoint
  #     (den `nix/lib/aspects/fx/class-module.nix:235-243`), so an emitter
  #     writing `postgres = { config, ... }: { ... }` reads what the collector
  #     computed, same-host, for free; cross-host, `pipe.broadcast`
  #     (`assemble-pipes.nix:790-854`) pushes a provider's value back to its
  #     consumers. Direction is not the gap.
  #
  #     The gap is ADDRESSING. A quirk's collector is a FIXED aspect -- every
  #     targeting construct in den is a fixed name, class or predicate -- so
  #     nothing answers "which postgres". That is what makes pgbouncer
  #     unreachable from a quirk: there is no way to say "whichever thing
  #     provides postgres here", only "the postgres collector".
  #     can read its dependents but can never hand back an address, a socket path
  #     or a port. Today ./nextcloud.nix just assumes peer auth over the socket
  #     and ./postgres.nix carries a `tcp` boolean that silently rewrites the
  #     server's auth config.
  #
  #   * PUTTING SOMETHING IN BETWEEN. This is the one that settles it: a quirk has
  #     no node to insert. pgbouncer is a thing that GIVES `postgres` and USES
  #     `postgres` -- structurally identical to `http` giving http and using tcp,
  #     which ../services/connection.nix already models and
  #     ../../lib/service-link.nix already resolves by intersecting over the
  #     underlay. With the edge, a consumer still writes `uses.postgres` and reads
  #     the link; whether it terminates at pgbouncer or at the server is
  #     resolution's business, so pgbouncer can be inserted with NO change at any
  #     of the five consumers.
  #
  #   * HOLDING THE CREDENTIAL. See below; this is the part that fixes something
  #     already recorded as broken.
  #
  # ── UNIX IS A CAPABILITY, AND IT ENFORCES `reach = "host"` BY CONSTRUCTION ──
  #
  # `loopback` gives `unix` and a segment does not -- measured:
  # `loopback@<host>` provides `[ loopback tcp udp unix ]` while
  # `lan-home@<host>` provides `[ lan-home tcp udp ]`. A unix socket has no
  # address, so it cannot leave the box.
  #
  # So a consumer that says `uses.unix` can only ever resolve to its OWN host's
  # loopback. §2.6's worry -- "auspc must use its own postgres, never secondpc's",
  # the failure `reach` was introduced to prevent -- stops being a rule anything
  # has to enforce and becomes unrepresentable. A consumer that genuinely wants a
  # database on another machine has to say `uses.tcp`, which is honest: different
  # auth, different exposure, and a credential where the socket case has none.
  #
  # ── THE CREDENTIAL BELONGS TO THE FIBER, AND THAT FIXES A RECORDED FAILURE ──
  #
  # ./postgres.nix explains that `ensureUsers` creates roles with NO password
  # (nixpkgs removed those options because "a password in the store is
  # world-readable"), so a TCP consumer's password is set out of band by
  # `postgresql-role-passwords`. It then records what was tried and abandoned:
  #
  #   "NOT one merged agenix template holding every ALTER ROLE statement, which
  #    is the obviously tidier shape and was tried: the template would live in
  #    THIS aspect's scope while each password belongs to the [consumer's]."
  #
  # That is constraint 5 -- the cross-scope template cycle that surfaces only at
  # `gen-secrets`, never at eval. ONE SOURCE MATERIALISED PER END removes the
  # reason it failed: the role password is the fiber's, the consumer materialises
  # its own copy owned by itself, postgres materialises its own copy owned by
  # `postgres`, and neither scope ever references the other's. There is nothing
  # left for the cycle to close around. Same mechanism as the OIDC client secret
  # in ./nextcloud.nix, which is its first instance.
  #
  # `d.passwordFile != null` -- today a flag threaded through unit dependencies --
  # then becomes "does this fiber carry a credential", which is answered by
  # whether the consumer said `uses.unix` or `uses.tcp` rather than by someone
  # remembering to set it.
  #
  # ── NOT MIGRATED YET, DELIBERATELY ──
  #
  # These are DEFINITIONS only. Nothing instantiates them and ./postgres.nix's
  # quirk is untouched, so this file changes no host's evaluated config. The five
  # consumers (nextcloud, vikunja, donetick, splitpro, home-assistant, and now
  # immich) move one at a time, and two of those files are being written by other
  # workers right now -- editing them concurrently is how a file gets clobbered.
  # ── `den.serviceTypes` IS GONE FROM THIS FILE ──
  #
  # `postgres` held `gives.postgres`, `uses.unix` and `reach`, all three of
  # which are now typed fields on the aspect above
  # (../base/capability-types.nix's `den.schema.aspect`) -- which is where they
  # belonged: "the serviceTypes also should contain the aspect, it is like an
  # abstraction over an aspect", and once the aspect carries them the
  # abstraction has nothing left to hold.
  #
  # `pgbouncer` held `gives.postgres` + `uses.postgres`, a layer over this one.
  # Dropped rather than converted: nothing instantiated it, so it was a design
  # note in option form. The question it raised is still real and is worth
  # keeping in words -- with both pgbouncer and postgres on one host, two
  # aspects would give `postgres`, and nearest-shadows-farther does not
  # disambiguate co-located providers. The answers available are a distinct
  # capability only pgbouncer consumes, an explicit `preference`, or a
  # `takes.postgres.carrier` at each consumer; the second is now expressible
  # and cheapest, since `preference` already orders candidates.

  # ── THE CAPABILITY, DECLARED WHERE THE SERVER IS ──
  #
  # Moved out of ../base/capability-types.nix: that file held every definition,
  # so "what a database edge IS" was edited in a file that knows nothing about
  # postgres while the thing that answers it lives here. The base vocabulary
  # (`address`, `tcp`, `credential`, `carried`) stays central, because every
  # other definition narrows it -- this one does not, it is this aspect's.
  # ── A DATABASE, which is the worked case for contributions ──
  # `taker` here is the postgres quirk's collector, typed. Seven
  # consumers push these fields into one server today
  den.capabilities.postgres = {
    meta.description = "A database and a role, named after whoever asked.";
    includes = [ caps.carried ];

    # COMPOSED OF a credential, not a narrowing of one: a TCP taker's role
    # password belongs to the edge -- one `.age` source, the server's copy
    takes = [ caps.credential ];

    options = {
      database = lib.mkOption {
        type = lib.types.str;
        description = "The database, named after the asker.";
      };
      user = lib.mkOption {
        type = lib.types.str;
        description = "The role, likewise. Peer auth over the socket is what makes the common case credential-free.";
      };
      over = lib.mkOption {
        type = lib.types.enum [
          "unix"
          "tcp"
        ];
        default = "unix";
        description = "How the server is reached. A take filters on this; the server furnishes it.";
      };

      # ── IS THERE A POOLER IN FRONT? ──
      #
      # Furnished, because it is a fact about what answers rather than about
      # who asks -- and a CHARACTERISTIC rather than a different capability,
      # because a pooler gives `postgres` exactly as the server does. That
      # is the whole point of ../services/pgbouncer.nix: a consumer writes
      # `takes.postgres` and resolution decides whether it terminates at the
      # pooler or at the server, so one can be put in front of the other
      # with no change at any consumer (../services/postgres.nix's own note
      # on "PUTTING SOMETHING IN BETWEEN").
      #
      # A consumer FILTERS on it when it has a reason to. Transaction
      # pooling breaks session state -- LISTEN/NOTIFY, session-level
      # advisory locks, server-side prepared statements, `SET` that outlives
      # a transaction -- so a service that needs any of those says
      # `pooled = false` and gets the server. One that wants the pool says
      # true. Saying nothing means either will do, which is the honest
      # default for a web app that opens a connection per request.
      pooled = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether this endpoint is a connection pooler rather than the server itself.";
      };
    };

    taker.options = {
      # ONE PER TAKER. The units that touch the database, so provisioning can
      # order itself ahead of them. Not derivable from the aspect name --
      services = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };

      # ONE SET, loaded once by the server. `listOf` concatenates every
      # taker's; `apply` is what stops two askers loading the same extension
      # twice.
      extensions = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        apply = lib.unique;
      };
      preload = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        apply = lib.unique;
      };

      # SERVER-GLOBAL, so two takers disagreeing is an error rather than a
      # merge. `cron.database_name` names exactly one database -- and
      # `anything` is what says so: it merges takers that set different keys
      # and refuses two definitions of one key, naming both files.
      settings = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };

      # A CONTAINERISED taker cannot use peer auth: the socket authenticates
      # the connecting process's uid, and the container's uid is the image's.
      # So it asks for TCP and brings its own password.
      tcp = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };

      # NO `passwordFile`. It was a path each TCP consumer wrote into its own
      # take and the server also had to know -- two files naming one secret.
    };

    # ONE SERVER, so these three are read as one value rather than per taker.
    takerMerged = [
      "extensions"
      "preload"
      "settings"
    ];
  };
}
