{
  den,
  lib,
  capability,
  caps,
  persons,
  ...
}:
# Turns `gateway.oidcClients` -- the shared OIDC shape each service aspect fills
# in through the `kanidmClients` class -- into kanidm's own entities.
#
# In its own file rather than in ./gateway.nix, which DECLARES that option: a
# `nixos` body that both declares an option and consumes it forces the forward
# adapters that define it too early, before den has wired `scoped` into them, and
# `scoped.<aspect>.secrets.<name>.path` in a client body then fails with
# `attribute 'scoped' missing`. Reading it from a separate aspect breaks that.
#
# ── HOISTED OUT OF THE `nixos` BODY ──
#
# `oidcTakers` is computed HERE, at file level, and not inside the class body.
# Flake-level `_module.args` -- `capability` (../base/capability.nix), `fleet`
# (../hosts/fleet.nix) -- reach an aspect FILE's arguments, not a class body's:
# nothing in the tree destructures `fleet` inside a `nixos = { ... }`, and
# ../base/builders.nix is the pattern, computing `collectBuilders` at file level
# and closing over it.
#
# It matters for more than scoping. A query that only needs the aspect graph and
# the host entities is answerable WITHOUT instantiating a host; the same
# expression inside a `nixos` body can only be forced by a full host
# evaluation, which on this box needs several GB and gets OOM-killed silently
# (../../CLAUDE.md).
#
# AND THE BODY'S REMAINING DEPENDENCY IS NOT THE HOST'S FIXPOINT, it is the
# SCOPE's. `scoped` is a per-scope view, so a secret path forces that scope's
# submodule and nothing more -- unlike `config.services.<x>`, which forces the
# machine. Those are different sizes of fixpoint and only the second is the
# expensive one, so the rule is not "get out of the body" but "depend on the
# smallest fixpoint that answers the question".
let
  # Which aspects on this host asked for OIDC. Pure over `den.aspects` plus the
  # host's own inclusion set, so it is answerable at flake level.
  # ── CROSS-HOST BY CONSTRUCTION ──
  #
  # `takersReaching`, not `takersOn`. kanidm is `reach = "household"`, so a
  # service on ANY host in the household needs a client registered here --
  # `takersOn` would have registered clients only for the services co-located
  # with kanidm and silently omitted every cross-host asker.
  #
  # An OIDC client is precisely the relation that crosses hosts: the identity
  # provider is one box and the services authenticating against it need not be.
  oidcTakers = host: capability.takersReaching den.aspects.kanidm "oidc" host;

  # ── THE EDGE, REALISED, ONE PER ASKER ──
  #
  # `capability.realise` hands back what ../base/capability-types.nix's
  # `credential` definition says a credential IS: the two accounts and the two
  # scope entries. Computed here because both ends are aspects and the whole
  # thing is a function of declarations -- no host fixpoint, and no `config`.
  realisedFor =
    host:
    lib.mapAttrs (
      _: t:
      capability.realise "oidc" {
        giver = den.aspects.kanidm;
        taker = t.aspect;
        # THE TAKER'S OWN HOST, not kanidm's. A credential materialised for a
        # cross-host asker is rekeyed to THAT host's key, which is agenix's
        # ordinary behaviour and the reason the edge has to carry which host
        # each side is on.
        host = t.host;
        peer = t.aspect;
        # AND WHICH HOST THE GIVER IS ON, which is this one: `realisedFor` is
        # called from kanidm's own config, so `host` here is where kanidm runs.
        # Stated rather than defaulted so the two ends of the edge are both
        # named, which is what `perProvider` needs to be answerable at all.
        giverHost = host;
        takerHost = t.host;
      }
    ) (oidcTakers host);

  # ── THE DOMAIN THIS INSTANCE SERVES ──
  #
  # `self.domain or <fallback>` DOES NOT WORK and never did: the aspect
  # schema declares `domain`, so the attribute EXISTS and `or` does not fire --
  # it fires on a MISSING attribute, not on a null value. The issuer below was
  # written that way, and interpolating it threw "cannot coerce null to a
  # string" the moment anything forced it. The same shape has now bitten a DNS
  # record's content, a samba account name, and an `||` that was really a
  # boolean or.
  # `lib.defaultTo`, not an `if`: the aspect schema declares `domain`, so the
  # attribute EXISTS and may be null -- which is why `or` is wrong here and
  # why this is a test rather than a fallback (../../CLAUDE.md).
  servedDomain = aspect: lib.defaultTo "ivymect.in" (aspect.domain or null);

  # ── THE DIRECTORY'S OWN NAMES, DERIVED FROM THE ONE DOMAIN ──
  #
  # kanidm derives its basedn from the domain it serves, so `auth.ivymect.in`
  # is `dc=auth,dc=ivymect,dc=in`. ../services/mail.nix held both of those as
  # literals -- the URI and the base DN, agreed with this file by hand -- which
  # is two copies of a fact only this aspect owns.
  ldapDomain = domain: "auth.${domain}";
  baseDnFor =
    domain:
    lib.concatMapStringsSep "," (label: "dc=${label}") (lib.splitString "." (ldapDomain domain));

  # kanidm's LDAPS port, loopback-only (../hosts/secondpc/sso.nix's
  # `ldapbindaddress`). Named once and read by both the port claim and the URI.
  ldapPort = 3636;

  # ── WHERE A TAKER'S BIND TOKEN LANDS ──
  #
  # Per taker, because the bind credential is per (directory, consumer): two
  # services sharing one token cannot be told apart in kanidm's log and cannot
  # be revoked separately. The path kept its existing spelling so the token
  # already minted for the mailserver is still the one it reads.
  #
  # NOT AN AGENIX SECRET, and it cannot be: kanidm returns a service account's
  # token exactly once, at creation, so there is no value for this repo to hold
  # or rekey -- only a path for the provisioning run to write.
  ldapTokenFile = name: "/var/lib/kanidm-provision/${name}-ldap.token";

  # Who asked for the directory. `takersReaching`, for the reason `oidcTakers`
  # gives above: kanidm is `reach = "household"`, so a consumer on another box
  # in the household still needs its account provisioned here.
  ldapTakers = host: capability.takersReaching den.aspects.kanidm "ldap" host;

  # The kanidm APPLICATION a consumer's people hold a password for, where it
  # named one. Read off the taker's own declaration rather than passed through
  # the edge, because a `gives` body is handed the peer and not its take.
  applicationFor = aspect: aspect.takes.ldap.application or null;
  tokenPathFor = aspect: aspect.takes.ldap.tokenPath or null;
  applicationGroupFor = aspect: aspect.takes.ldap.applicationGroup or null;
in
{
  # ── THE DIRECTORY, AS A CAPABILITY ──
  #
  # Declared HERE, in the aspect that serves it, per the rule the rest of the
  # move follows: a capability belongs to its owner, not to one big registry
  # where nobody can say whose it is.
  #
  # WHY IT IS WORTH HAVING, now rather than earlier: there are several
  # consumers. ../services/mail.nix binds dovecot to it, grocy binds for its
  # own logins, and ../hosts/secondpc/samba.nix keeps its own NT hash for the
  # one thing a read-only directory cannot do. Each of them needs the same four
  # facts -- the URI, the base DN, a bind, and what the attributes are called
  # -- and each one wrote all four out by hand against a server configured in a
  # third file.
  #
  # A NARROWING OF TCP, like `http` and `mqtt`: a consumer asking for a
  # directory is answered by one and never by some other tcp giver on the box,
  # and the port claim stays visible to ../base/firewall.nix (which leaves it
  # shut -- an exposed directory is a different threat model).
  den.capabilities.ldap = {
    meta.description = "A read-only directory, bound as a service account's API token. A narrowing of tcp.";
    side = "endpoint";
    includes = [ caps.tcp ];

    options = {
      # ── NAME-BASED, AND THAT IS NOT AN OVERSIGHT ──
      #
      # The URI carries the NAME kanidm's certificate covers, not the address
      # resolution found: LDAPS verifies the name, so every route to this
      # directory has to present the same one -- which is the same reasoning
      # the household's TLS names use (`sso.ivymect.in` resolving to a house
      # address is fine; calling it something else is not). `networking.hosts`
      # on the serving host already points it at loopback, so the connection
      # stays on the box AND verifies.
      #
      # The resolved address still has to exist, because this capability
      # narrows tcp: a consumer that cannot reach the gateway at all gets null
      # from `dial` rather than a URI that will fail at runtime.
      uris = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "Where to bind, as names the server's certificate covers.";
      };

      baseDn = lib.mkOption {
        type = lib.types.str;
        description = "The search base, derived from the domain the directory serves.";
      };

      bindDn = lib.mkOption {
        type = lib.types.str;
        default = "dn=token";
        description = "The search bind's DN. `dn=token` is kanidm's form for a service account's API token.";
      };

      tokenFile = lib.mkOption {
        type = lib.types.str;
        description = "Where this consumer's own bind token is written by the provisioning run.";
      };

      # ── WHAT THE ATTRIBUTES ARE CALLED ──
      #
      # Furnished rather than assumed, because every consumer that assumed got
      # one of them wrong: kanidm spells the account name `name` where most
      # LDAP documentation says `uid`, and a filter built on the wrong one
      # finds nobody while looking entirely correct.
      usernameAttribute = lib.mkOption {
        type = lib.types.str;
        default = "name";
        description = "The attribute holding the login name.";
      };
      mailAttribute = lib.mkOption {
        type = lib.types.str;
        default = "mail";
        description = "The attribute holding addresses. Every value reaches the person, which is also the alias mechanism.";
      };
      displayNameAttribute = lib.mkOption {
        type = lib.types.str;
        default = "displayname";
        description = "The attribute holding the human's name.";
      };
      memberOfAttribute = lib.mkOption {
        type = lib.types.str;
        default = "memberof";
        description = "The attribute holding group membership, for a consumer that authorises by group.";
      };

      # ── NULL, AND A CONSUMER HAS TO HANDLE IT ──
      #
      # kanidm's gateway is read-only and hands out no password material at
      # all, so there is no attribute to read a hash from: a consumer FINDS the
      # entry and then BINDS as that person to check the credential. Furnished
      # as an explicit null so a consumer configured for hash reads fails here,
      # where the reason is written down, rather than at runtime against an
      # empty attribute.
      passwordAttribute = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "The attribute holding password material, or null when the directory serves none.";
      };

      # ── HOW A PERSON BINDS, WHERE PEOPLE BIND AT ALL ──
      #
      # An authentication bind needs a DN per person, and kanidm's form for an
      # application password is `spn=<account>,app=<application>,<basedn>`.
      # Composed here from the application the taker named, so no consumer
      # writes that format out: `%s` is the account name, which is the
      # placeholder every LDAP client library already uses.
      #
      # Null when the taker named no application, which is the honest value --
      # nothing can bind as a person against an application that does not
      # exist.
      userDnTemplate = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "DN template for a person's authentication bind, with `%s` for the account name.";
      };
    };

    taker.options = {
      # ── THE APPLICATION, WHICH IS WHAT MAKES PEOPLE ABLE TO LOG IN ──
      #
      # kanidm application passwords are per (person, application): a mail
      # client gets a credential that works for mail and nowhere else, which is
      # what ../../patches/kanidm-provision/application-passwords.patch
      # provisions. A consumer that only SEARCHES the directory (looking up
      # addresses, say) names none and gets no application.
      application = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      # ── WHERE THIS CONSUMER WANTS THE TOKEN, IF NOT WHERE IT IS MINTED ──
      #
      # The provisioning run writes the token as `kanidm`, mode 0600 (it is an
      # `ExecStartPost` inside the kanidmd unit, `User = "kanidm"`). So a
      # consumer that reads it AS ROOT needs nothing -- dovecot's auth process
      # does, which is why ../services/mail.nix states no path and reads the
      # minted file directly. A consumer running as its own account cannot read
      # it at all, and grocy is the first: its php-fpm pool is `grocy:nginx`.
      #
      # Stating a path asks for a copy there, owned by whatever account this
      # taker furnishes through `gives.user` and mode 0400, kept in step with
      # the original. It is a PATH rather than a bool because some consumers
      # cannot choose where they read a secret from: grocy reads its bind
      # password from `settingoverrides/LDAP_BIND_PW.txt` under its own data
      # directory and nowhere else.
      #
      # This replaces a hand-written oneshot in ../services/grocy.nix. It
      # belonged here: both facts it needs -- who minted the token and which
      # account the consumer runs as -- are the capability's, and every
      # non-root consumer after the first would have copied that unit.
      tokenPath = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      # Units that read the token, so the copy is ordered ahead of them. The
      # same thing `postgres`' own `services` does for provisioning, and not
      # derivable from the aspect name: grocy's reader is `phpfpm-grocy`.
      services = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };

      # Only members of this group may hold an application password for it --
      # kanidm's own access rule, not one invented here, which is why the
      # application cannot be provisioned without naming a group. Long-term
      # this is the service's access character (`den.capabilities.service`);
      # until the household drives it, a consumer states it.
      applicationGroup = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
    };
  };
  den.aspects.kanidm = {
    includes = [
      den.aspects.gateway
      # ../services/restic.nix's `backup` quirk -- see the `backup` entry below.
      den.aspects.restic
    ];

    # ── THE GIVER SIDE, ON THE ASPECT ──
    #
    # Not a parallel `den.serviceTypes.kanidm`: "aspects/services/kanidm should
    # declare the serviceType as well, this doesnt make sense to separate the
    # serviceType from the aspect, at all, they are interrelated". Typed by
    # ../base/capability-types.nix's `den.schema.aspect`, so a misspelled field
    # here is an unknown-option error rather than a key nothing reads.
    gives =
      { self, ... }:
      {
        # ── THE LDAP GATEWAY'S PORT ──
        #
        # Declared so ../base/firewall.nix can see it and leave it SHUT: it is
        # loopback-only (../hosts/secondpc/sso.nix sets `ldapbindaddress`), and
        # an exposed directory is a different threat model entirely. Stated
        # here rather than nowhere, because a port no aspect declares is a port
        # `derived-port-exclusion` cannot catch a clash on.
        tcp = _: {
          name = self.name;
          ports.ldaps = {
            port = ldapPort;
            meta.description = "kanidm's LDAPS gateway, for an application that can only authenticate over LDAP; loopback-only, so nothing off the box dials it.";
            expose = [ ];
          };
          carrier = self.name;
        };

        # ── THE DIRECTORY ITSELF ──
        #
        # ── IT MUST CLAIM THE PORT, AND THIS ONCE DID NOT ──
        #
        # This said "NO `port` HERE ... furnishing 3636 twice from one aspect
        # would be two claims on one socket for ../base/capability.nix to
        # compare against each other". That reasoning is wrong, and the cost
        # was measured: `exclusionsIn` resolves the ENDPOINT side, `linkRaw`
        # picks exactly ONE furnished key per (aspect, capability) -- an exact
        # match, else the alphabetically first narrowing -- and `ldap` sorts
        # before `tcp`. So the clash walk asked kanidm for its endpoint, got
        # this body, found no ports, and 3636 was absent from the check
        # entirely: `endpointHalf kanidm "endpoint"` returned `ports = { }`
        # while `endpointHalf kanidm "tcp"` returned `ldaps`. The comment on
        # `gives.tcp` above says a port no aspect declares is one the exclusion
        # check cannot catch a clash on -- and this file had quietly become
        # that case.
        #
        # There is no double claim, for the same reason the bug existed: ONE
        # key is consulted per aspect, so kanidm contributes either this port
        # or the `tcp` one and never both. Same number either way -- `ldapPort`
        # is read here, by `gives.tcp`, and by the URI below.
        #
        # This is the second time alphabetical narrowing has cost something
        # (tempo's vhost vanished when it gained a `tcp` endpoint beside its
        # gRPC one), which is why `linkRaw` now prefers an exact match.
        ldap =
          { peer, ... }:
          let
            domain = servedDomain self;
            base = baseDnFor domain;
            # One name, read by `hostname` and by the URI below.
            dnsName = ldapDomain domain;
            app = applicationFor peer;
          in
          {
            name = self.name;

            # ── THE NAME, FURNISHED RATHER THAN BURIED IN A URI ──
            #
            # It was only ever inside the `uris` string, which made the link
            # unforceable: `hostname` is a required `str` on the `hostname`
            # capability, `ldap` narrows `tcp` and so carries it, and nothing
            # furnished it -- so forcing a whole `ldap` link threw
            # "`capability.ldap.hostname' was accessed but has no value
            # defined". Reading named fields worked, which is why it went
            # unnoticed until a consumer asked for the link itself.
            #
            # It is also the fact this endpoint most needs to state. LDAPS
            # verifies the NAME, so a consumer that has to check a certificate
            # or pin a CA needs it on its own, not parsed back out of a URI --
            # and the URI below is now composed FROM it, so the two cannot
            # disagree.
            hostname = dnsName;

            # The same socket the `tcp` giver claims, from the same binding.
            # Loopback-only, so ../base/firewall.nix leaves it shut whichever
            # half it reads.
            ports.ldaps = {
              port = ldapPort;
              meta.description = "kanidm's LDAPS gateway, for an application that can only authenticate over LDAP; loopback-only, so nothing off the box dials it.";
              expose = [ ];
            };

            uris = [ "ldaps://${dnsName}:${toString ldapPort}" ];

            baseDn = base;

            # PER CONSUMER. `peer` is who asked, so each one is told about its
            # own token and nothing else -- and the account that holds it is
            # provisioned below from the same name.
            #
            # WHERE IT ASKED FOR IT, where it asked: a consumer that cannot
            # read the minted file (see `tokenPath`) is told about its own copy
            # instead, and the unit that places that copy is derived below. A
            # consumer that states nothing is told about the minted file, which
            # is what a root reader wants.
            tokenFile = if tokenPathFor peer != null then tokenPathFor peer else ldapTokenFile peer.name;

            userDnTemplate = if app == null then null else "spn=%s,app=${app},${base}";
          };

        # THE ACCOUNT THIS SERVICE RUNS AS, furnished rather than restated by
        # whoever needs it. A credential's `realise` composes ownership out of
        # this, so no other file names this account.
        user = _: {
          name = "kanidm";
          group = "kanidm";
          carrier = self.name;
        };

        oidc =
          { peer, ... }:
          {
            # NAMED AFTER THE ASKER. The provider knows who is asking -- that is
            # what the edge carries -- so a consumer reads `oidc.clientId` back
            # rather than agreeing a string with this file.
            clientId = peer.name;
            issuer = "https://auth.${servedDomain self}";
            # THE CREDENTIAL IS THE EDGE'S, and it has TWO NAMED SIDES. One
            # scope, one `.age` source, materialised once per side and owned by
            # that side's unix user at 0400 -- which is exactly what
            # ../services/nextcloud.nix builds by hand today, and what the unix
            # group it used to manufacture was standing in for.
            #
            # The scope is named after BOTH ends, so two services asking kanidm
            # for OIDC get two credentials rather than sharing one, and moving
            # either end changes the scope rather than silently reusing a
            # secret.
            scope = "oidc-${self.name}-${peer.name}";
            secretName = "oidc-client-secret";
            carrier = self.name;
          };
      };

    # The house's identity provider, not fleet infrastructure -- a second
    # household would run its own.
    reach = "household";

    # NOT /var/lib/kanidm/kanidm.db.
    #
    # Everything about WHO exists here is provisioned from this repo, but
    # CREDENTIALS are not and cannot be: passkeys and passwords are enrolled by
    # each person against the live server (../hosts/secondpc/sso.nix's
    # reset-token note says why there is no other way), so that database is the
    # only copy of them and losing it means every member of the household
    # re-enrolling. It is also sqlite under an active server, and a file-level
    # copy of that is a torn copy -- so kanidm's own `online_backup` writes a
    # consistent export and THAT is what leaves the box. Same "the dump is the
    # unit, not the data directory" call ./postgres.nix makes.
    #
    # Restore is `kanidmd database restore <file>` with the server stopped.
    backup = { config, ... }: {
      name = "kanidm";
      paths = [ config.services.kanidm.server.settings.online_backup.path ];
    };

    nixos =
      {
        config,
        scoped,
        host ? { },
        ...
      }:
      let
        # The co-located consumers that asked for a readable copy of their bind
        # token. See `tokenPath` on the capability for why this exists at all.
        # `takersOn` maps a taker's NAME to its ASPECT -- it is `filterAttrs`
        # over `takers`, not the `{ aspect; host; }` records `takersReaching`
        # builds. The two are easy to confuse and the symptom is
        # "attribute 'aspect' missing" from whichever consumer guessed.
        handedOff = lib.filterAttrs (_: a: tokenPathFor a != null) (capability.takersOn host "ldap");

        # The oidc askers, keyed by name, carrying `{ aspect; host; }`. Bound
        # here because `gateway.oidcClients` below needs the TAKE and
        # `capability.realise` does not carry one -- see the note there.
        oidcHere = oidcTakers host;
      in
      {
        # `online_backup` is off by default (`versions = 0`); this is what turns
        # it on. 02:00 so the newest export is about an hour old when
        # ./restic.nix's 03:15 timer picks it up, and seven kept on disk so an
        # accident can be undone without reaching for B2 at all.
        services.kanidm.server.settings.online_backup = {
          schedule = "00 02 * * *";
          versions = 7;
        };

        # ── THE PEOPLE, FROM THE REGISTRY THAT ALREADY DESCRIBES THEM ──
        #
        # ../base/person.nix is where a human is declared -- display name, mail,
        # the raw kanidm groups they join, and `provisionKanidm` saying whether
        # they exist here at all. Every one of those fields was ALSO written out
        # by hand in ../hosts/secondpc/sso.nix's `persons.auscyber`, which is
        # the only reason the registry and the identity provider could ever have
        # disagreed -- and they did: ../persons/others.nix declares three people
        # the live instance has never heard of.
        #
        # `kanidm.name`, not the key: the key identifies the human in this repo
        # and this is what the account is called, which is the distinction
        # ../base/person.nix's header draws. They are the same string today.
        #
        # GROUP MEMBERSHIP COMES FROM HERE, not from a members list. nixpkgs'
        # module folds `persons.<p>.groups` into `groups.<g>.members`
        # (`config.members` in its group submodule), so a person joining
        # `idm_admins` is a line on the person -- and ../hosts/secondpc/sso.nix
        # keeps declaring that group only to hold `overwriteMembers = false`,
        # which is what stops the fold evicting the built-in `idm_admin`.
        services.kanidm.provision.persons = lib.mapAttrs' (
          _: p:
          lib.nameValuePair p.kanidm.name {
            inherit (p) displayName legalName mailAddresses;
            inherit (p.kanidm) groups;
          }
        ) (lib.filterAttrs (_: p: p.provisionKanidm) persons);

        # ── THE KEYS, FROM THE HUMAN WHO OWNS THEM ──
        #
        # `den.persons.<n>.sshKeys` is already where a person's keys are
        # declared once, and ../security/main-ssh-key.nix delivers them to every
        # ACCOUNT that names that person. This is the other half: the same list
        # into kanidm, so a host with no local account for somebody can still
        # authorise them (`kanidm_ssh_authorizedkeys`, enabled per host by
        # `services.kanidm.unix.sshIntegration`).
        #
        # That was the gap the user named: "i can have users which are defined
        # in kanidm with a unix user, but not as a den user but can have allowed
        # ssh keys and be provisioned from kanidm as well".
        #
        # ── THE TAG IS DERIVED FROM THE KEY ──
        #
        # kanidm addresses a key by tag and REPLACES one of the same tag, so the
        # tag decides what idempotence means here. A positional tag (`key0`,
        # `key1`) would rename every key after an insertion, deleting and
        # recreating keys that never changed; a hash of the material renames
        # nothing and gives a rotated key a new tag of its own.
        #
        # Short, because it only has to be unique within one person: 8 hex
        # characters of sha256 over the key itself.
        gateway.personSshKeys = lib.mapAttrs' (
          _: p:
          lib.nameValuePair p.kanidm.name (
            lib.listToAttrs (
              map (key: lib.nameValuePair "den-${lib.substring 0 8 (builtins.hashString "sha256" key)}" key) (
                p.sshKeys or [ ]
              )
            )
          )
        ) (lib.filterAttrs (_: p: p.provisionKanidm && (p.sshKeys or [ ]) != [ ]) persons);

        services.kanidm.provision.systems.oauth2 = lib.mapAttrs (
          _: c:
          {
            inherit (c) displayName;
            originUrl = "${c.origin}${c.callback}";
            originLanding = c.landing;
            # `name` rather than `spn`, so the username an app records is
            # `auscyber` and not `auscyber@auth.ivymect.in`. Anything matching
            # existing users by username depends on this.
            preferShortUsername = true;
            # ONE MAP PER GROUP THE CLIENT IS SCOPED ON. `scopes` defaults to
            # `[ scope ]` (../services/gateway.nix), so a client that says
            # nothing is unchanged; one scoped on its own grant groups
            # (`accessLib.groupsFor`) issues a token to the granted and to
            # nobody else.
            scopeMaps = lib.genAttrs c.scopes (
              _:
              [
                "openid"
                "profile"
                "email"
              ]
              ++ lib.optional c.groupsName "groups_name"
            );
          }
          // lib.optionalAttrs c.public { inherit (c) public; }
          // lib.optionalAttrs (c.secretFile != null) { basicSecretFile = c.secretFile; }
        ) config.gateway.oidcClients;

        # ── THE OTHER HALF: PROVISION FROM WHO ASKED ──
        #
        # This is the mutual-configuration half a query layer alone does not
        # reach. `gives.oidc` tells a CONSUMER its client id, issuer and which
        # scope holds the credential; it writes nothing at this end. But a
        # registration has to exist here too, and today each of the seven
        # clients arranges that by hand -- an entry plus a secret whose name
        # both files have to agree on.
        #
        # Derived from `takes.oidc`, both ends come from ONE declaration.
        #
        # ── WRITTEN AS THE OPTION, NOT THROUGH THE CLASS ──
        #
        # `kanidmClients` is a forwarded class whose target IS this option
        # (../services/gateway.nix: `mkForward "kanidmClients" [ "gateway"
        # "oidcClients" ]`), and it exists so OTHER aspects can contribute
        # without declaring the option. This file is the one that already reads
        # it, so it writes the option directly.
        #
        # Two things were wrong before. Written inside `nixos` as
        # `kanidmClients` it is a NixOS option that does not exist. Written at
        # aspect level as a FUNCTION, den evaluates it as a module and merges
        # the module's whole config -- including the module system's own
        # `warnings = [ ]` -- into an `attrsOf submodule`, which fails with
        # `gateway.oidcClients.warnings is not of type submodule`. The attr
        # form every other client uses has neither problem.
        # ── THREE OPTION NAMES, ALL THREE WRONG, AND NOTHING CAUGHT IT ──
        #
        # This wrote `basicSecretFile` and `preferShortUsername` and no
        # `callback`. ./gateway.nix's `oidcClients` submodule declares
        # `secretFile`, declares no `preferShortUsername` at all, and declares
        # `callback` with NO DEFAULT -- so the first aspect ever to write
        # `takes.oidc` would have met two unknown-option errors and a missing
        # required one at once.
        #
        # It went unnoticed because nothing takes `oidc` yet: measured,
        # `capability.takers "oidc"` is empty, so this `mapAttrs` has always run
        # over `{ }` and never produced an entry to typecheck. A derived
        # registration that no consumer exercises is indistinguishable from a
        # correct one.
        #
        # `preferShortUsername` is dropped rather than renamed: the oauth2
        # rendering above sets it unconditionally for every client, so stating
        # it here was a second definition of one fact.
        # ── THE TAKE COMES FROM THE TAKERS, NOT FROM `realise` ──
        #
        # `capability.realise` returns `{ giver; scope; secrets; taker }` and no
        # `take` -- measured. So `r.take.callback or ""` would have fired the
        # `or` on a MISSING attribute and registered every client with an empty
        # redirect path: the same trap that has now cost this repo six bugs
        # (`or` answers "absent", never "null"). The take lives on the taking
        # ASPECT, which `oidcTakers` carries, so that is what is read.
        gateway.oidcClients = lib.mapAttrs (
          name: r:
          let
            take = oidcHere.${name}.aspect.takes.oidc;

            # ── `or` IS CORRECT HERE, AND THAT IS NOT A CONTRADICTION ──
            #
            # A RAW take carries only the keys the taker actually set, plus the
            # base shape -- measured: a probe aspect setting only `callback`
            # produced a take with `callback` and no `displayName`, although
            # both are declared taker options. Defaults materialise through
            # `capability.contributions`, not here.
            #
            # So these attributes are genuinely ABSENT rather than null, which
            # is exactly the case `or` answers. The bug it keeps causing is the
            # opposite reading -- `x.y or default` on an attribute that EXISTS
            # and is null, where the default never fires. Both forms appear in
            # this file for that reason.
            stated = take.displayName or null;
          in
          {
            # `mkDefault`, so a taker may override it without `mkForce`.
            displayName = lib.mkDefault (if stated != null then stated else name);

            # THE TAKER'S OWN, because no provider can derive a redirect path --
            # every OIDC library picks its own. The capability carries it now
            # (../base/capability-types.nix), which is what makes `takes.oidc`
            # usable at all: it had no taker options whatsoever, so a consumer
            # had nowhere to say this and ./gateway.nix's `callback` has no
            # default.
            callback = take.callback or "";

            # THIS SIDE'S OWN COPY, and the only thing this file does with it is
            # apply `scoped`. No scope name, no entry name, no owner -- all three
            # came from the capability definition.
            secretFile = r.giver.pathIn scoped;
          }
        ) (realisedFor host);

        # ── THE SCOPE IS CREATED AT THE IMPLEMENT PLACE ──
        #
        # One scope per edge, holding the two materialisations the `credential`
        # definition produced -- the giver's copy owned by `kanidm`, the
        # taker's by whatever account that service furnishes through its own
        # `gives.user`. Neither this file nor the taker's names the other's
        # account, and neither names a path.
        #
        # Written here rather than through the aspect's `secrets` shorthand
        # because that shorthand is ONE scope per aspect
        # (../../lib/age-scoped.nix's `secretScope`), and there is one scope per
        # ASKER. The entries themselves are not written here at all: they come
        # from the capability definition.
        # ── ONE BIND ACCOUNT PER CONSUMER, FROM WHO ASKED ──
        #
        # ../services/mail.nix declared its own (`gateway.serviceAccounts.mail`
        # with a path it also wrote into dovecot's config), which is the same
        # two-ended registration `takes.oidc` above exists to collapse: the
        # consumer says it binds, and both ends follow.
        gateway.serviceAccounts = lib.mapAttrs' (
          _: t:
          lib.nameValuePair t.aspect.name {
            description = "${t.aspect.name} LDAP bind";
            apiTokenFile = ldapTokenFile t.aspect.name;
          }
        ) (ldapTakers host);

        # ── THE TOKEN, HANDED TO A CONSUMER THAT CANNOT READ IT ──
        #
        # `takersOn` and not `takersReaching`: the minted file only exists on
        # the host the provisioning run happened on, so copying it is a
        # question that only has an answer here. A consumer on another box
        # needs the token DISTRIBUTED, which is a different mechanism (an
        # agenix generator that mints it) and not this one.
        #
        # The owner is resolved, not written: whatever account the taker
        # furnishes through `gives.user`. `install -D` creates the directory
        # too, which grocy's case needs.
        #
        # `ConditionPathExists` rather than an assertion: on a first deploy the
        # service account does not exist yet, so the unit no-ops and the path
        # unit below installs the token as soon as provisioning writes it --
        # which is also how a rotation lands without a rebuild.
        systemd.services = lib.mapAttrs' (
          _: aspect:
          let
            account = capability.linkFor aspect "user" {
              inherit host;
              peer = aspect;
            };
            owner = if account == null then "root" else account.name;
            group = if account == null then "root" else (account.group or owner);
            minted = ldapTokenFile aspect.name;
          in
          lib.nameValuePair "ldap-bind-token-${aspect.name}" {
            description = "Install ${aspect.name}'s kanidm LDAP bind token where it can read it";
            wantedBy = [ "multi-user.target" ];
            after = [ "kanidm.service" ];
            before = aspect.takes.ldap.services or [ ];
            unitConfig.ConditionPathExists = minted;
            serviceConfig.Type = "oneshot";
            script = ''
              install -D -o ${owner} -g ${group} -m 0400 \
                ${lib.escapeShellArg minted} ${lib.escapeShellArg (tokenPathFor aspect)}
            '';
          }
        ) handedOff;

        systemd.paths = lib.mapAttrs' (
          _: aspect:
          lib.nameValuePair "ldap-bind-token-${aspect.name}" {
            wantedBy = [ "multi-user.target" ];
            pathConfig = {
              PathChanged = ldapTokenFile aspect.name;
              Unit = "ldap-bind-token-${aspect.name}.service";
            };
          }
        ) handedOff;

        # The applications whose passwords let PEOPLE bind, likewise derived.
        # Only consumers that named one get one; a search-only consumer has no
        # application and no person ever binds against it.
        gateway.applications = lib.listToAttrs (
          lib.concatMap (
            t:
            let
              app = applicationFor t.aspect;
            in
            lib.optional (app != null) (
              lib.nameValuePair app {
                displayName = t.aspect.name;
                linkedGroup = applicationGroupFor t.aspect;
              }
            )
          ) (lib.attrValues (ldapTakers host))
        );

        # Naming an application without the group that may hold its passwords
        # is a provisioning failure at deploy time -- kanidm requires the link
        # -- so it is an eval error here instead.
        assertions = lib.mapAttrsToList (_: t: {
          assertion = applicationFor t.aspect == null || applicationGroupFor t.aspect != null;
          message = "den.aspects.${t.aspect.name}: takes.ldap.application is set but applicationGroup is not. Only members of a linked group may hold an application password -- name the group whose people may log in.";
        }) (ldapTakers host);

        age.scoped = lib.mapAttrs' (_: r: lib.nameValuePair r.scope { inherit (r) secrets; }) (
          realisedFor host
        );
      };
  };

}
