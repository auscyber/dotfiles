{
  lib,
  den,
  genScope,
  record,
}:
# The instance type for `den.services` -- top-level, owned by anything, and
# IDENTIFIED BY ITS RESOLVED DEPENDENCIES.
#
# `services` is a function of the whole registry, because resolving a want needs
# to see every other service. It is passed in rather than read from `config`, so
# the direction stays one-way (see aspects/base/person.nix's header for what
# happens otherwise).
#
# Identity, per the notation den itself uses in fx/identity.nix (`/` joins a
# path, `@<kind>=<name>` binds an entity):
#
#   celler@secondpc          a service placed on a host
#   main/celler@secondpc     the `main` cache depending on that celler
#   other/celler@secondpc    a second cache on the same celler
#
# `/` is a service-to-service DEPENDENCY; `@` is PLACEMENT on a host. Both
# parametrise identity; a want that is neither -- a database, say -- does not.
#
# THE LINE BETWEEN `includes` AND `gives`, written down because it reads oddly
# and will be re-asked otherwise: `includes` is the EDGE, `gives` is the CONTENT
# the edge carries. `gives` looks like a relation and is a DECLARATION -- the set
# of interfaces this service satisfies, each with the manner of satisfying it --
# while `includes` is the relation that transfers one service's `gives` into
# another's. `allProvides` is the result of that transfer. A third word,
# `defines`, is the one that invents something new: `gives` says what I can be
# USED AS, `defines` says what I am made of.
#
# INHERITANCE (`includes`) is the second axis, and it is NOT a dependency: `lan`
# includes `connection` because a lan IS A connection narrowed, where a cache
# WANTS a celler. The two never mix -- `includes` contributes capabilities and
# obligations downward and does not touch identity, `uses` parametrises identity
# and contributes nothing.
#
# TWO AXES, AND THEY ARE NOT ONE MECHANISM. The consumption chain inherits too --
# `main/celler@secondpc` is on secondpc because the celler is -- but that
# inheritance CANNOT be a derived field here, and the reason is structural rather
# than stylistic: ./service-scope.nix builds the graph from `host`, `reach` and
# `allProvides`, and the chain is only known from `resolved`, which is an ANSWER
# the graph gives. A `host` or `allProvides` default that read `resolved` would
# close graph -> field -> resolved -> graph, which is the deadlock that file's
# header names. So the consumption chain inherits through `identity` -- which
# already spells the whole chain, `@secondpc` included -- and through consumers
# reading `resolved`/`children` as data, which ../aspects/base/celler/server.nix
# already does for its cache list. `includes` is the axis that may be a field,
# because it is DECLARED and so can be merged before any answer exists.
#
# WHY `provides` IS A LEGITIMATE FIELD and `endpoints`/`access`/`users`/`group`
# were not (they were added and backed out within the hour; see `serviceName`
# below). The test is whether the field is knowable from the entity and its own
# ancestors. Each of those four is knowable only IN RELATION to a second
# abstraction -- `endpoints` to a transport, `access` to an identity provider,
# `group` to the access collector, `users` to a household -- so declaring them
# made this type the union of every capability anything might relate to.
# `provides` needs no second abstraction: `{ tcp = { }; }` on a lan is true of
# the lan alone, which is why it can be merged along `includes` with no lookup.
# The earlier `provides` that recursed did so through its DEFAULT, not its
# existence -- `default = [ config.serviceName ]` -- and that fallback now lives
# in ./service-scope.nix. The field reads nothing.
#
# Three things that inheritance is deliberately NOT:
#
#   * NOT `imports`. A module's `imports` may not reference `config`, and an
#     include IS config (`config.den.services.connection`). Delivering a schema
#     through `imports` recursed on every service in this registry, including
#     ones that declared nothing, and Nix's own hint names the cause ("if you
#     get an infinite recursion here, you probably reference `config` in
#     `imports`"). So it is a DERIVED MERGE over a list of references.
#   * NOT typed. `strict = false` below means a removed or misspelled option
#     silently becomes untyped freeform data, so an obligation an includer fails
#     to define cannot be caught by the type system. The check is explicit.
#
#     THE RULE THAT FALLS OUT OF THAT, and it cost a measured regression to
#     learn: DO NOT WRITE A MIGRATION FALLBACK OVER A FREEFORM OPTION. When
#     `wants` was renamed to `uses`, every file still writing `wants` had its
#     definition silently accepted as freeform data, `config.uses` came back
#     empty, and all four celler identities quietly lost their chain -- `main`
#     instead of `main/celler@secondpc` -- with no error anywhere. A read-site
#     shim (`s.uses or s.wants or { }`) does not fix that, it HIDES it: over a
#     freeform option a fallback and the bug are indistinguishable, so a rename
#     can never fail loudly again.
#
#     The `wants` option below is deliberately not that. It is DECLARED, so the
#     old spelling is type-checked rather than freeform, and `uses` reads it
#     through a `default` so there is one source of truth. Declaring the old name
#     is the safe shape; falling back to freeform is not.
#   * NOT a default that reads the registry. An instance default that reads
#     another option the registry-wide graph also reads recursed:
#     `provides = [ config.serviceName ]` did exactly that, because
#     ./service-scope.nix reads every service's capabilities while building the
#     type those instances evaluate against. Such a fallback lives in that file,
#     where `services` is a plain function argument.
services:
let
  inherit (lib) mkOption types;

  # The scope graph over the registry, built ONCE from declared fields only.
  # `reach` is placement in it; see ./service-scope.nix.
  scope = import ./service-scope.nix {
    inherit lib genScope;
    # Which household each host serves, read from the host ENTITY's declared
    # option. Safe for the reason the whole model rests on: the module system is
    # lazy per option, so reading one host-entity option does not force that
    # host's `resolved`. Asking `hasAspect` would, and would close the loop.
    hostHouseholds = lib.concatMapAttrs (
      _: hs:
      lib.concatMapAttrs (
        hn: h: if (h.household or null) == null then { } else { ${hn} = h.household.name; }
      ) hs
    ) den.hosts;
  } services;

  # parent rawIdentity -> [ declaration keys of its children ], built ONCE for
  # the whole type.
  #
  # This was a `filterAttrs` over `services` inside the `children` option's
  # default -- i.e. inside the per-instance module -- so every instance built
  # its own scan of the whole registry: O(n^2) by construction. The fix is a
  # HOIST, not a library: this `let` is outside the instance module and
  # `services` is a function parameter, so it is one shared thunk, read once,
  # and not read at all if nothing asks for `children`.
  #
  # Researched the alternative before writing this: gen-graph's
  # `labeledTranspose` is the vetted version (Theta(n+E), one shared index,
  # and it preserves edge labels). Not adopted, because with six services the
  # quadratic is ~36 comparisons -- it is worth fixing because it is wrong by
  # CONSTRUCTION, not because it is slow -- and because nothing else in this
  # repo or in den speaks gen-graph. See aspects/framework/flake-file.nix.
  childIndex = lib.foldl' (
    acc: key:
    let
      s = services.${key};
      ws = lib.attrNames (lib.filterAttrs (_: w: w.identifying) s.uses);
    in
    if ws == [ ] then
      acc
    else
      let
        parent = (s.resolved.${lib.head ws}).rawIdentity;
      in
      acc // { ${parent} = (acc.${parent} or [ ]) ++ [ key ]; }
  ) { } (lib.attrNames services);

  # provider rawIdentity -> consumer key -> [ { use; contribution; } ], for the
  # NON-IDENTIFYING edges. The identifying half is `children`; together they are
  # every inbound edge.
  #
  # THIS IS THE OTHER HALF OF WHAT A USE MEANS. A use is existential plus a
  # CONTRIBUTION -- "I need a postgres somewhere, and I am configuring it this
  # way" -- so a provider has to be able to read what its consumers asked for.
  # ../aspects/services/postgres.nix does exactly that today by quirk,
  # collecting five services' `{ database; owner; extensions; ... }`; this is the
  # same collection reached through the edge that already exists instead of
  # through a side channel.
  #
  # GROUPED BY CONSUMER, not a flat list, because the question a provider
  # actually asks is per-consumer: celler mints ONE token per taker covering the
  # union of what that taker takes, so the grouping has to be the taker. A flat
  # list would make every reader re-group it, and they would not all agree.
  #
  # The inner value is a LIST because one consumer may hold several edges onto
  # one provider (`uses.tcp` and `uses.udp` both landing on one loopback), so the
  # consumer key does not identify the edge.
  #
  # Hoisted for the same reason as `childIndex` below -- one shared thunk over
  # the registry rather than one scan per instance -- and lazy, so a registry
  # where nothing reads `consumers` never resolves a non-identifying edge.
  useIndex = lib.foldl' (
    acc: key:
    let
      s = services.${key};
      edges = lib.attrNames (lib.filterAttrs (_: u: !u.identifying) s.uses);
    in
    lib.foldl' (
      acc': use:
      let
        p = (s.resolved.${use}).rawIdentity;
        here = acc'.${p} or { };
        row = {
          inherit use;
          contribution = s.uses.${use};
        };
      in
      acc'
      // {
        ${p} = here // {
          ${key} = (here.${key} or [ ]) ++ [ row ];
        };
      }
    ) acc edges
  ) { } (lib.attrNames services);

  # Transitive `children`, as declaration keys. Terminates because identity is
  # derived FROM the parent -- a cycle in this walk would already have been an
  # infinite recursion in `rawIdentity`, so there is nothing to guard here that
  # is not already guarded.
  descendantKeys =
    rid:
    let
      kids = childIndex.${rid} or [ ];
    in
    kids ++ lib.concatMap (k: descendantKeys services.${k}.rawIdentity) kids;

  useType = types.submodule (
    { name, ... }:
    {
      # NO `imports` here, and this is the finding that matters for §3d: a want
      # schema CANNOT be delivered by `imports`, because a module's `imports`
      # may not reference `config`. Nix says so directly -- "if you get an
      # infinite recursion here, you probably reference `config` in `imports`"
      # -- and `schemas = config.den.serviceSchemas` is config. It recursed on
      # every service, including ones with no uses at all.
      #
      # So the side table is not the answer by itself. The contribution has to
      # arrive through a channel that is not a type: a per-name `freeformType`,
      # or validation by assertion rather than by typing.
      #
      # THE FREEFORM PART IS THE WANT'S WHOLE POINT, and it took reading
      # ../aspects/services/postgres.nix to see it. A want is not a reference to
      # an instance; it is EXISTENTIAL PLUS A CONTRIBUTION -- "I need a postgres
      # somewhere, give me one, and I am configuring it THIS way". So
      # `uses.postgres = { database = "celler"; owner = "celler"; }` is both
      # halves at once: `name` resolves the existential, and everything freeform
      # beside it is configuration flowing TO the provider. The edge is
      # bidirectional -- the consumer gets an endpoint, the provider reads its
      # inbound edges and configures itself from them.
      #
      # That repo already runs this, by quirk rather than by want: five services
      # emit `{ database; owner; extensions; preload; settings; tcp; }` and
      # ../aspects/services/postgres.nix collects all of them. And the answer it
      # gives to "who declares the contribution's shape" is the one to copy --
      # THE PROVIDER NORMALISES RAW DATA, IT DOES NOT TYPE IT (`norm` there, with
      # `d.owner or d.database` and friends, plus a `mergeSettings` that THROWS
      # on two consumers disagreeing). No submodule, no schema side table, no
      # `imports`, and so nothing to recurse. `provides.<cap>`'s params are where
      # the vocabulary gets written down for a reader; enforcement stays with
      # whoever collects.
      # THE TOP LEVEL IS CLOSED, and that is the point of this shape.
      #
      # It used to be `freeformType = attrsOf raw` with a handful of reserved
      # keys carved out of it, and the rule that grew from that was "every new
      # role for the edge's attrset gets its own reserved key, because it is a
      # shared keyspace with three readers". That rule was arrived at by hitting
      # a real bug: `uses.postgres = { settings.work_mem = "32MB"; }` -- a
      # genuine ../aspects/services/postgres.nix shape -- parsed as a CARRIER
      # named `settings`. The keyspace was shared between the resolver's
      # directives and the provider's configuration, so the two could collide
      # and the collision was silent.
      #
      # Putting the content in NAMED REGIONS retires that rule instead of
      # extending it. Nothing is free at top level, so a new content role needs
      # no new reserved key and cannot collide with a directive:
      #
      #   DIRECTIVES -- how this edge RESOLVES. Read by the resolver only.
      #     identifying   this edge parametrises the wanter's identity
      #     via           narrow to a carrier (`uses.tcp.via.lan-home = { }`)
      #     needs         what the edge REQUIRES of its carrier
      #                   (`uses.http.needs.bodyBytes = null`, matched against
      #                   `provides.<cap>.limits.<key>`, satisfied iff
      #                   need <= limit with null unbounded either side)
      #     on            narrow to an INSTANCE (`uses.celler-push.on.main`).
      #                   Path-named targets cannot be keyed on `identity`:
      #                   that would force `resolved` and break acyclicity.
      #
      #   CONTENT -- what the edge CARRIES, by which side receives it.
      #     shared        one copy, both ends read it
      #     giving        lands on the GIVER's side
      #     taking        lands on the TAKER's side
      #
      # WHY THE SIDES ARE STRUCTURAL AND NOT A LABEL. The alternative was a
      # directive saying who reads a credential (`credential.readers =
      # [ "taker" ]`). That can disagree with where the declaration actually
      # went, and the disagreement is invisible: a symmetric credential
      # mislabelled asymmetric lands only in the app's scope, kanidm has
      # nothing to compare, and the failure is a 401 at first login rather than
      # an eval error. Declaring it under a region makes residency a
      # consequence of where it is written, so there is nothing to contradict.
      #
      # The three cases this tree already runs, placed:
      #
      #   taking   ../aspects/base/celler/_lib.nix's token. ASYMMETRIC -- only
      #            the taker reads it; the server verifies with its own RS256
      #            key and never sees the token. It lands in the taker's scope
      #            today (`secrets/generated/auspc/celler-host/...`) and that is
      #            correct, so `taking` is a name for what already happens.
      #   giving   that RS256 signing key. Only the server holds it.
      #   shared   kanidm's OIDC client secret. SYMMETRIC -- both ends compare
      #            one string, which is why ../aspects/services/nextcloud.nix
      #            manufactures a unix group holding both `kanidm` and
      #            `nextcloud` rather than giving either a copy that can drift.
      #            This is also the only case where a shared scope is REQUIRED
      #            and not merely tidier: stored under one end, the other end's
      #            template closes the `age.templates` -> `age.scoped.<other>`
      #            cycle that surfaces at `gen-secrets` and not at eval.
      #
      # No vocabulary is declared INSIDE a region, by the postgres-normalises
      # principle: the provider owns the words, and `provides.<cap>`'s params are
      # where they get written down for a reader.
      options = {
        name = mkOption {
          type = types.str;
          readOnly = true;
          default = name;
        };

        via = mkOption {
          type = types.attrsOf types.raw;
          default = { };
          description = "Narrow this edge to one carrier. Checked before `needs`, so a pin matching nothing reports the more specific fault.";
        };

        needs = mkOption {
          type = types.attrsOf types.raw;
          default = { };
          example = lib.literalExpression "{ bodyBytes = null; }";
          description = ''
            What this edge REQUIRES of whatever carries it, matched against the
            carrier's `provides.<cap>.limits.<key>`. Satisfied iff
            `need <= limit`, with `null` meaning unbounded on either side, so an
            ABSENT limit is no limit -- a lan declares no `bodyBytes` because a
            lan does not cap body size.

            This is what dissolves §2.7's traffic classes rather than relocating
            them: `"bulk-upload"` is an opaque token needing a central table to
            mean anything, while a limit is self-describing. The hand-written
            `cloudflared-tunnel.exclude` for `cache.ivymect.in` falls out of
            `needs.bodyBytes = null` against the tunnel's 100MB cap, instead of
            being remembered.
          '';
        };

        on = mkOption {
          type = types.attrsOf types.raw;
          default = { };
          example = lib.literalExpression "{ main = { }; }";
          description = ''
            Narrow this edge to a particular INSTANCE rather than to a carrier.
            `uses.celler = { }` means "whichever celler is reachable from here",
            which is the normal case and is what makes a host with its own
            celler use its own; `uses.celler.on.secondpc` means that one.
          '';
        };

        shared = mkOption {
          type = types.attrsOf types.raw;
          default = { };
          description = "Content both ends read, in one copy. The symmetric-credential case.";
        };

        giving = mkOption {
          type = types.attrsOf types.raw;
          default = { };
          example = lib.literalExpression ''{ database = "vikunja"; owner = "vikunja"; }'';
          description = ''
            Content that lands on the GIVER's side -- what the provider collects
            and configures itself from. Reads as a statement about that side:
            "on the giving side there is a database called vikunja".

            It lands on the giver's NEAREST IMPLEMENTING ANCESTOR, not
            necessarily on the edge's target, because anything reached through
            `defines` has no implementation of its own: a push and a pull have
            none, `celler-cache` has none, `celler` has one. That upward
            resolution is also what makes §3a's forward collision impossible
            rather than worked around -- sub-things never get their own scope,
            so there is no second resolution to double-declare
            `kanidmProvision` in.
          '';
        };

        taking = mkOption {
          type = types.attrsOf types.raw;
          default = { };
          description = ''
            Content that lands on the TAKER's side. Per-instance content merges
            across a fiber's edges (a substituter line is list-valued, so two
            pulls on one host both land); anything SINGULAR must be assembled at
            fiber level instead -- `nix.settings.netrc-file` is one path per
            host, so a host pulling from two non-public caches has two edges
            wanting one file, and the file is built by iterating the fiber's
            edges rather than emitted per edge.
          '';
        };

        identifying = mkOption {
          type = types.bool;
          default = false;
          description = ''
            This dependency decides WHICH instance the wanter is, so it becomes
            part of the wanter's identity: a cache wanting a celler is
            `main/celler@secondpc`, and the same declaration against a second
            celler is a second instance.

            False for a want that is a resource rather than an identity -- a
            database is something a service USES, not something it IS. Marking
            such a want identifying is the silent-merge failure mode, so it is
            opt-in rather than inferred.
          '';
        };
      };
    }
  );
in
den.lib.schema.mkInstanceType {
  strict = false;
  extraModules = [
    (
      { name, config, ... }:
      let
        # THE INHERITANCE MERGE, through gen-algebra's record fold rather than
        # hand-rolled `listToAttrs`/`concatLists`.
        #
        # `obliges` folds with "semilattice-set", which is the ACI join:
        # invariant under layer order and under duplicate contributions. That is
        # the property a diamond needs -- two includes that both include
        # `connection` contribute its obligation once, and swapping them cannot
        # change the answer, so `includes` is a SET of parents and not a
        # priority list.
        #
        # `provides` folds with "recursive" (a `//` fold) and NOT with
        # "semilattice-set", and the reason is a limit of the library rather
        # than a choice: semilattice-set is LIST-only there (it folds with `++`
        # and `builtins.elem`, seeded from `defaults.<f> or [ ]`), while
        # `provides` is capability -> params, an attrset. The partial fit is
        # worth recording: `//` is associative and idempotent, but commutative
        # only where two layers agree on a capability's parameters, so a
        # narrowing that RESTATES an inherited capability's parameters wins by
        # being the later layer. For a narrowing that is the intended reading;
        # it is not the ACI join, and two unrelated includes disagreeing about
        # one capability's parameters resolve silently by include order.
        #
        # `foldLayersTraced` and not `foldLayers`: `foldLayers` refuses
        # "semilattice-set" by name (only the traced fold implements it), and
        # the trace is what the obligation message needs -- `provenance.obliges`
        # is the ordered `[ { layer; value; } ]` chain, so a missing obligation
        # can name the include that imposed it.
        # THREE RELATIONS, AND THEY ARE NOT INTERCHANGEABLE. The whole model is
        # one KIND -- everything here is a service -- and these:
        #
        #   defines.<label>   a NEW sub-type this service invents. SCOPING
        #                     HAPPENS HERE AND NOWHERE ELSE: `celler` defining
        #                     `cache` yields the class `celler-cache`, because
        #                     `cache` means nothing outside celler, and a cache
        #                     defining `push`/`pull` yields `celler-cache-push`
        #                     and `celler-cache-pull` rather than bare verbs any
        #                     other service might also claim.
        #   gives             an EXISTING, top-level service OFFERED. A lan gives
        #                     `tcp` and `udp`; it did not invent them, so they
        #                     keep the names they already have. Unscoped for that
        #                     reason and not by exception -- which is what keeps
        #                     the agnostic `uses.tcp` the resolver is built on.
        #   uses.<svc>        a need, WITH REQUIREMENTS attached. `push` and
        #                     `pull` both use `http` and differ only in what they
        #                     require of it, which is precisely why they are two
        #                     services and not two labels on one edge.
        #
        # WHAT GIVING TRANSFERS, the question that mattered: ONLY THE NAME, into
        # `allProvides`. `includes` transfers a whole interface -- provides,
        # obliges, edgeObliges, defines and gives all flow into the includer --
        # and if `gives` did that, a lan offering `tcp` would inherit tcp's
        # obligations and a consumer could masquerade as a provider. It does not.
        # `tcp` becomes reachable THROUGH the lan; the lan does not become a tcp,
        # tcp does not become a lan, and nothing flows upward at all.
        #
        # `gives` IS AN ATTRSET AND NOT A LIST, and the value is the reason: it
        # declares the MANNER of giving, not merely the fact of it.
        #
        #   gives.http = { limits.bodyBytes = 104857600; };   capped at 100MB
        #   gives.tcp  = { };                                 unqualified
        #
        # An `implements`-style list could say only THAT an interface is
        # satisfied; this says how, which is where the resolver's `limits` live
        # and what lets `push` and `pull` be answered differently by the same
        # carrier. Folded straight into `provides`, because that is what it is --
        # `provides` is the older spelling of the same declaration and both are
        # declared options, so the merge has one result and neither spelling is
        # freeform.

        # The one place a scoped class name is formed. Already-scoped names
        # arrive from `includes` untouched, which is what makes "scoped by its
        # DEFINER" hold through an inheritance chain: a narrowing of `celler`
        # still offers `celler-cache`, not `<narrowing>-cache`.
        scopedDefines = lib.mapAttrs' (
          label: body: lib.nameValuePair "${config.serviceName}-${label}" body
        );

        strategies = {
          provides = "recursive";
          obliges = "semilattice-set";
          edgeObliges = "semilattice-set";
          defines = "recursive";
          gives = "recursive";
        };
        defaults = {
          provides = { };
          obliges = [ ];
          edgeObliges = [ ];
          defines = { };
          gives = { };
        };

        fromIncludes = record.foldLayersTraced {
          inherit strategies defaults;
          # `serviceName` of another instance, read from a default on this one.
          # Safe because it is never forced on the success path: the traced fold
          # stores a layer label verbatim and never reads one, so these thunks
          # are forced only by the throw below.
          layerNames = map (s: s.serviceName) config.includes;
          layers = map (s: {
            provides = s.allProvides;
            obliges = s.allObliges;
            edgeObliges = s.allEdgeObliges;
            defines = s.allDefines;
            gives = s.allGives;
          }) config.includes;
        };

        # Own declarations as the last layer, so a narrowing may restate an
        # inherited capability's parameters.
        withOwn = record.foldLayersTraced {
          inherit strategies defaults;
          layerNames = [
            "includes"
            "self"
          ];
          layers = [
            fromIncludes.value
            {
              inherit (config) obliges edgeObliges;
              provides = config.provides // config.gives;
              defines = scopedDefines config.defines;
              gives = config.gives;
            }
          ];
        };

        # THE OBLIGATION CHECK. What `includes` demands of ME is the join over
        # the includes alone -- my own `obliges` binds my includers, not me,
        # which is what lets `connection` oblige an address while defining none.
        inherited = fromIncludes.value.obliges;

        alternatives = alt: if lib.isList alt then alt else [ alt ];

        # Discharged by a NON-NULL value. `host` is `nullOr str` defaulting to
        # null and, with `strict = false`, an undeclared option such as
        # `address` is freeform data that is simply absent -- so "absent or
        # null" is the one test that covers both shapes.
        defined = opt: (config.${opt} or null) != null;

        missing = lib.filter (alt: !(lib.any defined (alternatives alt))) inherited;

        sourceOf =
          alt:
          let
            froms = map (e: e.layer) (lib.filter (e: lib.elem alt e.value) fromIncludes.provenance.obliges);
          in
          if froms == [ ] then "an include" else lib.concatStringsSep " and " froms;

        # THE EDGE OBLIGATION, the other half and a different subject. `obliges`
        # binds a NODE that includes me; `edgeObliges` binds an EDGE that points
        # at me -- "a `uses` edge to a network must carry an address". The two
        # exist because a connection turned out to be a RELATION rather than a
        # node: the address is a fact about the attachment, so the thing that
        # has to carry it is the edge, not either endpoint.
        #
        # Same ACI join down the same `includes` chain, so `lan-home` inherits
        # "edges to me need an address" from `network` with no declaration of its
        # own. One field and a reuse of the fold above, which is why this cost a
        # strategy key rather than a mechanism.
        #
        # Checked HERE, inside `resolved`, because that is the only place both
        # ends of the edge exist at once: the entry is the declaration and the
        # provider is what the scope graph answered. It names the provider rather
        # than the include that imposed the obligation -- the provider's own
        # provenance lives in its instance's `let`, not on its interface, and
        # "the thing you are using demands this" is the actionable half anyway.
        edgeCheck =
          use: entry: provider:
          let
            required = provider.allEdgeObliges or [ ];
            unmet = lib.filter (alt: !(lib.any (f: (entry.${f} or null) != null) (alternatives alt))) required;
          in
          if required == [ ] || unmet == [ ] then
            true
          else
            throw "den.services.${name}.uses.${use}: ${provider.serviceName} requires every edge to it to carry ${
              lib.concatMapStringsSep "; " (
                alt: lib.concatStringsSep " or " (map (f: "`${f}`") (alternatives alt))
              ) unmet
            }, and this one does not. An edge attribute is a fact about the ATTACHMENT -- an `address` is neither the consumer's nor the provider's -- so it is declared on the use and nowhere else.";

        # Named by DECLARATION KEY, never `identity`: identity is derived, so
        # interpolating it into a message raised while the instance is being
        # derived comes back as an unrelated recursion (see `resolved` below).
        obligationCheck =
          if config.abstract || inherited == [ ] || missing == [ ] then
            true
          else
            throw "den.services.${name}: ${
              lib.concatMapStringsSep "; " (
                alt:
                "${sourceOf alt} obliges ${
                  lib.concatStringsSep " or " (map (o: "`${o}`") (alternatives alt))
                }, which is not defined here"
              ) missing
            }. An included service's obligations must be discharged by everything that includes it, which is what makes `includes` an interface rather than a note. Define one of the named options, or set `abstract = true` if this entry is a TYPE others narrow rather than something placed anywhere.";
      in
      {
        options = {
          host = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "The host this instance is placed on, by fleet name. The `@` half of its identity.";
          };

          owner = mkOption {
            type = types.nullOr types.raw;
            default = null;
            description = ''
              What this service is FOR, and so who may want it -- a household
              owns amenities for people, and `null` is fleet infrastructure that
              serves hosts (celler). Distinct from `host`, which is placement,
              and from `reach`, which is how far a want may resolve.
            '';
          };

          # ── WHAT UNIX USER THIS RUNS AS ──
          #
          # Declared, NOT created. Almost every service here is a nixpkgs module
          # that already makes its own account -- `services.nextcloud` runs as
          # `nextcloud`, and nothing in this tree should be inventing a second
          # opinion about that. So this RECORDS the name an upstream module
          # already chose, which is what lets other aspects reach it:
          #
          #   users.users.${svc.user}.extraGroups = [ "media" ];
          #
          # Before this, "which user does this service run as" was a fact spread
          # across each service's own file and restated by hand at every site
          # that needed to grant it something -- so a service that changed its
          # account silently left the grants pointing at nothing.
          #
          # `null` because not every service has one: an abstract type narrows
          # into several, and a service that is only an edge (`connection`) runs
          # as nobody. A consumer that needs a user must therefore handle null
          # rather than assume.
          user = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "nextcloud";
            description = ''
              The unix user this service's processes run as, by name.

              Recorded here, not created here: the service's own module owns the
              account, and this names it so that group membership and file
              ownership elsewhere can refer to it instead of restating a literal.

              `null` means this service has no account of its own -- an abstract
              type, or a service that is only a dependency edge.
            '';
          };

          # The point of knowing `user` at all. Written on the service rather
          # than at the group's end because the service is what needs the
          # access, and a service moving hosts should carry its memberships with
          # it; ../aspects/base/service-user.nix folds these onto whichever host
          # runs the instance.
          groups = mkOption {
            type = types.listOf types.str;
            default = [ ];
            example = lib.literalExpression ''[ "media" ]'';
            description = ''
              Extra unix groups this service's `user` is added to on the host
              that runs it. Requires `user` to be set; the groups themselves
              must exist, since this only grants membership.
            '';
          };

          # WHERE THIS IS INJECTED, and it is a REFERENCE where it can be one.
          #
          # This was an enum of three words, and the middle one did not survive
          # contact with the graph: "household" named a single hardcoded scope
          # node that every host in the fleet hung off, including hosts serving
          # no household. One node standing for an entity that can have several
          # instances is an enum pretending to be a graph, and it cannot say
          # WHICH household a segment belongs to.
          #
          # So a household-scoped service names the household entity:
          #
          #   reach = den.households.ivymectin;   # this wire, this house
          #
          # The two words that remain are the graph's own ends, and they stay
          # words because there is no entity to reference: "fleet" IS the root
          # scope, and "host" is wherever this instance runs -- which for an
          # instance is already carried by `host`.
          reach = mkOption {
            type = types.either (types.enum [
              "host"
              "fleet"
            ]) types.raw;
            default = "host";
            example = lib.literalExpression "den.households.ivymectin";
            description = ''
              How far this service is visible: `"host"` (only where it runs),
              `"fleet"` (the root scope, everywhere), or a HOUSEHOLD ENTITY --
              visible to the hosts that serve that household and nowhere else.

              Placement, and nothing else. Where a service RUNS is `host`; this
              is where its declaration is planted, and the `parent*` walk does
              the rest -- a host-local provider shadows a fleet one for free,
              and a host outside the household simply never sees the segment.
            '';
          };

          includes = mkOption {
            type = types.listOf types.raw;
            default = [ ];
            example = lib.literalExpression "[ config.den.services.connection ]";
            description = ''
              The services this one IS A NARROWING OF -- `lan` includes
              `connection`, so a lan is a connection with more said about it, and
              `loopback` includes it too. References to other service CONFIGS,
              not keys, so the merge below needs no lookup table.

              A SET, not an ordered list: `allObliges` joins over it with the ACI
              strategy, so order and duplicates are both invisible. `allProvides`
              is the exception, see its description.

              Distinct from `uses`: an include contributes capabilities and
              obligations and does not touch identity; a want parametrises
              identity and contributes nothing. And distinct from `imports`,
              which cannot be used for this at all -- see this file's header.
            '';
          };

          provides = mkOption {
            type = types.attrsOf types.raw;
            default = { };
            example = lib.literalExpression "{ tcp = { }; udp = { }; }";
            description = ''
              Capabilities this service offers, capability name -> PARAMETERS.

              An attrset rather than a list of names because a capability
              CARRIES PARAMETERS: a `tcp` that says nothing and a `tcp` with an
              MTU floor are the same capability differently constrained, and a
              want is matched against the capability while the parameters are
              what the match then has to satisfy.

              Own declarations only -- read `allProvides` for the inherited set.
              There is deliberately no default derived from `serviceName`; the
              fallback for a service that declares nothing lives in
              ./service-scope.nix, for the reason in this file's header.
            '';
          };

          obliges = mkOption {
            type = types.listOf (types.either types.str (types.listOf types.str));
            default = [ ];
            example = lib.literalExpression ''[ [ "host" "address" ] ]'';
            description = ''
              Options that everything INCLUDING this service must define. A
              string is one required option; a list of strings is an ANY-OF --
              `connection` obliges `[ "host" "address" ]`, because a connection
              is to something and an implementation may say which either way (a
              loopback knows its host, a wan link only knows an address).

              Binds includers, never this service: `connection` defines neither,
              which is exactly what makes it a base. Inherited onward, so
              anything including `lan` owes `connection`'s obligations too.
            '';
          };

          edgeObliges = mkOption {
            type = types.listOf (types.either types.str (types.listOf types.str));
            default = [ ];
            example = lib.literalExpression ''[ "address" ]'';
            description = ''
              What every `uses` EDGE pointing at this service must carry. Same
              any-of shape as `obliges` and the same ACI join down `includes`,
              but a different subject: `obliges` binds a NODE that includes me,
              this binds an EDGE that points at me.

              `network` sets `[ "address" ]`, because `192.168.0.26` is a fact
              about neither the host nor the wire but about the ATTACHMENT
              between them -- which is the whole reason a connection turned out
              to be a relation rather than a node. Checked in `resolved`, the one
              place both ends of the edge exist at once.
            '';
          };

          defines = mkOption {
            type = types.attrsOf types.raw;
            default = { };
            example = lib.literalExpression "{ cache = { }; }";
            description = ''
              Sub-types this service INVENTS, keyed by an unscoped label. THE ONE
              PLACE SCOPING HAPPENS: `celler` with `defines.cache` yields the
              class `celler-cache`, and a cache with `defines.push` yields
              `celler-cache-push`, because `cache` and `push` mean nothing
              outside what defines them and a bare `push` is a label any other
              service could claim. Read `allDefines` for the scoped names.

              Everything defined is a SERVICE -- a push is not an action or a
              capability or a third kind of thing. The only difference between a
              cache and a push is CARDINALITY, which `instanceName` carries: a
              celler may have several caches so each needs a name, a cache has
              exactly one push so the class name IS the instance.

              DEEPER LEVELS ARE IMPLIED, NOT DECLARED. Declaring an instance
              `main` of `celler-cache` implies instances of everything
              `celler-cache` defines, and so on down. This field is the TYPE
              relation that cascade walks; the cascade itself is a policy's --
              see `allDefines`.

              A defining edge also carries the definer's STATE DOWN (a celler's
              master key reaching its caches), which is the opposite direction
              from a `uses` edge and why they are separate relations.
            '';
          };

          gives = mkOption {
            type = types.attrsOf types.raw;
            default = { };
            example = lib.literalExpression "{ tcp = { }; http.limits.bodyBytes = 104857600; }";
            description = ''
              The interfaces this service satisfies, each with the MANNER of
              satisfying it -- `gives.http = { limits.bodyBytes = 104857600; }`
              is "I give http, capped at 100MB", and `gives.tcp = { }` is "I give
              tcp, unqualified".

              An attrset and not a list for exactly that reason: a list could say
              only THAT an interface is satisfied, and the manner is what lets
              one carrier answer `push` and `pull` differently. It is also why
              this is not called `implements`.

              Not a relation, although it reads like one -- which is the thing
              that makes the line feel odd and so is worth saying twice:
              `includes` is the EDGE and `gives` is the CONTENT it carries.
              Merged into `allProvides`, which is what the resolver reads.

              Distinct from `defines`, which invents a NEW scoped sub-thing. This
              declares what I can be USED AS; that declares what I am made of.
            '';
          };

          abstract = mkOption {
            type = types.bool;
            default = false;
            description = ''
              This entry is a TYPE -- a narrowing others include -- rather than
              an instance placed anywhere, so it discharges no obligations.
              `connection`, `lan` and `loopback` are abstract; a host's own
              loopback is not.

              Declared rather than derived, and that is a decision worth the
              line: "abstract" could be read off the graph as "nothing includes
              me", but `includes` holds service CONFIGS and not keys, so
              inverting it means comparing submodule configs for equality --
              which Nix cannot be trusted to do (a config attrset carries
              `_module`, and comparing functions aborts uncatchably). The flag
              costs one word per type and buys a check that cannot misfire.
            '';
          };

          # `builtins.seq obligationCheck`: the check has to be forced by
          # something every consumer already reads, or laziness makes it dead
          # code -- the same reason `identity` below carries its own collision
          # check instead of delegating to a separate derived value.
          # ./service-scope.nix reads `allProvides` for every service, so
          # attaching it here checks the whole registry.
          allProvides = mkOption {
            type = types.attrsOf types.raw;
            readOnly = true;
            default = builtins.seq obligationCheck withOwn.value.provides;
            description = ''
              Every capability this service offers: its own `provides` merged
              with the transitive closure of `includes`.

              Transitive without a walk, because each include contributes its
              own `allProvides` -- the recursion is the option's, so a chain of
              any depth costs one merge per link and a diamond's shared base is
              merged once per path rather than once per node. Acyclic is on the
              declarer: `includes` pointing back at an includer is an infinite
              recursion here with no better message available, which is the
              general cross-entity rule in this repo (references point DOWN).
            '';
          };

          # FIELD VALUES DO NOT INHERIT THROUGH `includes` -- capabilities and
          # obligations do. So `den.services.loopback.address = "127.0.0.1"` on
          # the DEFINITION never reached `loopback@<host>`, every `tcp`
          # resolution came back with no address, and `link.endpoint` could not
          # work at all. Measured, not reasoned: `loopback.address` was
          # `127.0.0.1` while `loopback-auspc.address` was unset.
          #
          # The fix belongs here and not in whatever generates instances. An
          # instance can be made several ways -- a derived key today, a host
          # attachment tomorrow -- and a rule copied into each generator is a
          # rule that will be copied wrong once. Declared as an option with an
          # inheriting default, it holds however the instance came to exist.
          #
          # NEAREST WINS: an instance's own address beats an inherited one, and
          # the walk is shallow-first over `includes` in order, so a narrowing
          # may override the thing it narrows. `null` means "not addressable",
          # which is the honest answer for a `unix` socket and the reason this
          # is `nullOr` rather than throwing.
          # ── WHICH OF THIS TYPE'S FIELDS INHERIT THROUGH `includes` ──
          #
          # `address`, `prefix`, `domain` and `public` USED TO BE DECLARED HERE,
          # with inheriting defaults. They do not belong: they are properties of
          # particular SUBTYPES -- a segment has a prefix, a network may have a
          # DNS domain, a loopback has an address -- and not of being a service.
          # Declaring them here made this file the union of every field anything
          # might carry, which is exactly the mistake §3d records being made and
          # backed out once already with `endpoints`/`access`/`group`/`users`.
          #
          # What IS general is that SOME field values inherit down `includes`,
          # since `includes` otherwise merges only capabilities and obligations.
          # So this type carries the MECHANISM and knows none of the names: a
          # subtype lists its own inheriting fields and
          # ../aspects/base/service-instances.nix copies the nearest value down
          # when it creates an instance.
          #
          #   den.serviceTypes.network.inherits = [ "prefix" "domain" ];
          #   den.serviceTypes.loopback.inherits = [ "address" ];
          #
          # Lean, extensible, and assumption-free in the sense that matters: a
          # new subtype with a new field needs no change to this file.
          inherits = mkOption {
            type = types.listOf types.str;
            default = [ ];
            example = lib.literalExpression ''[ "prefix" "domain" ]'';
            description = ''
              Field names whose values an instance of this type takes from the
              nearest definition in its `includes` chain that states one.

              The names are the subtype's, never this type's. `strict = false`
              means such a field is freeform data here, so nothing type-checks
              it -- the honest limit, and the reason the names live with whoever
              gives them meaning rather than in the relation primitive.
            '';
          };

          # `implement` USED TO BE DECLARED HERE and is gone. It held module
          # content as a function of the resolved edges -- which is exactly what
          # a den aspect's `nixos` already is, so it was a second name for one
          # thing, and a service ended up described twice: once as a type with
          # relations, once as an aspect with config.
          #
          # A serviceType IS an aspect with relations. Its `nixos` is freeform
          # here (`strict = false`) and delivered by
          # ../aspects/base/service-implement.nix with `bindings` applied, so a
          # body reads `{ postgres, http, cache, ... }` and names no carrier, no
          # path and no port.

          bindings = mkOption {
            type = types.raw;
            readOnly = true;
            default =
              let
                # THE INSTANCE ITSELF. A body's own fields are reachable no
                # other way: an attachment's `address` and `interface` are its
                # own, not an edge's and not an ancestor's, so a definition that
                # configures a NIC from them has nothing to ask for without
                # this. Listed first below so a want or ancestor actually named
                # `self` shadows it rather than the reverse.
                self = services.${name};

                # A LAYER'S ADDRESS IS ITS UNDERLAY'S. `http` has none of its
                # own, so an http link answers whatever tcp that http resolved.
                # This lives on the link and NOT on an `address` option: an
                # option default that reads `resolved` makes a DEFINITION
                # resolve, definitions resolve against the empty registry, and
                # that does not throw -- it fails to terminate.
                addressOf =
                  provider:
                  if (provider.address or null) != null then
                    provider.address
                  else
                    lib.findFirst (a: a != null) null (
                      map (p: p.address or null) (lib.attrValues (provider.resolved or { }))
                    );

                # The CLASS of an instance is the definition it includes; its
                # LABEL is that class with its definer's prefix stripped
                # (`celler-cache` under `celler` is `cache`). The label is what
                # a body says, because that is what the level is CALLED by
                # whoever defined it -- binding by `serviceName` gave `main`,
                # the instance's own name, which a definition cannot know.
                classOf = key: lib.head ((services.${key}.includes or [ ]) ++ [ null ]);
                labelOf =
                  key:
                  let
                    cls = classOf key;
                    p = services.${key}.parent or null;
                    pcls = if p == null then null else classOf p;
                  in
                  if cls == null then
                    services.${key}.serviceName
                  else if pcls == null || !(lib.hasPrefix "${pcls.serviceName}-" cls.serviceName) then
                    cls.serviceName
                  else
                    lib.removePrefix "${pcls.serviceName}-" cls.serviceName;

                # PARENT AND IDENTIFYING EDGE ARE ONE RELATION. The cascade
                # STAMPS `parent` on an instance it created under another; a
                # TAKER declares the same relation as an identifying `uses`, and
                # which provider it resolved to is which instance it is.
                # `resolveWant` gives the provider's KEY, which `resolved` does
                # not -- and a key is what the walk needs to continue upward.
                identifyingTargetOf =
                  key:
                  let
                    me = services.${key};
                    stamped = me.parent or null;
                    ids = lib.attrNames (lib.filterAttrs (_: u: u.identifying) me.uses);
                  in
                  if stamped != null then
                    stamped
                  else if ids == [ ] then
                    null
                  else
                    scope.resolveWant key (lib.head ids);

                ancestorsOf =
                  key:
                  let
                    p = identifyingTargetOf key;
                  in
                  if p == null || !(services ? ${p}) then { } else { ${labelOf p} = services.${p}; } // ancestorsOf p;

                # ── THE LINK IS BINARY WHEN THE TARGET IS ELSEWHERE ──
                #
                # A unary "nearest provider" lookup is right only when the thing
                # being reached is on this machine. A pull on auspc reading
                # secondpc's cache resolves `http@auspc` nearest, whose underlay
                # is auspc's OWN loopback -- so the body would render
                # `127.0.0.1` and point at nothing.
                #
                # `linkBetween` over the UNDERLAY is what makes this one rule
                # rather than a case per protocol: the intersection is over
                # segments and an `http` node carries no prefix, but an http
                # link between two hosts IS the tcp link with a scheme on top,
                # and the provider carries its own `uses.tcp`.
                farKey = identifyingTargetOf name;
                farHost = if farKey == null then null else (services.${farKey}.placement or null);
                myHost = config.placement;
                crossHost = farHost != null && myHost != null && farHost != myHost;

                linkOf =
                  wantName: provider:
                  let
                    underlay = lib.attrNames (provider.uses or { });
                    cap = if underlay == [ ] then wantName else lib.head underlay;
                    link = if crossHost then scope.linkBetween cap name farKey else null;
                  in
                  {
                    address = if link == null then addressOf provider else link.address;
                    carrier = if link == null then provider.serviceName else link.chosen.network;
                    # DERIVED, never declared: a `protocol` stated on a
                    # definition can disagree with the carrier that actually
                    # won, and then the URL is wrong in a way nothing checks.
                    protocol = if (provider.tls or false) then "https://" else "http://";
                  }
                  # THE EDGE'S OWN ATTRIBUTES, which is where the port lives --
                  # "the link using tcp has a port", so it is neither end's.
                  // (config.uses.${wantName}.shared or { });
              in
              { inherit self; } // lib.mapAttrs linkOf config.resolved // ancestorsOf name;
            description = "What `implement` may bind: `self`, each resolved want by name, and each ancestor by its defining label.";
          };

          allEdgeObliges = mkOption {
            type = types.raw;
            readOnly = true;
            default = withOwn.value.edgeObliges;
            description = "Every requirement this service places on edges to it, own plus inherited. ACI, so a diamond imposes one.";
          };

          # NOT the instance cascade. This joins the TYPE RELATION down
          # `includes`, so a narrowing gives whatever its bases give -- which is
          # the same fold as `allProvides` and costs one strategy key.
          #
          # Expanding `gives.cache.main = { }` into the push and pull it implies
          # is a different operation and is NOT here: it walks instance -> type's
          # `gives` -> implied instances -> their types' `gives`, which CREATES
          # registry entries. A derived option cannot add keys to the attrset it
          # is an entry of, so the cascade belongs to whatever writes
          # `den.services`, i.e. a policy. What it needs from this side is
          # exactly this field plus `abstract` to tell a type relation from an
          # instance declaration -- both present.
          #
          # AND IT NEEDS A BOUND. `includes` recursing is caught by Nix as an
          # infinite recursion on `allProvides`; a type that transitively GIVES
          # itself is an unbounded cascade in the policy, where there is no
          # option fixpoint to notice. A depth limit with a named refusal beats
          # an abort, and the limit is cheap because the real hierarchy is three
          # deep (celler -> cache -> push/pull).
          # The TYPE relation, joined down `includes`, keyed by the SCOPED class
          # name. Not the instance cascade.
          #
          # Expanding `celler-cache` instance `main` into the push and pull it
          # implies is a different operation and is NOT here: it walks instance
          # -> its type's `defines` -> implied instances -> their types' defines,
          # which CREATES registry entries. A derived option cannot add keys to
          # the attrset it is an entry of, so the cascade belongs to whatever
          # WRITES `den.services`, i.e. a policy. What it needs from this side is
          # this field, `abstract` to tell a type relation from an instance
          # declaration, and `instanceName` to know whether an implied instance
          # needs a name -- all three present.
          #
          # AND IT NEEDS A BOUND. `includes` recursing is caught by Nix as an
          # infinite recursion on `allProvides`; a type that transitively DEFINES
          # itself is an unbounded cascade in the policy, where there is no
          # option fixpoint to notice and nothing to blame. A depth limit with a
          # named refusal beats an abort, and the limit is cheap because the real
          # hierarchy is three deep (celler -> cache -> push/pull).
          allDefines = mkOption {
            type = types.attrsOf types.raw;
            readOnly = true;
            default = withOwn.value.defines;
            description = "Sub-types this service defines, own plus inherited, keyed by SCOPED class name (`celler-cache`).";
          };

          allGives = mkOption {
            type = types.attrsOf types.raw;
            readOnly = true;
            default = withOwn.value.gives;
            description = "Interfaces this service satisfies with their manner, own plus everything its `includes` give. A view on `allProvides`, which merges the older `provides` spelling in too.";
          };

          allObliges = mkOption {
            type = types.raw;
            readOnly = true;
            default = withOwn.value.obliges;
            description = ''
              Every obligation this service imposes on its includers: its own
              `obliges` joined with everything `includes` imposes. The join is
              ACI, so a diamond imposes a shared obligation once.
            '';
          };

          uses = mkOption {
            type = types.attrsOf useType;
            default = { };
            example = lib.literalExpression ''{ celler.identifying = true; postgres = { database = "x"; }; }'';
            description = ''
              What this service USES, keyed by the service or capability it
              needs. The edge of the graph, and the only one that carries data in
              both directions: a use is EXISTENTIAL PLUS A CONTRIBUTION -- "I
              need a postgres somewhere, give me one, and I am configuring it
              this way" -- so the consumer gets an endpoint and the provider
              reads its inbound edges (`consumers`) to configure itself.
            '';
          };

          # Every want resolved to a concrete instance.
          #
          # The throws below name the DECLARATION key, not `identity`: identity
          # is derived from `resolved`, so interpolating it into a message that
          # `resolved` raises is circular and the throw never surfaces -- it
          # comes back as an unrelated recursion instead. Found by testing the
          # throw rather than trusting it. The resolution, not a
          # declaration, is what parametrises identity -- so a typo cannot
          # quietly create a second entity the way a hand-written key could.
          resolved = mkOption {
            type = types.raw;
            readOnly = true;
            default = lib.mapAttrs (
              use: entry:
              let
                key = scope.resolveWant name use;
              in
              if key == null then
                throw "den.services.${name}.uses.${use}: nothing visible from ${scope.scopeOf config} declares a service '${use}'. Declared, by scope: ${
                  lib.concatStringsSep "; " (
                    lib.mapAttrsToList (
                      s: ds: "${s} -> ${if ds == [ ] then "nothing" else lib.concatStringsSep ", " ds}"
                    ) scope.declaredIn
                  )
                }."
              else
                builtins.seq (edgeCheck use entry services.${key}) services.${key}
            ) config.uses;
            description = ''
              Each want, resolved to the one service that satisfies it -- by a
              scope-graph query, not a filter.

              `reach` is PLACEMENT (./service-scope.nix): the walk is `parent*`
              from this service, and the NEAREST declaration wins, so a
              host-local provider shadows a fleet one with no filter. Two
              providers at the SAME scope are still an ambiguity and gen-scope
              refuses by name, which is the case that should shout.
            '';
          };

          # The aspect that implements this service, auto-linked by
          # `serviceName`. Resolved ONCE, in a scope bound to this instance --
          # see ../aspects/base/service.nix's `service-claim` policy and §3a:
          # resolving one aspect in two scopes double-declares any forward it
          # reaches, which is why the host must stop including it directly.
          aspect = mkOption {
            type = types.nullOr types.raw;
            default = den.aspects.${config.serviceName} or null;
            defaultText = "den.aspects.\${serviceName}, if one exists";
          };

          # The services whose IDENTIFYING want resolved to this one -- so a
          # celler's caches. Derived, never declared: the children are whoever
          # pointed at me, which is the same inversion `service.users` uses.
          #
          # This is what makes one aspect instance per PARENT enough: the aspect
          # reads its children as DATA rather than needing a scope each, so
          # there is nothing to collide.
          children = mkOption {
            type = types.raw;
            readOnly = true;
            default = lib.getAttrs (childIndex.${config.rawIdentity} or [ ]) services;
          };

          # The inbound NON-IDENTIFYING edges, with their contributions, grouped by
          # consumer -- what a provider reads to configure itself from what was
          # asked of it. `children` is the identifying half; this is the rest.
          consumers = mkOption {
            type = types.attrsOf types.raw;
            readOnly = true;
            default = useIndex.${config.rawIdentity} or { };
            description = "Inbound non-identifying edges as `consumer -> [ { use; contribution; } ]`.";
          };

          # EVERY TAKER OF ANYTHING UNDER ME, GROUPED BY TAKER -- the query a
          # provider aspect actually has, and the reason the index exists.
          #
          # celler does not want "who uses the celler"; it wants every taker of
          # any cache, push or pull beneath it, grouped so that ONE token per
          # taker can cover the union of what that taker takes. `consumers` alone
          # answers only for this node, and asking each descendant separately
          # gives a per-edge answer that the caller then has to re-group -- which
          # is the regrouping every reader would otherwise write differently.
          #
          # `of` is which of my sub-services the taker took, so a token can name
          # its scope. The sub-tree is the transitive `children`, which is the
          # given sub-tree as it exists TODAY; once the cascade writes implied
          # instances it is the same walk over the same relation.
          takers = mkOption {
            type = types.attrsOf types.raw;
            readOnly = true;
            default =
              let
                mine = [
                  config.rawIdentity
                ]
                ++ map (k: services.${k}.rawIdentity) (descendantKeys config.rawIdentity);
              in
              lib.foldl' (
                acc: rid:
                lib.foldlAttrs (
                  acc': consumer: edges:
                  acc'
                  // {
                    ${consumer} =
                      (acc'.${consumer} or [ ])
                      ++ map (e: {
                        of = rid;
                        inherit (e) use contribution;
                      }) edges;
                  }
                ) acc (useIndex.${rid} or { })
              ) { } mine;
            description = "`taker -> [ { of; use; contribution; } ]` over this service and everything beneath it.";
          };

          # The bare kind, before identity. `name` on a top-level entry is the
          # registry key, which IS the identity -- so the kind has to be stated.
          # NO bespoke `endpoints` / `access` / `group` / `users` here. They were
          # added and backed out within the hour, which is the lesson: each of
          # them only means something IN RELATION to another abstraction --
          # `endpoints` to a transport, `access` to an identity provider,
          # `group` to the access collector, `users` to a household. Declaring
          # them on the service makes this type the union of every capability
          # anything might relate to, which is the inconsistency and the
          # maintenance load.
          #
          # They belong to the PROVIDER, contributed into `uses.<provider>`.
          # §2.2 already said so and I did not follow it: "The sub-declaration
          # types are contributed by the PROVIDING service (postgres declares
          # what a postgres need accepts), so the schema stays open without a
          # central list."
          #
          # See §3d in the plan for the contribution mechanism this needs.

          serviceName = mkOption {
            type = types.str;
            default = name;
            description = ''
              The CLASS of this service, as opposed to which instance -- and the
              class name is SCOPED BY WHOEVER DEFINES IT: `celler` defines a
              cache, so the class is `celler-cache`, and the things you can do
              with one are `celler-push` and `celler-pull` rather than bare
              `push`/`pull` that any other service might also claim. `cache`
              means nothing outside celler, so it never appears alone.

              ONE EXCEPTION, and it is UNCONFIRMED -- flagged rather than
              relied on: the prefix comes from where a label is DEFINED, not
              from who gives it. `tcp` is defined once by the protocol layer and
              every connection gives that same `tcp`, so scoping it to its giver
              would make `lan-tcp` and `tailscale-tcp` distinct labels and
              destroy the agnostic `uses.tcp` the resolver is built on. Universal
              labels stay bare; everything a single service defines gets that
              service's prefix.
            '';
          };

          # WHICH instance, where there is more than one possible. Null is not a
          # missing name, it is a CARDINALITY: exactly one of this class exists
          # in its context, so the class name IS the instance -- a cache needs to
          # be called `main` because a celler may have several, a push does not
          # because a cache has exactly one.
          #
          # Inferred from this one field rather than declared as a separate
          # `singleton`, which was the alternative: the identity rule and the
          # cardinality rule then cannot disagree, because they are one rule.
          # Everything a service `gives` is a SERVICE either way -- a push is not
          # a different KIND of thing from a cache, it is a service of which
          # there is one.
          # THE PARENT INSTANCE'S KEY, stamped by the cascade that created this
          # instance -- never derived. A sub-instance exists because something
          # defined it, and the cascade knows which one at DECLARATION time.
          #
          # Declared rather than derived for the reason that governs this whole
          # file: ./service-scope.nix builds the graph from declared fields and
          # reads a lookup scope while resolving, so anything the graph consumes
          # must not be a function of resolution. `placement` may walk the
          # identifying chain because nothing the graph reads reads IT; a lookup
          # scope cannot.
          #
          # It is what lets a sub-instance look up from its PARENT's scope
          # rather than straight from its host, which is what makes siblings
          # visible to each other: a push and a pull of one cache are declared
          # in the same scope, so a push can resolve its own pull without a
          # global lookup that would find every other cache's pull too. It also
          # means reaching a cache is a question about the cache, not a
          # traversal through whichever host happens to hold it.
          parent = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Registry key of the instance that defined this one, or null for a top-level instance. Stamped by the cascade, not written.";
          };

          instanceName = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "main";
            description = "Which instance of `serviceName` this is, or null where exactly one exists.";
          };

          # The identity WITHOUT the collision check. Split out so the check in
          # `identity` can compare every instance without recursing: reading
          # `identity` would re-enter the check, which would read `identity`.
          rawIdentity = mkOption {
            type = types.str;
            readOnly = true;
            internal = true;
            default =
              let
                identifying = lib.attrNames (lib.filterAttrs (_: w: w.identifying) config.uses);

                # ONE SEGMENT OF THE PATH, and it has to be self-describing: the
                # CLASS always, the instance name only where there is one to say.
                # `push/main/celler@secondpc` never says that `main` is a cache,
                # so a reader has to look it up -- and this string is what every
                # error message in this file prints, which is exactly when
                # nobody can afford a lookup. `celler-push/celler-cache:main/
                # celler@secondpc` reads as "the push of celler-cache `main` of
                # the celler on secondpc" with nothing to consult.
                #
                # `:` and not `.`, because `.` is already taken -- a published
                # name like `cache.ivymect.in` is dotted, so a dotted separator
                # could not be told from the name it separates. PROVISIONAL:
                # the separator is not confirmed, and it is one string here.
                #
                # Backward compatible on purpose: with `instanceName` null the
                # segment is the bare class, which is what every existing
                # identity already is. Setting the scoped class and the instance
                # name on a declaration is what turns the new shape on, one file
                # at a time, rather than all at once.
                segment =
                  if config.instanceName == null then
                    config.serviceName
                  else
                    "${config.serviceName}:${config.instanceName}";
              in
              if identifying == [ ] then
                if config.host == null then segment else "${segment}@${config.host}"
              else if lib.length identifying > 1 then
                throw "den.services.${config.serviceName}: ${lib.concatStringsSep ", " identifying} are all marked `identifying`, so this service has no single identity. Exactly one dependency may carry it."
              else
                "${segment}/${config.resolved.${lib.head identifying}.rawIdentity}";
          };

          # WHERE THIS SERVICE ACTUALLY IS. `host` is DECLARED placement and is
          # null on a given sub-service -- `push` is placed nowhere, which is the
          # measured bug: `push/main/celler@secondpc` is reachable at secondpc
          # because the CELLER is there, and a link to it threw because nothing
          # said so. This walks the identifying chain to the nearest declared
          # host.
          #
          # DERIVED HERE, AND IT HAS TO BE, which is the part worth reading:
          # `host` itself cannot carry this. ./service-scope.nix builds the graph
          # from `host`, so a `host` default that read `resolved` would close
          # graph -> host -> resolved -> graph, and that is the deadlock this
          # file's header already records twice. `placement` is safe for the
          # mirror-image reason: nothing the graph reads ever reads it.
          #
          # STAMPING `host` IN THE CASCADE IS STILL BETTER and this does not
          # replace it -- a cascade writing `host = <giver's host>` makes
          # placement a declared fact, after which this option is `host` and can
          # go. It exists because the cascade does not, and because a derived
          # answer beats a throw in the meantime.
          placement = mkOption {
            type = types.nullOr types.str;
            readOnly = true;
            default =
              if config.host != null then
                config.host
              else
                let
                  identifying = lib.attrNames (lib.filterAttrs (_: u: u.identifying) config.uses);
                in
                if identifying == [ ] then null else config.resolved.${lib.head identifying}.placement;
            description = "The host this service is on, declared or inherited from whatever gives it. Read this, not `host`, to address a service.";
          };

          identity = mkOption {
            type = types.str;
            readOnly = true;
            default =
              let
                me = config.rawIdentity;
                clashes = lib.attrNames (lib.filterAttrs (_: s: s.rawIdentity == me) services);
              in
              if lib.length clashes > 1 then
                throw "den.services: identity '${me}' is claimed by ${lib.concatStringsSep " and " clashes}. An identity is derived from what an identifying want resolved to, so two declarations reaching the same provider with the same `serviceName` collide -- rename one, or narrow a `reach`."
              else
                me;
            description = ''
              This instance's identity, DERIVED from what its identifying want
              resolved to -- never typed, so it cannot disagree with the
              dependency it names.

              The collision check lives HERE rather than in a separate derived
              value because laziness makes an unconsumed check dead code: it has
              to be forced by the thing everyone already reads.
            '';
          };
        };
      }
    )
  ];
} den.schema.service
