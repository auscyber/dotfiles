{
  den,
  lib,
  config,
  checkLib,
  ...
}:
# ── WHAT THIS REPLACES, AND WHY THAT VERSION EXISTED ──
# CAPABILITY RESOLUTION AS A DEN EXTENSION.
# This is the whole genuinely-new idea, and it is small. Research into den's
let
  capabilities = config.den.capabilities;
  inherit (den.lib.policy) include mkPolicy;

  # `gives` TAKES THE SCOPE CONTEXT, like any other parametric aspect. It is
  # handed an attrset -- never a positional `self` -- for the reason everything
  givenBy =
    ctx: aspect:
    let
      args = ctx // {
        self = aspect;

        # THE RESOLVER, HANDED TO THE GIVER. A giver often needs a NESTED
        # capability of the thing it is furnishing for: postgres names a
        resolve =
          cap: target:
          linkFor target cap {
            host = ctx.host or { };
            peer = ctx.peer or target;
          };
      };
      g = aspect.gives or null;
    in
    if g != null && den.lib.canTake.atLeast args g then g args else { };

  # ── THE CHAIN, READ OFF THE PARAMETERS ──
  # ./capability-types.nix dropped `gives` from the capability record, because a
  # parametric definition already declares its ancestors by destructuring them:
  # `push = { cache, celler, ... }:` says a push hangs beneath a cache beneath a
  # celler. So the exposure relation is the inverse of the parameters, derived
  # rather than declared a second time.
  paramsOf =
    cap:
    let
      f = (capabilities.${cap} or { parametric = null; }).parametric;
    in
    if f == null then [ ] else lib.attrNames (lib.functionArgs f);

  # THE DIRECT PARENT is the parameter that itself requires all the others --
  # `push` takes `cache` and `celler`, `cache` takes `celler`, so `cache` is the
  # parent and `celler` the grandparent. That is the transitive reduction of the
  # requires relation, and it is why a definition may name its whole chain
  # without having to say which link is which.
  parentOf =
    cap:
    let
      ps = paramsOf cap;
      covers = p: lib.all (q: q == p || lib.elem q (paramsOf p)) ps;
      direct = lib.filter covers ps;
    in
    if direct == [ ] then null else lib.head direct;

  # WHAT HANGS BENEATH THIS ONE. The old `def.gives`, inverted out of the
  # parameters instead of listed beside them.
  exposedBy = cap: lib.attrNames (lib.filterAttrs (c: _: parentOf c == cap) capabilities);

  # ── BINDING THE CHAIN, which is what makes a nested capability concrete ──
  # The registry holds the chain-free view: ./capability-types.nix applies the
  # definition with nulls so its DECLARATIONS (`options`, `includes`, `takes`)
  # are readable without a chain, which is what lets `includes = [ caps.tcp ]`
  # keep referencing by value. Re-applied here with the resolved ancestors, so
  # `furnishes` and `realise` can read them.
  #
  # Overlaid rather than re-run through the module system: the declarations are
  # chain-independent and were already type-checked by the registry's own pass,
  # and a submodule evaluation per (capability, chain) is a cost this layer
  # cannot carry.
  bind =
    chain: cap:
    let
      def = capabilities.${cap} or null;
      f = if def == null then null else def.parametric;
    in
    if f == null then
      def
    else
      let
        args = lib.functionArgs f;
        body = f (lib.intersectAttrs args chain);
      in
      def // removeAttrs body (lib.attrNames args) // { chain = lib.intersectAttrs args chain; };

  # ── THE OPTION SET A GIVER IS ACTUALLY CHECKED AGAINST ──
  # Capability membership follows the type closure: a giver of `tcp` is a giver
  # of `address`, because ../base/capability-types.nix declares tcp a narrowing
  capSurface =
    cap:
    let
      def = capabilities.${cap} or null;
      edges = if def == null then [ ] else map (c: c.name) (def.includes ++ def.takes);
    in
    lib.unique ([ cap ] ++ lib.concatMap capSurface edges);

  # REVERSED, so the most specific wins. `capSurface` is `[ cap ] ++ parents`,
  # so folding it forwards let an included capability override the one that
  # narrows it -- backwards, and it is `carried.name` that made it matter:
  # `user` declares a required `name` and would have inherited the generic
  # nullable one.
  capOptions =
    cap:
    lib.foldl' (acc: c: acc // ((capabilities.${c} or { options = { }; }).options)) { } (
      lib.reverseList (capSurface cap)
    );

  # ── THE OPTIONS OF ONE HALF ──
  # A link is composed from a CARRIER (which knows the address and the transport
  # facts) and an ENDPOINT (which knows the port and the scheme), so each half
  sideOptions =
    want: cap:
    lib.foldl' (
      acc: c:
      let
        def = capabilities.${c} or null;
      in
      if def == null || (def.side != want && def.side != "either") then acc else acc // def.options
    ) { } (lib.reverseList (capSurface cap));

  # ── AND THE TYPES ARE ENFORCED, not merely documented ──
  # Furnished values go through a real submodule, so a giver that misspells a
  # field or furnishes a string where an int belongs fails AT THE GIVER with the
  typeAgainst =
    opts: cap: value:
    let
      t = lib.types.submodule (
        { config, ... }:
        {
          options = opts;
          # NO CREDENTIAL PATH OR NAME HERE. Where a generated secret is filed
          # is decided once, by `age.secrets.<n>.scope` in ../../lib/age-scoped.nix;
          # a credential only says which link it belongs to.
          config = lib.optionalAttrs (opts ? port) {
            # ── `port` IS A SINGLETON `ports`, NAMED AFTER THE CAPABILITY ──
            # So every reader reads one attrset and a giver writes whichever
            # spelling is natural. The key was the literal `default`, which
            # named nothing: `gives.tcp.port = 8095` is this aspect's tcp port,
            # so `ports.tcp` is what it is, derived from the capability being
            # typed rather than from a word that happens to sort first.
            ports = lib.mkIf (config.port != null) { ${cap} = config.port; };
          };
        }
      );
    in
    t.merge
      [ "capability" cap ]
      [
        {
          file = "den.capabilities.${cap}";
          inherit value;
        }
      ];

  # ── A HALF IS A PROJECTION, NOT A VALIDATION ──
  # `typeHalf` keeps only the keys belonging to the side asked for, then types
  # those. It cannot reject the others, because a capability like `tcp` narrows
  typeHalf =
    want: cap: value:
    let
      opts = sideOptions want cap;
    in
    typeAgainst opts cap (builtins.intersectAttrs opts value);

  typeLink = cap: value: typeAgainst (capOptions cap) cap value;

  # ── WHAT A CAPABILITY *IS*, AS AGAINST WHAT IT IS COMPOSED OF ──
  # The `includes` closure only. A derived capability's furnished value is
  # checked against these and not against the whole surface, because `pull`
  # TAKES an http link -- resolved by its own take against whichever carrier
  # serves the asker -- and that is not something the derivation could supply.
  ownOptions =
    cap:
    lib.foldl' (acc: c: acc // ((capabilities.${c} or { options = { }; }).options)) { } (
      lib.reverseList (lib.unique (capClosure cap))
    );

  ownLink = cap: value: typeAgainst (ownOptions cap) cap value;

  # ── WHAT A TAKER MAY CONTRIBUTE: THE CLOSURE, AS ONE MODULE ──
  #
  # `ownOptions` above folds `options` over the `includes` closure, so a giver
  # of `tcp` is checked against `carried`'s `carrier` and `name` as well as its
  # own. The taker side used to need a SECOND hand-rolled fold for the same
  # reason, because an attrset of `{ type; default; merge; }` records does not
  # inherit. A declaration that inherits is what `imports` is, so the closure
  # is ../base/capability-types.nix's `takerModule` and there is no fold here.
  #
  # What that fold existed for is still true and worth keeping written down:
  #
  #   den.capabilities.endpoint  declares `direction`
  #   den.capabilities.tcp       declares nothing
  #   capability.contributions <host> "tcp"   ->  [ ]   (before the fold)
  #
  # `tcp` includes `endpoint`, so `direction` is declared for every tcp take
  # and appeared in no collection. It worked anyway, but only because
  # `endpoint`'s own `rules` read `take.direction or "in"` off the RAW take --
  # so the option had a reader by accident of being read raw, and any
  # inherited taker option WITHOUT a raw reader was invisible: declared,
  # typed, documented, and unreachable.
  takerModuleFor = cap: (capabilities.${cap} or { takerModule = { }; }).takerModule;

  # ── ONE EVALUATION PER CAPABILITY FOR THE DECLARED SURFACE ──
  # The names and types a take is checked against do not depend on the taker,
  # so they are evaluated once and shared rather than re-derived per asker.
  # Lazy, so a capability nothing collects costs nothing.
  takerSurface = lib.mapAttrs (
    _: def: removeAttrs (lib.evalModules { modules = [ def.takerModule ]; }).options [ "_module" ]
  ) capabilities;

  takerOptionsOf = cap: takerSurface.${cap} or { };

  # OWN DECLARATIONS ONLY, which is the partition `judge` makes: a key this
  # capability itself accepts as a contribution is not a filter clause.
  takerOwnNames = lib.mapAttrs (
    _: def:
    lib.attrNames (removeAttrs (lib.evalModules { modules = [ def.taker ]; }).options [ "_module" ])
  ) capabilities;

  # WHICH KEYS THE GIVER READS AS ONE VALUE, over the same closure the
  # declarations follow -- an inherited contribution keeps the shape the
  # capability that declared it gave it.
  mergedNamesFor =
    cap:
    lib.concatMap (c: (capabilities.${c} or { takerMerged = [ ]; }).takerMerged) (
      lib.unique (capClosure cap)
    );

  # ── THE SIX RESOLUTION CLAUSES, DECLARED AS OPTIONS ──
  #
  # `emptyTake` was this set written out by hand, and every caller had to
  # remember to merge it in. `reachEndpoint` did not, and `attribute 'within'
  # missing` took auspc's entire evaluation down -- twice, because the second
  # `{ }` call site was one file away. A default that comes from a declaration
  # cannot be forgotten, which is ../../CLAUDE.md's rule and the whole reason
  # to put these in the module system rather than in an attrset.
  #
  # TYPED FROM WHAT THE TREE CONTAINS, measured over all 74 take sites: `name`
  # is null at 69 and a string at 5, and `carrier`, `within`, `sameHost` and
  # `where` are null at EVERY site. Four of the six clauses are declared,
  # partitioned by `judge`, defaulted, and set by nothing.
  takeClauseOptions = {
    name = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Which instance. `takes.udp = { name = \"music-assistant\"; }`.";
    };
    carrier = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Demand one named carrier. Unused in this tree.";
    };
    within = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Cap the carrier at this one's rank. Unused in this tree.";
    };
    sameHost = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = "Demand the giver be on this box, or not be. Unused in this tree.";
    };
    where = lib.mkOption {
      type = lib.types.nullOr lib.types.raw;
      default = null;
      description = "A predicate over the candidate. Unused in this tree, so `raw` until a caller says what it is.";
    };
    gives = lib.mkOption {
      type = lib.types.attrsOf lib.types.raw;
      default = { };
      description = "Nested sub-requests: the capabilities OF this one that are also wanted.";
    };
  };

  # ── AND THE WHOLE TAKE AS ONE GENERATED SUBMODULE ──
  # Clauses plus whatever this capability declares a taker may contribute. The
  # freeform type is what still lets a LINK clause through undeclared -- a
  # clause is whatever the giver's own option is, up to and including a
  # predicate (`mtu = m: m >= 1500`) -- and it is also what `judge`'s
  # hand-rolled `unknown` throw is standing in for. Narrowing this to strict is
  # the next step and a behavioural one, so it is not taken here.
  takeModuleFor = cap: {
    imports = [ (takerModuleFor cap) ];
    freeformType = lib.types.attrsOf lib.types.raw;
    options = takeClauseOptions;
  };

  # A take has two kinds of key: filter clauses matched against the giver (which
  # may be predicates -- vban.nix writes `mtu = m: m >= 1500`) and typed
  # contributions the giver reads back. So: freeform module, contributions
  # declared, clauses passing through.
  #
  # Evaluating it is what makes a declared default actually reach a reader, and
  # what makes a nested option (`carried`'s `listensOn`) work at all.
  # ../../CLAUDE.md's advice to use `or` on a raw take describes the OLD
  # behaviour and should go.
  #
  # One evalModules per (aspect, cap), ~85 here. If that ever shows up, evaluate
  # once per capability and apply the taker's values as a second module.
  rawTakeFor =
    aspect: cap:
    let
      k = takeKeyFor cap aspect;
    in
    if k == null then null else (takesOf aspect).${k};

  evaluatedTake =
    aspect: cap:
    let
      raw = rawTakeFor aspect cap;
    in
    if raw == null then
      null
    else
      (lib.evalModules {
        modules = [
          (takeModuleFor cap)
          raw
        ];
      }).config;

  # ── AND EVERY TAKER AT ONCE, FOR THE KEYS A GIVER READS AS ONE VALUE ──
  #
  # One evaluation with one definition per taker, so the option's own type does
  # the merging: `listOf` concatenates, and a type that cannot merge two
  # definitions reports the conflict with both defining files named. Restricted
  # to the merged keys because the others are deliberately per taker -- two
  # takers naming different subdomains is normal, and offering both as
  # definitions of one option would be a conflict rather than a collection.
  #
  # REVERSED, because the module system reads a definition list back to front
  # (measured) and the collected order is the takers' own.
  mergedContributions =
    cap: asked: names:
    let
      want = lib.genAttrs names (_: null);
      defs = lib.reverseList (
        lib.mapAttrsToList (name: aspect: {
          _file = "den.aspects.${name}.takes.${cap}";
          config = builtins.intersectAttrs want (lib.defaultTo { } (rawTakeFor aspect cap));
        }) asked
      );
    in
    if names == [ ] then
      { }
    else
      (lib.evalModules { modules = [ (takeModuleFor cap) ] ++ defs; }).config;

  # The shared body. `asked` is name -> aspect, whichever way the caller
  # found them.
  foldContributions =
    cap: asked:
    let
      # THE CLOSURE, not the capability's own declarations: a `tcp` take
      # carries `endpoint`'s `direction` too, which `imports` gives and an
      # attrset of option descriptions could not.
      opts = takerOptionsOf cap;
      merged = mergedNamesFor cap;

      # FROM WHATEVER TAKE MATCHED, which may be a narrowing: a service
      # declaring `takes.gated.subdomain` is contributing a `proxy` option,
      # and reading `takes.proxy` would find nothing.
      #
      # ONE evalModules PER TAKER, NOT PER TAKER x KEY. Reading the value
      # inside the per-key walk re-evaluated the whole typed take for every
      # key: `gated` has 11 options and 20 takers on secondpc, so 220
      # evalModules where 20 are needed, and `proxy` 7 x 26. Hoisting it here
      # shares the thunk across keys -- 575 evalModules to 86 over the eleven
      # capabilities that have contributions, and `nrOpUpdateValuesCopied` is
      # the statistic that found it.
      evaluated = lib.mapAttrs (_: aspect: evaluatedTake aspect cap) asked;

      # Lazy, and never forced for a capability that merges nothing.
      together = mergedContributions cap asked merged;
    in
    # ── KEYED BY TAKER, UNLESS THE CAPABILITY SAYS IT IS ONE VALUE ──
    # "who asked for this" is what a provider provisions against, so the
    # collection keeps the asker; a merged key is reduced by its own option
    # type instead.
    lib.mapAttrs (
      key: _: if lib.elem key merged then together.${key} else lib.mapAttrs (_: t: t.${key}) evaluated
    ) opts;

  # WALKS REFERENCES, reading each included capability's own `name`. An aspect's
  # `includes` holds `den.aspects.<other>` rather than a string, and
  capClosureRaw =
    cap:
    let
      def = capabilities.${cap} or null;
      parents = if def == null then [ ] else map (c: c.name) def.includes;
    in
    [ cap ] ++ lib.concatMap capClosure parents;

  # ── MEMOISED, BECAUSE `satisfies` RECOMPUTES IT FOR EVERY ASPECT ──
  # `givers` filters all 259 aspects per query, `satisfies` walks each aspect's
  # given names, and each name re-derived its whole `includes` closure from
  # scratch. One thunk per declared capability instead, shared across every
  # query -- and the recursion above goes through `capClosure`, so the inner
  # levels hit the table too. An undeclared name has no entry and falls back.
  capClosureMemo = lib.mapAttrs (name: _: capClosureRaw name) capabilities;

  capClosure = cap: capClosureMemo.${cap} or (capClosureRaw cap);

  # Does this aspect give something that SATISFIES `cap`? Either directly, or by
  # giving a narrowing of it.
  satisfies =
    ctx: aspect: cap:
    let
      given = lib.attrNames (givenBy ctx aspect);
    in
    lib.any (g: lib.elem cap (capClosure g)) given;

  # ── GIVERS BY CAPABILITY, INVERTED ONCE PER HOST RATHER THAN SEARCHED ──
  #
  # This filtered all 262 aspects on every ask, and CALLED each one's `gives`
  # to do it, then re-derived the narrowing closure of every name it found.
  # The question is the other way round: "who gives `tcp`" is a lookup, not a
  # search.
  #
  # So the map is inverted once -- every aspect walked once, each give name
  # expanded through `capClosure` (itself memoised above), and the result keyed
  # by capability. A query is then `index.${cap}`, built lazily on the first
  # ask and shared by every ask after it.
  #
  # ONE INDEX PER HOST, because `gives` takes the scope context and may read
  # the host. It does not read it for the NAMES, measured: the set of give
  # names is identical across all eleven hosts for all 262 aspects -- but one
  # index per host costs eleven traversals instead of one and needs no claim
  # about that holding for the next giver written.
  #
  # THE HOST FILTER IS NOT IN HERE. `providersFor` still applies `hasAspect` to
  # the host it was PASSED, so a class body's projected membership keeps
  # deciding its own scope; this only replaces the search for candidates.
  #
  # ── MEASURED BOTH WAYS, BECAUSE THE FIRST MEASUREMENT SAID THE OPPOSITE ──
  #
  # One query per (host, capability) -- 330 of them -- is SLOWER indexed: 3.0s
  # against 0.5s, because that asks each index for one key after paying to
  # build all thirty, while the search's `lib.any` short-circuits on the first
  # give that matches and most aspects give one or two things.
  #
  # The real workload is the other shape. `nix flake check` resolves far more
  # than thirty times per host, and there the index wins: `.#checkFindings`
  # 31.8s against 37.7s wall, 56.5s against 64.8s user, with all 20 checks
  # returning identical findings. So this is amortisation, and the sweep above
  # is its worst case rather than a counterexample.
  giversIndexFor =
    host:
    let
      ctx = { inherit host; };

      # ONE PASS AND A `groupBy`, NOT A FOLD WITH `//`. The fold version was
      # measured SLOWER than the search it replaced -- 9.9s against 7.0s --
      # because `acc // { ... }` copies the whole accumulator once per aspect,
      # which is quadratic in a map that ends up with thirty keys.
      pairs = lib.concatMap (
        aname:
        let
          aspect = den.aspects.${aname};
        in
        map (c: {
          cap = c;
          entry = lib.nameValuePair aname aspect;
        }) (lib.unique (lib.concatMap capClosure (lib.attrNames (givenBy ctx aspect))))
      ) (lib.attrNames den.aspects);
    in
    lib.mapAttrs (_: items: lib.listToAttrs (map (i: i.entry) items)) (lib.groupBy (i: i.cap) pairs);

  giversIndex = lib.mapAttrs (_: h: giversIndexFor h) hostsOf;

  # A host with no entry -- a probe's hand-built attrset, an entity not in
  # `den.hosts` -- falls back to the search, which is what it is for.
  givers =
    ctx: cap:
    let
      idx = giversIndex.${ctx.host.name or ""} or null;
    in
    if idx == null then lib.filterAttrs (_: a: satisfies ctx a cap) den.aspects else idx.${cap} or { };

  hostEntities = lib.concatMapAttrs (_: hs: hs) den.hosts;

  # ── EVERY ENTITY, CONTAINERS INCLUDED ──
  # A container is declared under its host (./container.nix) and is an entity
  # in its own right: something resolves FROM it, and other things reach it.
  # So the universe the resolver searches is the hosts and their containers.
  hostsOf =
    hostEntities // lib.concatMapAttrs (_: h: h.containers or { }) hostEntities // externalEntities;

  allHomes = lib.concatMap lib.attrValues (lib.attrValues den.homes);

  # ── MACHINES WE ONLY DIAL ──
  # A `fleet.external` record is an entity too: nothing resolves on it, so what
  # it holds is what its record's `aspects` says.
  externalEntities = lib.mapAttrs (
    n: r:
    {
      users = { };
      attachments = { };
      aliases = [ ];
      household = null;
      parent = null;
    }
    // r
    // {
      name = n;
      id_hash = "external:${n}";
      # Its accounts: any it states, and the homes that name it as their host.
      users =
        r.users or { }
        // lib.listToAttrs (
          map (h: lib.nameValuePair h.userName { inherit (h) person; }) (
            lib.filter (h: lib.elem h.hostName ([ n ] ++ r.aliases or [ ])) allHomes
          )
        );
      hasAspect = a: lib.elem (a.name or null) (map (x: x.name) (r.aspects or [ ]));
    }
  ) config.fleet.external;

  # ── WHAT AN ENTITY HAS ON ITS OWN ──
  #
  # Read off the path set each machine's production run already leaves
  # (`__pathSetByScope`, one bucket per scope's entity), never `hasAspect`,
  # which re-runs the whole pipeline per entity. A host answers for itself
  # and its users; a container is its own bucket, so a host does not "have" a
  # container's service. `id_hash` ignores the parent, hence keyed by machine.
  # The same key den's `hasAspect` looks up.
  aspectKey =
    a:
    den.lib.aspects.fx.identity.pathKey (
      if a ? name && a ? meta then den.lib.aspects.fx.identity.aspectPath a else a.__aspectChain
    );
  # `defaultTo`, not `or`: `parent` is declared, so it exists as null.
  runOf = e: lib.defaultTo e.name (lib.defaultTo (e.host.name or null) (e.parent or null));
  memberships = lib.mapAttrs (
    _: h:
    let
      ps = h.__pathSetByScope;
    in
    ps
    // (den.lib.container.pathSets.${h.name} or { })
    // {
      ${h.id_hash} = lib.foldl' (acc: u: acc // (ps.${u.id_hash} or { })) (ps.${h.id_hash} or { }) (
        lib.attrValues (h.users or { })
      );
    }
  ) hostEntities;

  # An nspawn container brings its own run's set (../base/container.nix);
  # anything else outside a run is resolved on its own.
  has =
    e: a:
    lib.defaultTo (e.hasAspect a) (
      lib.mapNullable (b: b ? ${aspectKey a}) (memberships.${runOf e}.${e.id_hash or ""} or null)
    );

  # ── WHAT EACH ENTITY HOLDS, INVERTED ONCE PER RUN ──
  # The bucket's keys mapped back to aspects, then grouped by the capability
  # each takes and gives, so every query below is a lookup rather than a
  # filter over all aspects.
  aspectByKey = lib.listToAttrs (
    lib.mapAttrsToList (n: a: lib.nameValuePair (aspectKey a) n) (
      lib.filterAttrs (_: a: builtins.isAttrs a && a ? name && a ? meta) den.aspects
    )
  );
  heldBy =
    e:
    lib.defaultTo (lib.filterAttrs (_: a: e.hasAspect a) den.aspects) (
      lib.mapNullable (
        b:
        lib.listToAttrs (
          map (k: lib.nameValuePair aspectByKey.${k} den.aspects.${aspectByKey.${k}}) (
            lib.filter (k: aspectByKey ? ${k}) (lib.attrNames b)
          )
        )
      ) (memberships.${runOf e}.${e.id_hash or ""} or null)
    );
  indexFor =
    e:
    let
      held = heldBy e;
      pairs = lib.concatMap (
        n:
        map (c: {
          inherit c;
          entry = lib.nameValuePair n held.${n};
        }) (lib.unique (lib.concatMap capClosure (lib.attrNames (takesOf held.${n}))))
      ) (lib.attrNames held);
    in
    {
      inherit held;
      takes = lib.mapAttrs (_: xs: lib.listToAttrs (map (x: x.entry) xs)) (lib.groupBy (x: x.c) pairs);
      gives = cap: lib.intersectAttrs held (givers { host = e; } cap);
    };
  entityIndex = lib.mapAttrs (_: indexFor) hostsOf;
  indexOf = e: entityIndex.${e.name or ""} or (indexFor e);
  takesAt = e: cap: (indexOf e).takes.${cap} or { };

  # WHICH PROVIDERS THIS HOST HAS. Membership is inclusion: a host that includes
  # `den.aspects.tailscale` is on the tailnet, and there is nothing to declare
  # and nothing that can disagree with reality.
  providersFor = host: (indexOf host).gives;

  # ── WHO ASKS, which the giver needs in order to provision ──
  # The inverse of `givers`, and the half a pure query layer was missing: a
  # capability resolves what a CONSUMER reads, but the PROVIDER has to write
  takesOf = aspect: aspect.takes or { };

  # ── A TAKE FOLLOWS THE NARROWING CLOSURE, EXACTLY AS A GIVE DOES ──
  # `gated` narrows `proxy`, so an aspect declaring `takes.gated` IS taking a
  # proxy -- it wants publishing AND a gate. An exact-key match would have left
  takeKeyFor =
    cap: aspect: lib.findFirst (k: lib.elem cap (capClosure k)) null (lib.attrNames (takesOf aspect));

  takers = cap: lib.filterAttrs (_: a: takeKeyFor cap a != null) den.aspects;

  # Narrowed to this host. Correct for a `reach = "host"` provider -- postgres
  # serves only its own box -- and WRONG for anything shared, which is what
  # `takersReaching` below is for.
  #
  # ON THIS MACHINE: the host and the containers it runs, each taker tagged
  # with the entity it is on. A host-reach provider serves its containers too,
  # and the one that dials back (postgres) needs to know from where.
  entitiesOn = host: [ host ] ++ lib.attrValues (host.containers or { });
  takersAt =
    host: cap:
    lib.concatMap (
      entity: lib.mapAttrsToList (name: aspect: { inherit name aspect entity; }) (takesAt entity cap)
    ) (entitiesOn host);
  takersOn =
    host: cap: lib.listToAttrs (map (t: lib.nameValuePair t.name t.aspect) (takersAt host cap));

  # ── TAKERS AS FAR AS THE GIVER REACHES ──
  # The inverse of `visibleFrom`, and it has to exist for the same reason. A
  # provider provisions for everyone it serves, and how far that is belongs to
  # Every (taker, entity) pair, for a provider that serves each one apart --
  # a credential per link is one per client host, not one per taker.
  takerEdgesReaching =
    giver: cap: from:
    lib.concatMap (
      h:
      lib.mapAttrsToList (name: aspect: {
        inherit name aspect;
        host = h;
      }) (takesAt h cap)
    ) (lib.filter (reaches from giver) (lib.attrValues hostsOf));

  takersReaching =
    giver: cap: from:
    let
      # THE SAME CONDITION `visibleFrom` AND `resolveFrom` READ. It was a
      # third copy of it, which is how the two of them came to disagree.
      candidates = lib.filter (reaches from giver) (lib.attrValues hostsOf);

      # Each entity's OWN takers: `hostsOf` already lists every container.
      found = lib.concatMap (
        h:
        lib.mapAttrsToList (name: aspect: {
          inherit name aspect;
          host = h;
        }) (takesAt h cap)
      ) candidates;
    in
    # ONE ENTRY PER TAKER, not per (taker, host): an aspect included on two
    # hosts is still one asker as far as a client registration goes, and two
    # entries would collide on the registration's name.
    lib.listToAttrs (map (x: lib.nameValuePair x.name { inherit (x) aspect host; }) found);

  # ── IS THIS CAPABILITY ROUTED? ──
  # The two resolution questions are genuinely different and conflating them
  # would have broken kanidm across hosts. A capability that narrows `address`
  isRouted = cap: lib.elem "address" (capClosure cap);

  # ── WHICH MACHINE AN ENTITY RUNS ON ──
  #
  # A container is an entity (celler2 is one: class `nixos`, its own
  # attachment), so "the same box" and "the same entity" have come apart. An
  # entity's machine is its `parent` where it has one and itself where it does
  # not -- so today, with no parents declared, this reduces to `name` and every
  # answer below is unchanged. That is deliberate: the span value needs
  # something to test before the first container entity exists, rather than the
  # two arriving in separate commits.
  # `defaultTo`, NOT `or`: `parent` is a declared option defaulting to null,
  # so it always EXISTS and `h.parent or h.name` would hand back the null.
  machineOf = h: lib.defaultTo (h.name or null) (h.parent or null);

  sameMachine = a: b: (machineOf a) != null && (machineOf a) == (machineOf b);

  # A container's or user's household is its machine's.
  householdOf = e: (hostEntities.${runOf e} or e).household.name or null;
  sameHousehold = a: b: householdOf a != null && householdOf a == householdOf b;

  # ── HOW FAR A GIVER REACHES: ONE CONDITION, READ TWICE ──
  #
  # "Does the giver on `on` serve `from`." `reach` defaults to `host`
  # (./capability-types.nix), so the safe answer is "do not reach across
  # machines" and being shared is opted into.
  #
  # IT HAS TO BE ONE FUNCTION, because it answers two questions that were
  # being answered differently: `visibleFrom` asked it as a boolean over the
  # whole fleet -- "is this giver visible at all" -- and `resolveFrom` then
  # enumerated the hosts carrying the aspect and never asked again. So a
  # `reach = "host"` giver passed the visibility gate because the ASKER has
  # it, and was then offered on every host that has it: `resolveFrom auspc
  # "udp"` returned 22 candidates, eleven of them a `loopback` claiming
  # `127.0.0.1` on somebody else's box, and `postgres`' own `takes.unix`
  # resolved on secondpc to the loopback of Ivys-MacBook-Pro.
  reaches =
    from: giver: on:
    if giver.reach == "fleet" then
      true
    else if giver.reach == "household" then
      sameHousehold from on
    # `host` MEANS THIS MACHINE, which is what it always meant: a container on
    # secondpc reaches secondpc's postgres because it runs on that box. Names
    # were the same test while every entity was its own machine; a container
    # entity (`parent`) is where they part, and this is then still the right
    # one. `linksBetween` decides HOW -- the loopback stops at one entity, so a
    # container arrives over its bridge.
    else
      sameMachine from on;

  # ── PLACEMENT RESOLUTION, for the non-routed half ──
  visibleFrom =
    host: cap:
    lib.filterAttrs (
      _: giver: lib.any (h: has h giver && reaches host giver h) (lib.attrValues hostsOf)
    ) (givers { inherit host; } cap);

  # ── THE GIVER'S OWN FURNISHING, UNTYPED, PLUS WHAT IT WAS MATCHED AT ──
  # The link this giver furnishes for `peer`, FROM `host`. An ADDRESS is a
  # property of the attachment between a host and a network -- not a fact about
  linkRaw =
    aspect: cap:
    { host, peer }:
    let
      given = givenBy { inherit host; } aspect;
      # The giver may furnish a narrowing rather than the capability asked for:
      # a want for `address` is satisfied by `tcp`. Take whichever it has.
      #
      # AN EXACT MATCH WINS, and it has to. This was `findFirst` over the
      # furnished names alone, which picks the ALPHABETICALLY FIRST key whose
      # closure contains the capability -- so a giver furnishing both `grpc`
      # and `tcp`, asked for `tcp`, got `grpc`, because `g` sorts before `t`
      # and gRPC narrows tcp. Measured on ../services/tempo.nix the moment it
      # gained a `tcp` endpoint beside its gRPC one: its vhost disappeared,
      # because the link that came back was the gRPC port under a name
      # nothing was asking for.
      #
      # Narrowing is still answered -- it is only consulted when the giver has
      # nothing exact, which is what makes `takes.address` resolvable by a
      # `tcp` giver.
      match =
        if given ? ${cap} then
          cap
        else
          lib.findFirst (g: lib.elem cap (capClosure g)) null (lib.attrNames given);
    in
    if match == null then
      null
    else
      {
        cap = match;
        # ── THE INNER LEVEL GETS THE RESOLVER TOO ──
        # `gives` is two levels: the outer takes the giver's scope, the inner
        # takes the peer being addressed. Both are attrsets of injected args,
        value = given.${match} {
          inherit peer host;
          # ── `resolve` IS FOR A PEER, `own` IS FOR THIS BOX ──
          # `resolve`'s `target` is the giver AND the peer, which is right for
          # the three callers that pass a peer entity -- `resolve "user" peer`
          # asks what user THAT peer gets, and a user entity gives its own
          # `user`. It is wrong for a CARRIER: ../services/mail.nix asked
          # `resolve "tcp" den.aspects.loopback` and so bound `peer` to the
          # aspect, whose `name` is "loopback" and never a host name. That went
          # unnoticed while the loopback giver discarded `peer`; once it began
          # answering honestly, `relayHost` became null against a `types.str`
          # and secondpc stopped evaluating.
          resolve =
            c: target:
            linkFor target c {
              inherit host;
              peer = target;
            };
          own =
            c: giver:
            linkFor giver c {
              inherit host;
              peer = host;
            };
        };
      };

  # Back-compatible whole-link accessor: a CARRIER's furnishing is already a
  # complete link for the host-to-host questions (names, firewall), because
  # those need no port from a service.
  # ── `carrier` NAMES ITSELF ──
  #
  # Every giver used to write `carrier = self.name`, and the carriers wrote
  # their own name as a literal -- `carrier = "tailscale"` inside
  # `den.aspects.tailscale`, which is the same string twice with nothing
  # keeping them equal. It is derived here instead: the aspect that furnished
  # a link IS its carrier, so a giver that says nothing gets the right answer
  # and one that says something else is still believed (./capability-types.nix
  # lets a service name its own carrier, which is how `gives.user` attributes
  # an account to the service rather than to the wire).
  # ── AND SO DOES `name`, FOR THE SAME REASON ──
  #
  # Thirty-three givers wrote `name = self.name` by hand, which is the aspect
  # restating its own identity inside itself -- the spelling ./segments.nix
  # already records as able to disagree exactly once and silently. A giver IS
  # the instance it furnishes, so the instance's name defaults to the aspect's
  # and a giver naming something else is still believed: that is how a second
  # instance on one box, or an account named for the daemon rather than the
  # aspect, still says so.
  named =
    aspect: value:
    {
      carrier = aspect.name;
      name = aspect.name;
    }
    // value;

  linkFor =
    aspect: cap: args:
    let
      v = linkRaw aspect cap args;
    in
    if v == null then null else typeLink v.cap (named aspect v.value);

  # ── THE HALVES, EACH TYPED AGAINST ITS OWN SIDE ──
  carrierHalf =
    aspect: cap: args:
    let
      v = linkRaw aspect cap args;
    in
    if v == null then null else typeHalf "carrier" v.cap (named aspect v.value);

  endpointHalf =
    aspect: cap: args:
    let
      v = linkRaw aspect cap args;
    in
    if v == null then null else typeHalf "endpoint" v.cap (named aspect v.value);

  # ── WHICH CAPABILITY THE CARRIER IS ASKED FOR ──
  # Narrowing runs one way: giving `http` gives `tcp`, and giving `tcp` does NOT
  # give `http`. That is correct and it is also why a carrier cannot answer an
  carrierCapFor =
    cap:
    let
      cl = capClosure cap;
    in
    if lib.elem "tcp" cl then
      "tcp"
    else if lib.elem "udp" cl then
      "udp"
    else
      cap;

  # ── A USABLE LINK IS THE COMPOSITION OF THREE THINGS ──
  # The endpoint half from the service, the transport half from the carrier that
  # won, and the PUBLISHED NAME from the zone the serving host publishes under.
  composeLink =
    cap:
    {
      endpoint,
      carrier,
      published ? null,
    }:
    let
      e = if endpoint == null then { } else removeAttrs endpoint [ "carrier" ];
      c = if carrier == null then { } else carrier;

      # THE NAME, where the serving host publishes this service. Independent of
      # which carrier supplies the address -- that is the whole point.
      name = lib.optionalAttrs (published != null) { hostname = published; };

      # A CARRIER WITH NO ADDRESS OF ITS OWN addresses by the published name:
      # cloudflared dials OUT, so there is nothing to dial but the name, and it
      dialled =
        if (c.address or null) == null && published != null then { address = published; } else { };

      merged = e // c // name // dialled;
    in
    # ── YOU GET WHAT YOU ASKED FOR ──
    #
    # Narrowed to the REQUESTED capability's options before typing. The
    # endpoint half is typed against whatever the giver furnished, which may
    # be a NARROWING of what was asked: celler gives `http`, and a caller
    # asking for `tcp` composes that endpoint with a carrier. `protocol` is
    # http's and not tcp's, so typing the merge against `tcp` failed with
    # "The option `capability.tcp.protocol' does not exist" the first time
    # anything resolved a service's endpoint as plain tcp (../services/
    # proxy-nginx.nix, dialling an upstream).
    #
    # Dropping it is right rather than lenient: a consumer that asked for tcp
    # asked for an address and a port, and a field it did not ask for has no
    # business in the link it gets. `typeHalf` already works this way --
    # `intersectAttrs` before typing -- so this makes the whole-link path
    # agree with the half path.
    typeLink cap (builtins.intersectAttrs (capOptions cap) merged);
  # ── A GIVER'S OWN LINKS, keyed by capability ──
  # So a giver's class content can read the link it itself furnished, rather
  # than restating the port it already declared in `gives`. Read through
  # `capability.ownLink` below.
  #
  # This was once delivered as a MODULE ARGUMENT by a `den.aspects.own-links`
  # on `den.default.includes`, so a giver's content could take `{ grpc, ... }`.
  # Removed: den resolves an aspect's arguments through its own binder BEFORE
  # any aspect's `_module.args` has merged, so the argument never arrived and
  # the consumer fell through to `config._module.args.grpc`, failing with
  # `attribute 'grpc' missing`. Nothing else in this tree injects a custom
  # module arg from host content; the exports that work (`capability` itself,
  # ../base/celler/server.nix's `cellerCapability`) are all set at flake level.
  ownLinksFor =
    host:
    let
      givers = lib.filterAttrs (_: a: a.gives != null && has host a) den.aspects;

      pairs = lib.concatLists (
        lib.mapAttrsToList (
          name: aspect:
          map (cap: { inherit cap name aspect; }) (lib.attrNames (givenBy { inherit host; } aspect))
        ) givers
      );
    in
    lib.mapAttrs (
      cap: xs:
      if lib.length xs > 1 then
        throw "capability: ${
          lib.concatMapStringsSep " and " (x: x.name) xs
        } both give `${cap}` on ${host.name or "this host"}, so there is no one value to bind as the `${cap}` argument. A consumer resolving it picks by preference, but a GIVER reading its own link cannot -- declare the port in each aspect's own content instead."
      else
        let
          x = lib.head xs;
          l = endpointHalf x.aspect cap {
            inherit host;
            peer = host;
          };
        in
        if l == null then
          throw "capability: ${x.name} is listed as giving `${cap}` but furnishes no endpoint half for it."
        else if l.ports == { } then
          throw "capability: ${x.name}'s `${cap}` furnishes no port, so there is nothing for it to bind. Set `port` or `ports` in its `gives.${cap}`."
        else
          l
          // {
            # `address:port` for a LOCAL listener. A bind address is local by
            # definition, and every service here listens on loopback on purpose
            bind = "127.0.0.1:${toString (l.ports.default or (lib.head (lib.attrValues l.ports))).port}";
          }
    ) (lib.groupBy (x: x.cap) pairs);

in
{
  # ── A DECLARED PORT SHOULD SAY WHAT IT IS FOR ──
  #
  # The walk every other port question already uses: host entities, the givers
  # each host has, the endpoint half each furnishes. A REPORT rather than an
  # error, because `ports.http = 8095` coerces a bare int and has nowhere to
  # put a description -- so this is the list of numbers nobody has explained
  # yet, which is the state a hand-written firewall list leaves behind.
  den.checks.declared-ports-say-what-they-are = {
    description = "Every declared port carries a description of what it serves.";
    severity = "report";
    fix = "Write `gives.<cap>.ports.<n> = { port = N; description = \"...\"; }` instead of the bare number. A port nobody can describe is one nobody can decide to close.";
    findings =
      let
        carriesPorts = cap: lib.elem "endpoint" (capClosure cap);

        forHost =
          h:
          let
            me = hostsOf.${h.name or ""} or h;
            ctx = {
              host = me;
              peer = me;
            };
            mine = lib.filterAttrs (_: a: has me a) den.aspects;
          in
          lib.concatLists (
            lib.mapAttrsToList (
              aname: aspect:
              lib.concatLists (
                map (
                  cap:
                  let
                    half = endpointHalf aspect cap ctx;
                    ports = if half == null then { } else half.ports or { };
                  in
                  lib.mapAttrsToList (
                    pn: p: "${aname} gives `${cap}` port `${pn}` (${toString p.port}) with no description."
                  ) (lib.filterAttrs (_: p: p.meta.description == null) ports)
                ) (lib.filter carriesPorts (lib.attrNames (givenBy ctx aspect)))
              )
            ) mine
          );
      in
      lib.unique (lib.concatMap forHost checkLib.hostEntities);
  };

  # ── A CROSS-HOST EMISSION MUST NOT BIND `config` ──
  #
  # The rule comes from den's own pipe semantics, which the resolution layer is
  # being moved onto: a function-valued emission whose arguments are all bound
  # in the scope is called with the scope context, but one requiring `config`
  # becomes a config-dependent thunk -- resolved lazily in-scope, and resolved
  # EAGERLY against the source's instantiated config when it crosses a host
  # boundary via `pipe.collect` or `pipe.broadcast`.
  #
  # So one `config`-taking emission on a capability that other hosts resolve
  # turns a cheap fleet-wide gather into fleet-wide host INSTANTIATION -- the
  # thing ../../CLAUDE.md measures at several GB per host and says to serialise.
  # It would not fail; it would just quietly become unusable, which is the worst
  # version of this.
  #
  # Today the layer satisfies the rule, but by habit rather than by construction:
  # a giver's `gives` is a plain function over ENTITIES and `hasAspect` is a
  # projected lookup, which is why resolution measures ~8s for a whole host's
  # takes and needs no eval lock. This check is what turns that habit into an
  # invariant before the transport moves.
  #
  # ⚠ SCOPE, STATED HONESTLY: this covers the CAPABILITY half -- a `gives` on an
  # aspect whose `reach` lets another host resolve it. It does NOT yet cover
  # quirk emissions, because whether a quirk crosses a host is decided by the
  # PIPE (`collect`/`collectAll`/`broadcast`), not by `reach`, so the question
  # there is "which routes move fleet-wide" and that needs the pipe declarations
  # rather than the aspect. Worth adding when the transport lands, since that is
  # exactly when it starts to matter.
  den.checks.cross-host-emissions-are-entity-only = {
    description = "No capability a remote host can resolve furnishes its value from a function binding `config`.";
    fix = "Read the fact off the host ENTITY (`den.hosts.<sys>.<n>`) instead of its rendered config. If it genuinely only exists after instantiation, it is not a fact another host may resolve.";
    findings =
      let
        # `builtins.isFunction`, NOT `lib.isFunction`: the lib version is also
        # true for a FUNCTOR (an attrset carrying `__functor`), which den uses
        # to wrap declarations, and `builtins.functionArgs` rejects one --
        # measured, as `error: 'functionArgs' requires a function`. A functor's
        # own `__functor` takes `self` and then the real arguments, so its
        # parameter names are not the emission's either; a functor-valued
        # `gives` is therefore not inspectable this way and is skipped rather
        # than guessed at.
        bindsConfig = f: builtins.isFunction f && (builtins.functionArgs f) ? config;

        # `reach` decides whether anybody off this box resolves it at all:
        # `host` keeps the value local, where a config-dependent thunk is
        # resolved lazily and costs nothing extra.
        crossHost = aspect: (aspect.reach or "host") != "host";
      in
      lib.concatLists (
        lib.mapAttrsToList (
          name: aspect:
          lib.optional (crossHost aspect && bindsConfig (aspect.gives or null))
            "den.aspects.${name}: `gives` binds `config` and `reach = \"${aspect.reach or "host"}\"`, so another host resolving it would instantiate this host's configuration to read the value."
        ) den.aspects
      );
  };

  den.lib.capability = rec {
    inherit
      providersFor
      ownLinksFor
      heldBy
      householdOf
      sameHousehold
      hostsOf
      ;

    # A giver reading the link it itself furnished, by name -- the supported
    # route, and the one ../services/tempo.nix takes. See `ownLinksFor` above
    # for the module-argument version that used to exist and why it does not.
    #
    # `ownLinkFor`, NOT `ownLink`: this set is `rec`, and the enclosing `let`
    # already binds `ownLink` (the typing helper at the top of this file) which
    # `linkFor` calls below. Naming this one `ownLink` shadowed that for every
    # reference inside this set, so `ownLink cap (v // ...)` started calling
    # THIS function with a capability name as its `host` and failed with
    # `expected a set but found a string: "pull"`.
    ownLinkFor =
      host: cap:
      let
        links = ownLinksFor host;
      in
      links.${cap}
        or (throw "capability: nothing on ${host.name or "this host"} gives `${cap}`, so there is no own link to read. Givers here: ${lib.concatStringsSep ", " (lib.attrNames links)}.");

    # ── SEARCHING FOR A NAME ──
    # Every name every carrier this host has furnishes for `peer`. This is the
    # query the tree currently has four hand-rolled answers to, each in its own
    namesFor =
      host: peer:
      lib.unique (
        lib.concatMap (
          aspect:
          let
            l = linkFor aspect "hostname" { inherit host peer; };
          in
          if l == null then [ ] else [ l.hostname ] ++ l.aliases
        ) (lib.attrValues (providersFor host "hostname"))
      );

    # ── SEARCHING FOR A LINK: EVERY CANDIDATE, IN ORDER ──
    # The whole candidate list rather than one answer, because §2.7a's split
    # holds: eval time knows the SHAPE of the decision and run time knows what
    linksBetween =
      cap: hostA: hostB:
      let
        a = providersFor hostA cap;
        b = providersFor hostB cap;

        # ── MEMBERSHIP, PER THE CARRIER'S OWN RULE ──
        # A segment needs both ends attached; a PUBLISHED route needs only the
        # serving one. Requiring both for a published zone makes the public
        #
        # ── AND MEMBERSHIP IS BY CARRIER NAME, NOT BY SEGMENT ──
        #
        # `a ? ${n}` asks whether host A has a carrier with the SAME ATTRIBUTE
        # NAME. Two different carriers onto one physical wire are therefore not
        # siblings, even when both furnish the same `segment` prefix -- the
        # prefixes are never compared, here or anywhere: grep `prefix` and the
        # only consumer is the `segment` capability's own typed option.
        #
        # Measured when ../network/house-wifi.nix was added: the AP is BRIDGED
        # onto the house lan, so a wifi client is physically on
        # `192.168.0.0/16` and can reach a cabled host -- but `linksBetween
        # "tcp" auspc secondpc` returns `lan-home`, `tailscale` and
        # `publish-ivymect-in` and NOT `house-wifi`, because secondpc has no
        # `house-wifi`. auspc is unaffected (it has the cable too, and
        # everything resolves over that); a WIFI-ONLY host would not be a
        # sibling of a cabled one at all.
        #
        # Lifting it is TWO changes and doing one alone is worse than doing
        # neither:
        #
        #   1. this filter has to admit a carrier whose segment prefix matches
        #      one of host A's, not just one sharing its name; and
        #   2. the ADDRESS model has to follow, because an address is a fact
        #      about a host ON A SEGMENT and not about a host on a carrier.
        #      ../network/house-wifi.nix reads `peer.attachments.house-wifi
        #      .address`, which is null for a cabled peer -- so matching by
        #      segment alone would make a wifi-only host find the link and then
        #      resolve a null address, which is the silent half of the failure
        #      rather than the loud one.
        #
        # The honest shape is an attachment to a SEGMENT carrying the address,
        # with the interface staying per carrier. Not done here: nothing in the
        # fleet is wifi-only today except ../hosts/birdsarentreal.nix, which
        # does not take this carrier.
        shared = lib.filterAttrs (n: asp: asp.attachment == "provider" || a ? ${n}) b;

        # The loopback stops at one ENTITY; "on this box" is the machine.
        sameEntity = hostA.name == hostB.name;
        sameHost = sameMachine hostA hostB;

        # A LOOPBACK IS NOT A SEGMENT, and this is now a capability question
        # rather than a probe for a field. Every host has a loopback and no two
        # ── A PUBLISHED ROUTE IS NOT A SEGMENT, AND MUST NOT NEED ONE ──
        #
        # `shared` above already admits a provider-attached carrier for the
        # reason ../network/cloudflare.nix states: "a client reaching
        # `cache.ivymect.in` from a cafe includes nothing and needs to". This
        # filter then threw it away again, because a zone gives `dns` and
        # `http` and never `segment` -- so `attachment = "provider"` bought
        # membership in one line and lost it in the next.
        #
        # Measured before the fix: `reachEndpoint "http"` from the laptop to
        # secondpc's celler returned ONE candidate, the tailnet, with
        # `cache.ivymect.in` attached as a hostname that nothing could dial.
        # The public https route was unreachable through resolution, which is
        # why ./celler/push.nix carries its own throw and why that name is
        # excluded from the tunnel by hand.
        #
        # The ORDER still does the work: a zone sits at `preference = 60`,
        # last, so it only wins when the two ends share no wire at all.
        usable =
          if sameEntity then
            shared
          else
            # ── ASKED OF A DECLARED VALUE, NOT OF A MISSING GIVE ──
            # This was `satisfies ... "segment"`, which was true exactly when
            # a carrier HAD a `segment` give -- so the thing keeping a
            # loopback out of a cross-host answer was that it gave none, a
            # fact stored as an absence. Every carrier declares `spans` now
            # (./capability-types.nix) and a loopback is a segment that stops
            # at one box, so the test is the span itself.
            # AND `one-machine` IS USABLE BETWEEN TWO ENTITIES ON ONE BOX,
            # which is the clause a container entity needs: the bridge between
            # it and its host is not a wire several machines share, and it is
            # not the loopback either. Adds nothing while no `parent` is
            # declared -- `sameMachine` is then equal names, which took the
            # `sameHost` branch above.
            lib.filterAttrs (
              _: aspect:
              aspect.attachment == "provider"
              || aspect.spans == "many-hosts"
              || (aspect.spans == "one-machine" && sameMachine hostA hostB)
            ) shared;

        # PREFERENCE, NOT TRUTH. Ascending, so a loopback (0) beats a wire (20)
        # beats a tunnel (40): the ordering is depth of layering, because a
        ranked = lib.sort (x: y: x.rank < y.rank) (
          lib.mapAttrsToList (name: aspect: {
            inherit name aspect;
            rank = aspect.preference;
          }) usable
        );
      in
      map (c: {
        carrier = c.name;
        via = c.aspect;
        inherit sameHost;
        # WHAT THE CONSUMER RECEIVES, furnished by the carrier. The consumer
        # never names `lan-home` or the tailnet: it asks for `tcp` and reads an
        link = carrierHalf c.aspect cap {
          host = hostA;
          peer = hostB;
        };
      }) ranked;

    # ── REALISING A CAPABILITY, GENERICALLY ──
    # The resolver knows nothing about what any particular capability means. A
    # capability that needs more than its furnished fields -- a credential
    realise =
      cap: edge:
      let
        # ── FOLLOWS THE NARROWING CLOSURE, like everything else here ──
        # ALONG THE SURFACE -- `includes` AND `takes`. `postgres` is COMPOSED OF
        # `credential` and declares no `realise` of its own, so looking only at
        withRealise = lib.filter (c: ((capabilities.${c} or { realise = null; }).realise) != null) (
          capSurface cap
        );

        def = if withRealise == [ ] then null else capabilities.${lib.head withRealise};
      in
      if def == null then
        null
      else
        def.realise (
          edge
          // {
            inherit linkFor capClosure;

            # ── BOTH ENDS, AND WHICH HOST EACH IS ON ──
            # An edge is two aspects AND two hosts, and this carried one host.
            # That is enough for a credential whose giver runs in one place --
            # kanidm is one box -- and wrong as soon as the giver aspect serves
            # several: `celler` is one aspect on two hosts, so a realisation
            # keyed on the giver's NAME collapses two servers' credentials onto
            # one secret.
            #
            # Defaulted from `host` so the one-host callers read as before, and
            # `takerHost` is the one `host` already meant: a credential for a
            # cross-host asker is rekeyed to THAT host's key.
            giverHost = edge.giverHost or edge.host or { };
            takerHost = edge.takerHost or edge.host or { };

            # THE RESOLVED ANCESTORS, where this edge came from a chain. A
            # celler token is minted at the celler and scoped to the caches
            # below it, so the realisation needs the levels above its own.
            chain = edge.chain or { };

            # The furnished link itself, so a realisation reads the fields its
            # own capability declares without the caller unpacking them.
            link = linkFor (edge.giver or { }) cap {
              host = edge.host or { };
              peer = edge.peer or { };
            };
            resolve =
              c: aspect:
              linkFor aspect c {
                host = edge.host or { };
                peer = edge.peer or { };
              };
          }
        );

    # ── REALISING A RESOLVED CANDIDATE ──
    # The form a chain leaf is in. `resolveChain` already answered everything an
    # edge needs -- which giver won, which host it is on, and the links above it
    # -- so a caller that resolved should not have to take the answer apart and
    # hand the pieces back one at a time, which is what the two existing callers
    # do by construction because they never resolved in the first place.
    realiseOn =
      cap:
      {
        cand,
        taker,
        from,
      }:
      realise cap {
        giver = cand.via;
        giverHost = cand.host or { };
        takerHost = from;
        chain = cand.chain or { };
        inherit taker;
        host = from;
        peer = taker;
      };

    # ── A TAKE FILTERS ON CHARACTERISTICS, not just on a capability name ──
    # `takes.<cap> = { ... }` is the consumer half, and what is inside it is a
    # FILTER over what the giver furnished rather than a parameter pushed at the
    judge =
      cap: take: from: candidates:
      let
        def = capabilities.${cap} or null;
        # The whole SURFACE, not just this capability's own options: giving
        # `http` furnishes `tcp`'s and `address`'s too, so a clause on `ports`
        # is legitimate and the message must not claim otherwise.
        furnished = lib.attrNames (capOptions cap);
        # Over the closure: `http` includes `endpoint`, whose `direction` is a
        # taker option a take on `http` may state.
        contributable = lib.unique (lib.concatMap (c: takerOwnNames.${c} or [ ]) (capClosure cap));

        # The EDGE clauses, judged separately -- they are about the candidate
        # rather than about the link, so they are not compared against it.
        declared = removeAttrs take [
          "carrier"
          "sameHost"
          "where"
          "within"
          "name"
          "gives"
        ];

        # A CONTRIBUTION IS NOT A FILTER. `extensions = [ "pg_cron" ]` flows TO
        # the giver; filtering on it would compare against a key no link carries
        # and reject everything. Contributions are collected by `contributions`.
        clauses = removeAttrs declared contributable;

        # ── A FOURTH PARTITION: NESTED SUB-REQUESTS ──
        # A key naming a capability this one EXPOSES (`def.gives`) is not a
        # clause -- it is a request for that capability OF this one, which is
        exposes = exposedBy cap;
        nested = lib.filter (k: lib.elem k exposes) (lib.attrNames clauses);

        unknown = lib.filter (k: !(lib.elem k furnished) && !(lib.elem k nested)) (lib.attrNames clauses);

        # AN UNMATCHED KEY IS LOUD. Silence here is what `strict = false` gave
        # the old instance type: a removed option left `endpoints.http.class`
        # evaluating as untyped data with nothing behind it.
        checked =
          if unknown == [ ] then
            true
          else
            throw "capability: `takes.${cap}` has ${
              if lib.length unknown == 1 then "a clause" else "clauses"
            } ${
              lib.concatMapStringsSep ", " (k: "`${k}`") unknown
            } that ${cap} neither furnishes nor accepts as a contribution. It furnishes ${lib.concatStringsSep ", " furnished}; it accepts ${
              if contributable == [ ] then "nothing" else lib.concatStringsSep ", " contributable
            }.${
              lib.optionalString (exposes != [ ])
                " It exposes ${lib.concatStringsSep ", " exposes}, so a key by one of those names would be a nested request rather than a clause."
            }";

        clauseVerdict =
          cand: k:
          let
            want = take.${k};
            got = cand.link.${k} or null;
            shown =
              v:
              (
                {
                  null = _: "unbounded";
                  bool = lib.boolToString;
                }
                .${builtins.typeOf v} or toString
              )
                v;
          in
          if lib.isFunction want then
            if want got then null else "${k} is ${shown got}, which this flow refuses"
          else if want == got then
            null
          else
            "${k} is ${shown got}, want ${shown want}";

        # ── HOW FAR AWAY A CANDIDATE IS ──
        # NULL, NOT ABSENT. The three declared clauses always exist now that
        # ./capability-types.nix types them on the aspect, so "no opinion" is a
        distanceOf =
          cand:
          let
            h = cand.host or null;
          in
          if cand.sameHost or false then
            "host"
          else if h != null && sameHousehold from h then
            "household"
          else
            "fleet";

        # Ascending, so `within = "household"` admits a host-local candidate
        # too: a nearer provider is always acceptable to a consumer willing to
        # go further.
        rank =
          d:
          if d == "host" then
            0
          else if d == "household" then
            1
          else
            2;

        edgeVerdict =
          cand:
          lib.filter (x: x != null) [
            (
              let
                d = distanceOf cand;
              in
              if take.within == null || rank d <= rank take.within then
                null
              else
                "is ${d}-distant, and this take resolves no further than ${take.within}"
            )
            (
              if take.carrier == null || lib.elem cand.carrier (lib.toList take.carrier) then
                null
              else
                "carrier is ${cand.carrier}, want ${lib.concatStringsSep " or " (lib.toList take.carrier)}"
            )
            (
              if take.sameHost == null || cand.sameHost == take.sameHost then
                null
              else if take.sameHost then
                "crosses hosts, and this flow requires the same host"
              else
                "is the same host, and this flow requires a different one"
            )
            # ── WHICH INSTANCE, FALLING BACK TO WHERE IT IS ──
            # An instance with no furnished name is identified by its host, and
            # for a service that is not a shortcut: "a celler server is named
            # after its host" is a fact ./celler/push.nix already relies on. The
            # giver cannot furnish it itself -- its own `gives` is evaluated in
            # the ASKER's scope, so `host` there is the wrong host.
            (
              let
                got = if (cand.link.name or null) != null then cand.link.name else (cand.host.name or null);
              in
              if take.name == null || got == take.name then
                null
              else
                "is named ${if got == null then "<unnamed>" else got}, want ${take.name}"
            )
            (if take.where == null || take.where cand then null else "rejected by `where`")
          ];
        # ── THE CAPABILITY'S OWN RULES, beside the take's clauses ──
        # Facts that follow from what the two halves ARE, which no flow should
        # restate: a carrier that caps request bodies cannot take a flow that
        ruleVerdicts =
          cand:
          let
            rules = lib.concatMap (c: (capabilities.${c} or { rules = [ ]; }).rules) (capSurface cap);
          in
          lib.filter (x: x != null) (
            map (
              r:
              r {
                endpoint = cand.link;
                carrier = cand.link;
                inherit take;
              }
            ) rules
          );
      in
      map (
        cand:
        let
          why =
            lib.optionals checked (edgeVerdict cand)
            # NESTED KEYS ARE NOT CLAUSES. A key naming a capability this one
            # EXPOSES is a sub-request resolved by `resolveChain`, not a field
            # to compare against this level's link.
            ++ lib.filter (x: x != null) (map (clauseVerdict cand) (lib.attrNames (removeAttrs clauses nested)))
            ++ ruleVerdicts cand;
        in
        {
          inherit cand why;
          ok = why == [ ];
        }
      ) candidates;

    # `from` is the ASKING host, which `judge` needs for `within` -- how far a
    # candidate is depends on who is asking, so it cannot be read off the
    # candidate alone.
    filterCandidates =
      cap: take: from: candidates:
      map (v: v.cand) (lib.filter (v: v.ok) (judge cap take from candidates));

    # THE TABLE, for a throw. One line per carrier, in preference order, saying
    # what it is and why it did or did not qualify -- which is the message
    # ../services/vban.nix's comment contains for exactly one case.
    explainTake =
      cap: take: from: candidates:
      lib.concatMapStringsSep "\n" (
        v: "  ${v.cand.carrier}: ${if v.ok then "OK" else lib.concatStringsSep "; " v.why}"
      ) (judge cap take from candidates);

    # ── COLLECTING WHAT THE TAKERS CONTRIBUTED ──    # ── COLLECTING WHAT THE TAKERS CONTRIBUTED ──
    # The provider side of a take, and the thing the postgres quirk does by
    # hand: fold every taker's contribution into one value per key, by the
    contributions = host: cap: foldContributions cap (takersOn host cap);

    # ── THE SAME FOLD, AS FAR AS THE GIVER REACHES ──
    #
    # `contributions` asks `takersOn host`, which means "takers whose aspect is
    # on THIS host". That is right for a capability served locally -- postgres
    # hands databases to the things beside it -- and WRONG for one with
    # `reach = "household"`, where the whole point is that a taker on another
    # machine resolves here. Such a taker's contribution was silently dropped:
    # the take resolved, the link resolved, and the thing the giver had to
    # CONFIGURE in order to serve it never appeared.
    #
    # Found on Home Assistant. Every `takes.home-assistant` in the tree happened
    # to sit on the same box as HA, so `takersOn` found them all and the gap was
    # invisible until ../services/openrgb.nix -- auspc's lighting daemon, which
    # HA integrates with from secondpc -- became the first taker on another
    # host. Its `extraComponents = [ "openrgb" ]` resolved and reached nobody,
    # which is exactly the failure ../services/homeassistant/core.nix describes
    # for contributions nothing read: every file looks correct and the
    # integration is simply absent.
    #
    # `giver` and not just `host`, because how far is a fact about the SERVER.
    # This is the fold over `takersReaching`, whose entries are
    # `{ aspect; host; }` rather than bare aspects -- a difference that has
    # already cost an `attribute 'aspect' missing` once.
    contributionsReaching =
      giver: cap: from:
      foldContributions cap (lib.mapAttrs (_: v: v.aspect) (takersReaching giver cap from));

    inherit
      foldContributions
      entitiesOn
      takersAt
      takerEdgesReaching
      ;

    # The names a take of this capability may contribute, its OWN declarations
    # only -- what ../framework/checks.nix asks "and who sets it".
    takerNames = cap: takerOwnNames.${cap} or [ ];

    # ── WHERE IT MUST LISTEN, DERIVED FROM WHO TAKES IT ──
    # "if loopback only wants it, then only loopback demand." A bind address is
    # not the service's own fact. It is the union of the addresses its consumers
    # dial, and resolution already answers that per pair -- so a service that
    # only anybody local wants binds loopback, and one with a consumer on
    # another box binds whatever carrier that consumer reaches it over.
    #
    # This is the same derivation `expose` wants: a port needs a firewall hole
    # exactly when some consumer is not local. `expose` is still declared, and
    # the two should meet.
    #
    # CAP-LEVEL, NOT PORT-LEVEL, because `takersReaching` is: a take names a
    # capability, not one of its ports. So every port of a capability inherits
    # its consumer set, which over-reports rather than under-reports.
    #
    # No cycle, and the reason is worth stating: this resolves CARRIER halves
    # (`linkBetween` over `carrierCapFor`), never the endpoint half that carries
    # `ports`. Deriving a port's bind from something that reads `ports` would
    # close a loop through the type.
    # ── WHERE A SERVICE'S OWN SOCKET SHOULD BIND ──
    #
    # THIS REPLACES A VERSION THAT COULD NOT BE USED. The first `bindAddressFor`
    # took `(host, cap)` and unioned the addresses that every taker of that
    # capability anywhere on the host dials. Its comment claimed to answer "one
    # address, which is what a service's own config takes" -- but a host with
    # twenty tcp givers has one answer for all of them, and on this box that
    # union is several carriers, so every service would have been told to bind
    # `0.0.0.0`. It had no consumer for as long as it existed, and the signature
    # is why: a bind address is per (giver, port), not per host.
    #
    # DERIVED FROM `expose`, which is the same question ./firewall.nix answers
    # and therefore the same resolution -- that file reads `.interface` off each
    # named carrier's half, this reads `.address`. One declaration,
    # `gives.tcp.ports.<n>.expose`, now decides three things that used to be
    # written separately and could disagree: which wires the firewall opens,
    # which interface it opens them on, and what the service binds.
    #
    # `expose = [ ]` therefore means loopback, and the LOOPBACK CARRIER is
    # asked for it (../network/segments.nix furnishes `127.0.0.1` beside `lo`)
    # rather than a literal here -- the same reason nothing else in the tree
    # writes that string any more.
    #
    # THE ENTITY IS RESOLVED HERE, not by the caller. A class body's `host`
    # carries den's PROJECTED `hasAspect`, which answers "delivered into this
    # scope" rather than "does that host have it" -- the trap that left every
    # `expose` in the tree inert until ./firewall.nix was rewritten, and which
    # ../services/shares.nix hit before it. A service calling this from its own
    # `nixos` body would hit it a third time, so it is handled on this side.
    # ── A GIVER READING ITS OWN DECLARED PORT BACK ──
    #
    # So the number is written ONCE, in `gives`, and the service's own module
    # option is set FROM it: `services.foo.port = capability.portFor host self
    # "tcp" "http"`. Thirty-odd aspects keep that agreement with a `let`
    # binding shared between `gives` and `nixos`, which works and cannot be
    # read by anything outside the file.
    #
    # THE OTHER DIRECTION CANNOT WORK, and it is worth saying why rather than
    # leaving it to be discovered: ./firewall.nix and `bindAddressFor` both
    # READ `ports`, so a giver deriving its port from rendered config would
    # close a loop through the type -- the same reason stated above `bind`.
    # A module option's DEFAULT is readable without the fixpoint, but the live
    # value is not, and the live value is the only one worth agreeing with.
    portsFor =
      host: aspect: cap:
      let
        me = hostsOf.${host.name or ""} or host;
        half = endpointHalf aspect cap {
          host = me;
          peer = me;
        };
      in
      if half == null then { } else lib.mapAttrs (_: p: p.port) (half.ports or { });

    portFor =
      host: aspect: cap: portName:
      let
        ps = portsFor host aspect cap;
      in
      ps.${portName}
        or (throw "capability: `${aspect.name or "?"}` gives no `${cap}` port called `${portName}`, so there is nothing to read back. It declares: ${
          if ps == { } then "none" else lib.concatStringsSep ", " (lib.attrNames ps)
        }.");

    # ── THE PROBE CHAIN FOR ONE RESOLVED LINK, PARENT FIRST ──
    #
    # "Can I reach this service" is not one question. It is: does this wire
    # carry anything, does THAT host answer on it, does the service answer.
    # Each is a probe about the same link, declared by whoever owns the answer
    # -- the carrier for the first two (./capability-types.nix's aspect-level
    # `probe`), the capability for the last (`den.capabilities.<c>.probe`) --
    # and the test is all of them, in that order.
    #
    # ORDER IS THE WHOLE VALUE. A runner that stops at the first failure
    # reports a failing SEGMENT as a failing segment instead of reporting four
    # services down, and never dials anything beneath a wire that is not
    # carrying. That is also why this returns a LIST and not a single command:
    # the caller short-circuits, and what failed is which element.
    #
    # EACH TAKES `{ link, ... }`, not a positional link, for the reason every
    # other declaration here does: a probe that later wants the host or the
    # peer should not change arity to get it.
    probesFor =
      cap: resolved:
      let
        carrierAspect = if resolved == null then null else resolved.carrier or null;
        carrierProbe = if carrierAspect == null then null else carrierAspect.probe or null;

        # EVERY LEVEL OF THE NARROWING, broadest first: `builder` narrows `tcp`,
        # so a `tcp` probe applies to a builder and runs before the builder's
        # own. One declaration covers every narrowing of it.
        capProbes = lib.filter (x: x != null) (
          map (c: (capabilities.${c} or { }).probe or null) (lib.reverseList (capClosure cap))
        );

        args = {
          link = if resolved == null then null else resolved.link or resolved;
          inherit resolved;
        };
      in
      if resolved == null then
        [ ]
      else
        map (f: f args) (lib.optional (carrierProbe != null) carrierProbe ++ capProbes);

    # ── RESOLUTION PUSHES TO THE FIREWALL ──
    #
    # A port needs a hole exactly when some consumer is not local, and
    # resolution already knows who those consumers are and which wire each one
    # reaches this box over. That was written in the note above as something
    # `expose` and the bind "should meet", with the two still derived
    # separately: `expose` was hand-written per port, so a cross-host take that
    # resolved perfectly well still needed someone to have REMEMBERED to open
    # the wire it resolved over, and nothing said when they had not.
    #
    # So the resolved links are the source and the declaration is the
    # supplement. `exposeFor` is the union, and it is the ONE answer both
    # ./firewall.nix and `bindAddressFor` read -- which is what makes a
    # hand-written `expose = [ "lan-home" ]` and a link that resolves over
    # `lan-home` the same fact arrived at two ways, rather than two mechanisms
    # that can disagree.
    #
    # STATED EXPOSE STILL MATTERS, for every consumer the graph cannot see: an
    # ESPHome node, a phone, a TV. Those are not entities and never resolve, so
    # nothing derives their wire and someone has to say it.
    #
    # NO CYCLE, for the reason the note above already gives: this resolves
    # CARRIER halves (`linksBetween` over `carrierCapFor`) and reads `takes`,
    # never the endpoint half that carries `ports`. A derivation that consulted
    # `expose` to decide `expose` would close the loop; this one cannot.
    consumerCarriers =
      host: aspect: cap:
      let
        me = hostsOf.${host.name or ""} or host;

        # ── THE ASKER MUST RESOLVE TO *THIS* GIVER, which is the whole
        #    correction here ──
        #
        # `takersReaching` answers "who takes `tcp`", and that is not "who
        # dials this service": a take names a CAPABILITY, so every consumer of
        # `tcp` anywhere in reach came back for every tcp giver on the box.
        # Measured, before this filter: audiobookshelf's 8000 and
        # music-assistant's 8095 were opened on the lan because
        # `celler-cache-pull` on auspc takes `tcp` -- a hole attributed to a
        # service the consumer has never heard of, which is worse than no
        # derivation at all because it looks deliberate.
        #
        # So each asker is RESOLVED, with its own take, and counted only when
        # the winner is this giver on this host. `dial` reads links and ports
        # and never `expose`, so the derivation still cannot close a loop
        # through the thing it is deriving.
        #
        # STILL CAP-LEVEL WITHIN A GIVER, honestly: a take names no port, so
        # every port this giver furnishes for that capability gets the
        # carrier. That over-reports among one service's own ports -- which is
        # the narrow version of the same approximation, and the one the earlier
        # note meant.
        # ── ONE PREDICATE, NOT A LADDER ──
        # Three stacked `if`s said the same thing a filter says once, and the
        # middle one is the only interesting clause. `dial`'s `carrier` is the
        # carrier ASPECT and not its name, which is the kind of thing a ladder
        # hides and a named predicate does not: the list this returns is read
        # by ./firewall.nix as carrier NAMES.
        serves =
          d:
          d != null
          && (d.via.name or null) == (aspect.name or null)
          && (d.host.name or null) == (me.name or null);

        servedCarrier =
          v:
          let
            key = takeKeyFor cap v.aspect;
            take = if key == null then { } else v.aspect.takes.${key} or { };
            # ── RESOLVED UNDER THE TAKER'S OWN KEY, NOT `cap` ──
            # A narrowing carries clauses the parent does not: resolving a
            # `takes.share` against `tcp` fails with "`takes.tcp` has a clause
            # `acl` that tcp neither furnishes nor accepts". The taker's key is
            # also the more precise question, since it narrows `cap` by
            # construction.
            d = if key == null then null else dial key take v.host;
          in
          lib.optional (serves d) (d.carrier.name or d.carrier);

        # OFF-BOX ASKERS ONLY. A same-host consumer dials the loopback, which
        # needs no hole -- that is what makes "nothing else wants it" render as
        # a closed port rather than as a wildcard bind.
        remote = lib.filter (v: (v.host.name or null) != (me.name or null)) (
          lib.attrValues (takersReaching aspect cap me)
        );
      in
      lib.unique (lib.concatMap servedCarrier remote);

    # Every carrier this host is attached to, which is what `expose = true`
    # expands to. Read off the same `providersFor` the link search uses, so it
    # cannot name a wire the host has no attachment to.
    carriersOn =
      host: cap: lib.attrNames (providersFor (hostsOf.${host.name or ""} or host) (carrierCapFor cap));

    exposeFor =
      host: aspect: cap: portName:
      let
        me = hostsOf.${host.name or ""} or host;
        half = endpointHalf aspect cap {
          host = me;
          peer = me;
        };
        port = if half == null then null else half.ports.${portName} or null;
        stated = if port == null then [ ] else port.expose;
        expanded = lib.concatMap (c: if c == "*" then carriersOn me cap else [ c ]) stated;
      in
      lib.unique (expanded ++ consumerCarriers me aspect cap);

    bindAddressFor =
      host: aspect: cap: portName:
      let
        me = hostsOf.${host.name or ""} or host;

        half = endpointHalf aspect cap {
          host = me;
          peer = me;
        };
        port = if half == null then null else half.ports.${portName} or null;

        # THE ESCAPE HATCH, and it wins. ./capability-types.nix's `bind.address`
        # is for a socket resolution cannot work out -- a container veth, an
        # access point's own link -- and a stated one is believed.
        stated = if port == null then null else port.bind.address or null;

        addressOf =
          carrier:
          let
            a = den.aspects.${carrier} or null;
            ch =
              if a == null || !(has me a) then
                null
              else
                carrierHalf a cap {
                  host = me;
                  peer = me;
                };
          in
          if ch == null then null else ch.address or null;

        # `exposeFor`, NOT `port.expose`: the stated list plus the wires
        # consumers actually resolve this over, which is the same answer
        # ./firewall.nix opens. One derivation, so a bind and a hole cannot
        # disagree about which wires are in play.
        exposed = lib.unique (lib.filter (x: x != null) (map addressOf (exposeFor me aspect cap portName)));

        # ── THE LOOPBACK IS ALWAYS IN THE SET, WHICH IS WHY ANY EXPOSURE
        #    MEANS THE WILDCARD ──
        #
        # The obvious rule -- loopback for nothing exposed, the carrier's own
        # address for exactly one, the wildcard for several -- is WRONG for the
        # middle case, and wrong in the silent direction. A port exposed on the
        # house lan still has consumers ON THIS BOX, and `dial` hands them the
        # loopback because it is the shallowest route (../network/segments.nix's
        # `preference = 0`). So binding only `192.168.0.26` would leave Home
        # Assistant dialling `127.0.0.1:1883` at a broker that is not listening
        # there -- a connection refused that no eval can see, for a service
        # whose declaration had not changed.
        #
        # Measured, not reasoned about after the fact: this function returned
        # `192.168.0.26` for mosquitto's `mqtt` port, which is exactly that
        # breakage, and mosquitto is the next thing that would have moved onto
        # it.
        #
        # So the set is always `{ loopback } ∪ { exposed carriers }`, and a
        # socket cannot bind two specific addresses -- hence the wildcard the
        # moment anything is exposed. NOTHING IS LOST BY IT: ./firewall.nix
        # opens the port on exactly the interfaces the same `expose` names, so
        # reachability is enforced there rather than by the width of the bind.
        # The wildcard is the honest answer to "which addresses must this
        # socket answer on", not a shortcut around working it out.
        derived = if exposed == [ ] then addressOf "loopback" else "0.0.0.0";
      in
      if stated != null then stated else derived;

    # ── ENFORCING WHAT A CAPABILITY SAYS IS EXCLUSIVE ──
    # One walk for every rule any capability declares, rather than a
    # hand-written check per kind. ./capability-types.nix's `exclusive` says
    # what the resource is, how far it has to be unique and how to print it;
    # this collects the claims in that scope and reports the collisions.
    #
    # Was three port-specific functions here. A port is not special -- it is the
    # first instance of "a giver claims something no second giver may claim" --
    # and `subdomain` is the second, one scope wider.
    exclusionsIn =
      host:
      let
        hostsFor =
          scope:
          if scope == "fleet" then
            lib.attrValues hostsOf
          else if scope == "household" then
            lib.filter (h: sameHousehold host h) (lib.attrValues hostsOf)
          else
            [ host ];

        # EVERY CLAIM IN SCOPE, tagged with who made it and where, because the
        # whole value of the message is naming both claimants.
        claimsOf =
          cap: rule:
          lib.concatMap (
            h:
            lib.concatLists (
              lib.mapAttrsToList (
                name: aspect:
                let
                  half =
                    if rule.side == "carrier" then
                      carrierHalf aspect cap {
                        host = h;
                        peer = h;
                      }
                    else
                      endpointHalf aspect cap {
                        host = h;
                        peer = h;
                      };
                in
                if half == null then
                  [ ]
                else
                  map
                    (c: {
                      giver = name;
                      on = h.name or "?";
                      claim = c;
                    })
                    (
                      rule.claims half {
                        host = h;
                        inherit cap;
                        # WHERE EACH PORT BINDS, which a rule cannot work out
                        # from the giver's own fields.
                        #
                        # A FUNCTION OF THE PORT, not one address for the link.
                        # It was the latter, because the old `bindAddressFor`
                        # could not answer per port -- so Home Assistant's
                        # loopback-only 8123 and its LAN-exposed HomeKit bridge
                        # were compared as if they bound the same address, which
                        # is both too strict (two services on one port but
                        # different wires are not one socket) and dishonest
                        # about what the clash message says.
                        bindAt = portName: bindAddressFor h aspect cap portName;
                      }
                    )
              ) (providersFor h cap)
            )
          ) (hostsFor rule.scope);

        # A SHARED KEY IS ONE RESOURCE, NOT TWO CLAIMANTS. `rule.shares` says
        # which keys are held by every participant at once (a multicast group),
        # and those MERGE: no finding, and the firewall opens the union of what
        # each claimant exposed, because each giver's own `expose` is resolved
        # on its own. Checked before `overlaps`, since a shared group binds the
        # wildcard on one interface and would overlap by construction.
        merged =
          rule: a: b:
          rule.shares != null && rule.shares a.claim b.claim;

        collide =
          rule: a: b:
          rule.key a.claim == rule.key b.claim
          && !(merged rule a b)
          && (rule.overlaps == null || rule.overlaps a.claim b.claim);

        conflictsOf =
          cap: ruleName: rule:
          let
            claims = claimsOf cap rule;
            shown =
              x:
              "${x.giver}'s ${rule.describe x.claim}${lib.optionalString (rule.scope != "host") " on ${x.on}"}";
          in
          lib.concatLists (
            lib.imap0 (
              i: a:
              map (
                b:
                "capability: ${cap} ${ruleName} claimed twice${
                  lib.optionalString (rule.scope == "host") " on ${host.name or "?"}"
                } -- ${shown a} and ${shown b}"
              ) (lib.filter (b: collide rule a b) (lib.drop (i + 1) claims))
            ) claims
          );
      in
      lib.concatLists (
        lib.mapAttrsToList (
          cap: def: lib.concatLists (lib.mapAttrsToList (conflictsOf cap) def.exclusive)
        ) (lib.filterAttrs (_: def: def.exclusive != { }) capabilities)
      );

    # ── REACHING A SERVICE'S ENDPOINT, which is the composed query ──    # ── REACHING A SERVICE'S ENDPOINT, which is the composed query ──
    # "How do I reach that service's http endpoint from here." The endpoint half
    # comes from the service's own `gives`; the carrier half from whichever
    reachEndpoint =
      cap: rawTake: from: service:
      let
        # TOTAL, exactly as `dial` does it: three call sites pass `{ }` because
        # they have no flow clauses, and `judge` reads `within` off the take
        # unconditionally. ../base/firewall.nix's derived exposure passed `{ }`
        # and broke auspc's whole evaluation with `attribute 'within' missing`,
        # which no flake-level check sees because none of them force a host.
        take = emptyTake // rawTake;

        serviceAspect = service.aspect;
        serviceHost = service.host;

        # The giver's own host outside, the dialer as `peer`.
        endpoint = endpointHalf serviceAspect cap {
          host = serviceHost;
          peer = from;
        };

        # ── THE PUBLISHED NAME, FROM THE ZONE THE SERVING HOST PUBLISHES UNDER ──
        # A zone is a carrier that attaches on the provider side only
        # (../network/cloudflare.nix), so "which zone does that box publish
        zones = lib.filterAttrs (
          _: a: a.domain != null && a.attachment == "provider" && has serviceHost a
        ) den.aspects;

        published =
          if endpoint == null || (endpoint.subdomain or null) == null || zones == { } then
            null
          else
            "${endpoint.subdomain}.${(lib.head (lib.attrValues zones)).domain}";

        # The carrier is asked for the ROUTED BASE, not for `cap` -- see
        # `carrierCapFor`.
        carriers = linksBetween (carrierCapFor cap) from serviceHost;

        # EVERY WAY TO REACH IT, in preference order: loopback if it is the same
        # box, the lan if they are cabled together, the tailnet, then the public
        composed = map (
          c:
          c
          // {
            link = composeLink cap {
              inherit endpoint published;
              carrier = c.link;
            };
          }
        ) carriers;
      in
      if endpoint == null then [ ] else filterCandidates cap take from composed;

    # ── A TAKE WITH NOTHING IN IT ──
    # The clauses `judge` reads always exist on a typed take, and a sub-request
    # for a capability with one instance has no take to carry them -- so this is
    # what `gives.pull = { }` resolves with.
    # DERIVED FROM `takeClauseOptions`, not written out again. This set used to
    # be six literals here and the same six names in `judge`'s `removeAttrs`;
    # now the declaration is the only place they are listed.
    emptyTake = (lib.evalModules { modules = [ { options = takeClauseOptions; } ]; }).config;

    # ── BENEATH A CANDIDATE: ON ITS HOST, OR DERIVED FROM IT ──
    # A nested request is for ONE PARTICULAR giver's: "we consider that a cache
    # gives a scoped pull -- specifically a scoped pull that can be consumed
    # from that cache, but *that* cache". So a sub-level is not searched across
    # the fleet, it is reached through whatever won above.
    #
    # Two sources, and which applies is a fact about the capability rather than
    # a case this function picks:
    #
    #   GIVEN     -- an aspect on the winning host furnishes it. A celler
    #                carries several caches and which ones is a deployment fact,
    #                so ../../services/celler-service.nix furnishes `cache.main`
    #                and the instances are the aspects that do.
    #   DERIVED   -- nobody furnishes it, and `furnishes` on the parametric
    #                definition computes it from the chain. There is one pull per
    #                cache, so a cache gets its flows by existing.
    beneath =
      from: chain: parent: cap: take:
      let
        pHost = parent.host or { };
        ctx = {
          host = pHost;
          peer = pHost;
        };

        # ON THE WINNING HOST. A celler is named after its host, so "that
        # celler's caches" is "the caches there" -- and the parent candidate
        # carries the host, which is what stops a sub-request wandering off to
        # another provider's instances.
        givers = lib.filterAttrs (
          _: a: (a.gives or null) != null && has pHost a && (givenBy ctx a) ? ${cap}
        ) den.aspects;

        given = lib.mapAttrsToList (
          name: a:
          let
            l = linkFor a cap {
              host = from;
              peer = pHost;
            };
          in
          parent
          // {
            carrier = name;
            via = a;
            link = l;
            # ITS OWN NAME, because a giver furnished it: `main` is this cache
            # as against its siblings, which is what belongs in the path.
            label = if (l.name or null) != null then l.name else cap;
          }
        ) givers;

        # THE PARAMETRIC DEFINITION, APPLIED TO THE CHAIN. `bind` hands it the
        # resolved ancestors by capability name, so `furnishes` reads
        # `cache.name` and `celler` without this layer knowing what either is.
        def = bind chain cap;
        v = if def == null then null else def.furnishes;

        derived = lib.optional (v != null) (
          parent
          // {
            link = ownLink cap (v // { carrier = v.carrier or parent.carrier; });
            # THE CAPABILITY NAME, because a derived level has no identity of
            # its own -- `pull` inherits the cache's name, so putting that in
            # the path would repeat the segment above it.
            label = cap;
          }
        );
      in
      filterCandidates cap take from (if givers == { } then derived else given);

    # ── WALKING A NESTED TAKE, WITH THE PATH ──
    #   takes.celler = {
    #     name = "secondpc";
    #     gives.cache.main.gives.pull = { };
    #   };
    #
    # Resolve this level, then each sub-take against what the winner exposes.
    # `gives.<cap>.<instance>` keys a sub-request by the instance's name, so the
    # name is structure rather than a clause, and an EMPTY instance set means
    # every instance -- which is what makes `gives.pull = { }` read as "the pull
    # of this cache", since a cache has exactly one.
    #
    # Each leaf carries `chain` -- the resolved link at every level, keyed by
    # capability -- and `path`, `push/main/celler@secondpc`, leaf first, `/` for
    # a step and `@` for placement, as den's own `fx/identity.nix` does.
    resolveChain =
      cap: take: from:
      let
        walk =
          chain: parentPath: parent: cap: take:
          let
            # THE ROOT IS SEARCHED; EVERYTHING BELOW IT IS REACHED.
            cands = if parent == null then resolveFrom from cap take else beneath from chain parent cap take;
          in
          lib.concatMap (
            cand:
            let
              here = cand // {
                inherit cap;
                # `pull/main/celler@secondpc` -- leaf first, `/` for a step
                # and `@` for PLACEMENT, which is the serving host rather than
                # the asker. The root segment is the capability name: a celler
                # is named after its host, so naming it twice says nothing.
                path =
                  if parentPath == null then
                    "${cap}@${cand.host.name or from.name or "?"}"
                  else
                    "${cand.label or cap}/${parentPath}";
                chain = chain // {
                  ${cap} = cand.link;
                };
              };
            in
            if take.gives == { } then
              [ here ]
            else
              lib.concatLists (
                lib.mapAttrsToList (
                  subCap: instances:
                  let
                    # ── ONE RESERVED KEY, AND ONE PIECE OF SUGAR ──
                    # An instance key SETS the `name` filter, so asking for
                    # every instance needs a key that does not: `*`. An empty
                    # instance set is sugar for exactly that, which is what
                    # makes `gives.pull = { }` read as "the pull of this cache"
                    # -- and `*` is the spelling to use when every instance
                    # also has a sub-take to carry.
                    wanted = if instances == { } then { "*" = emptyTake; } else instances;
                  in
                  lib.concatLists (
                    lib.mapAttrsToList (
                      instance: subTake:
                      # `emptyTake //` so a sub-take is TOTAL: the declared
                      # clauses `judge` reads off every take are present even
                      # when the take came from somewhere untyped, which a take
                      # written out by hand -- a probe, a test -- always is.
                      walk here.chain here.path here subCap (
                        emptyTake // (if instance == "*" then subTake else subTake // { name = instance; })
                      )
                    ) wanted
                  )
                ) take.gives
              )
          ) cands;
      in
      walk { } null null cap (emptyTake // take);

    # ── RESOLVING A SERVICE TAKE, where the PROVIDER IS DISCOVERED ──
    # The non-routed half, and the asymmetry with `linksBetween` is the point.
    # A routed take already knows both ends: a service on auspc wants to reach
    resolveFrom =
      host: cap: take:
      let
        # BOUND ONCE. This was called inside the lambda as well as for the key
        # set, so it was re-evaluated per giver -- measured at ~30k function
        # calls a repeat, which a let-bound thunk shares instead.
        vis = visibleFrom host cap;

        # Likewise: `hostsOf` is an attrset built by folding `den.hosts`, and
        # its value list was rebuilt for every giver.
        everyHost = lib.attrValues hostsOf;

        candidates = lib.concatMap (
          giverName:
          let
            giver = vis.${giverName};
          in
          map (providerHost: {
            carrier = giverName;
            via = giver;
            host = providerHost;
            sameHost = sameMachine providerHost host;
            link = linkFor giver cap {
              inherit host;
              peer = host;
            };
            # ── `reaches`, AND NOT JUST `hasAspect` ──
            # `vis` already said the giver is visible from here; this says
            # WHICH of its hosts serves the asker. Without it a host-reach
            # giver was offered on every host that has the aspect.
          }) (lib.filter (h: has h giver && reaches host giver h) everyHost)
        ) (lib.attrNames vis);

        # THE LINK IS FURNISHED FOR THE ASKER, not for the provider's host --
        # which is why `peer = host` above rather than `peer = providerHost`.
        ranked = lib.sort (x: y: x.via.preference < y.via.preference) candidates;
      in
      filterCandidates cap take host ranked;

    # ── WHO, AND THEN HOW: THE TWO-STEP EVERY CONSUMER ACTUALLY WANTS ──
    #
    # `resolveFrom` answers WHICH giver, on WHICH host -- it applies `reach`,
    # the identity clauses and the preference order. The link it hands back is
    # the giver's OWN furnish, so it carries ports and no address: an address
    # is a fact about the attachment between two hosts, and the service does
    # not have one. `reachEndpoint` answers that other half.
    #
    # Every caller that wants "where do I dial this from here" needs both, and
    # until now each wrote the pair out: ./celler/push.nix's `endpointFor` is
    # this function with the capability, the giver and the host search frozen
    # for one service. Written once here, a consumer that declares
    # `takes.udp = { name = "music-assistant"; mtu = m: m >= 1500; }` gets an
    # address and a port out of the declaration it already made, and
    # ./service-reach.nix can ask the same question of every take on a host to
    # find the ones that cross machines.
    #
    # ── THE TAKE IS JUDGED IN TWO HALVES, BECAUSE IT DESCRIBES TWO THINGS ──
    #
    # `name` is the instance: "the music-assistant one". Only the GIVER
    # furnishes it, and `composeLink` overlays the carrier's half on top of the
    # endpoint's -- so by the time there is an address, the name of the thing
    # it belongs to is no longer legible. `mtu` and `maxBodyBytes` are the
    # opposite: the service has no opinion and the wire does, so they do not
    # exist until the two halves are composed.
    #
    # Judging the whole take at either stage therefore rejects everything, and
    # both failures are silent -- an empty candidate list, no reason given. So
    # identity is judged where the giver is chosen and the flow's constraints
    # where the carrier is, which is also what ./celler/push.nix does by hand:
    # `resolveChain` finds the servers with no flow clause at all, and
    # `endpointFor` applies `pullTake`/`pushTake` to the composed link.
    # ── RESOLVE A CAPABILITY TO ITS PROVIDER, WITH THE PEER NAMED ──
    #
    # `linkFor` needs the giving ASPECT, so every consumer that wanted a
    # per-consumer link named its provider: ../services/mail.nix asked
    # `linkFor den.aspects.kanidm "ldap"`. That is the thing this layer exists
    # to stop. mail wants A DIRECTORY, not kanidm -- and the day a second one
    # exists, or kanidm is renamed, the consumer is wrong rather than
    # re-resolved.
    #
    # `dial` already resolves by capability, but it binds the giver's `peer` to
    # the asking HOST, so it cannot carry anything per consumer -- and for
    # `ldap` or `oidc` half the link is exactly that (a token path minted per
    # asker, a DN template naming the asker's application). This is the missing
    # third form: WHO provides it, resolved; what it furnishes, for ME.
    #
    # `resolveFrom` and not `providersFor`, deliberately: it walks `den.hosts`
    # entities itself, where `providersFor host` asks a projected `hasAspect`
    # and comes back empty from a class body -- the trap ./firewall.nix and
    # ../services/shares.nix were both bitten by.
    linkTo =
      cap:
      { host, peer }:
      let
        candidates = resolveFrom host cap emptyTake;
        chosen = if candidates == [ ] then null else (lib.head candidates).via;

        # ── THE MESSAGE IS DERIVED, NOT WRITTEN ──
        #
        # A hand-written throw names the provider and the file it lives in, and
        # is wrong the moment either changes -- it says what was true when
        # somebody typed it. Everything worth saying here is already known to
        # this layer: which capability went unanswered, who asked, from where,
        # and which aspects in the flake could answer it.
        # `givers` is this file's own "which aspects satisfy this capability",
        # with the host as context -- the same predicate `providersFor` filters.
        able = lib.attrNames (givers { inherit host; } cap);
        asker = peer.name or "?";
        where = host.name or "?";
      in
      if chosen == null then
        throw (
          "capability: `${asker}` needs `${cap}` and nothing reachable from ${where} gives it. "
          + (
            if able == [ ] then
              "No aspect in this flake gives `${cap}` at all."
            else
              "These do: ${lib.concatStringsSep ", " able} -- include one on ${where}, or on a host ${where} can reach within that capability's `reach`."
          )
        )
      else
        linkFor chosen cap { inherit host peer; };

    dial =
      cap: rawTake: from:
      let
        # `emptyTake //` so the take is TOTAL. The clauses `judge` reads off
        # every take are present on one typed by ./capability-types.nix and
        # absent from one written out by hand -- which a take handed straight
        # to this function often is, since the point is to resolve a
        # declaration without a scope around it. `resolveChain` does this for
        # the same reason.
        take = emptyTake // rawTake;

        # WHICH INSTANCE, and nothing about the wire.
        whoTake = emptyTake // {
          inherit (take)
            name
            within
            sameHost
            where
            ;
        };

        # HOW IT IS CARRIED, and nothing about which one it is.
        howTake = take // {
          name = null;
        };

        cands = lib.concatMap (
          c:
          map (composed: composed // { giver = c; }) (
            reachEndpoint cap howTake from {
              aspect = c.via;
              host = c.host;
            }
          )
        ) (resolveFrom from cap whoTake);

        # ASCENDING DEPTH OF LAYERING, across every (giver, carrier) pair
        # rather than within one: a loopback to a second instance on this box
        # beats the lan to the first, which is the same judgement
        # `linksBetween` makes one pair at a time.
        ranked = lib.sort (x: y: x.via.preference < y.via.preference) cands;
        winner = if ranked == [ ] then null else lib.head ranked;

        l = if winner == null then null else winner.link;
        ports = if l == null then { } else l.ports or { };

        # The single port, where there is one. A service with several named
        # ports (garage's three, music-assistant's api and receiver) has no
        # "the" port, so this is null and the caller reads `ports.<name>` --
        # which is the whole reason ../base/capability-types.nix names them.
        port =
          if ports ? default then
            ports.default.port
          else if lib.length (lib.attrValues ports) == 1 then
            (lib.head (lib.attrValues ports)).port
          else
            null;
      in
      if l == null || (l.address or null) == null then
        null
      else
        {
          inherit (l) address;
          inherit ports port;
          protocol = l.protocol or null;
          # WHERE IT IS, which is the half a reachability check needs: a dial
          # that stays on this box needs no check at all.
          host = winner.giver.host;
          via = winner.giver.via;
          carrier = winner.via;
          sameHost = sameMachine winner.giver.host from;
          link = l;
        };

    # EVERY ACCEPTABLE LINK FROM `host` TO `peer`, in preference order. This is
    # the query a consumer's declaration turns into, and it is deliberately the
    resolveTake =
      cap: take: host: peer:
      filterCandidates cap take host (linksBetween cap host peer);

    inherit
      # Exported for probes: what an aspect gives in a given scope, by name.
      givenBy
      # And what an entity has ON ITS OWN, as against through a child scope.
      has
      takeKeyFor
      endpointHalf
      carrierHalf
      composeLink
      carrierCapFor
      takesOf
      takers
      takersOn
      takersReaching
      visibleFrom
      # What the diagram judges candidates with, so it cannot drift from it.
      reaches
      isRouted
      # `linkFor` and `capOptions` are exported because a consumer sometimes
      # wants one specific provider's link rather than a resolution --
      linkFor
      capOptions
      ;

    # ── THE PAIRWISE QUESTION, which is the second genuinely-new thing ──
    # Unary resolution cannot produce it: "nearest provider of tcp" gives a
    # consumer its OWN loopback, which is right same-host and points at nothing
    linkBetween =
      cap: hostA: hostB:
      let
        candidates = linksBetween cap hostA hostB;
      in
      if candidates == [ ] then null else lib.head candidates;
  };
}

# ── THE MIGRATION: `den.services` FOLDS INTO THIS FILE ──
# Every field of a `den.services` instance is either an aspect fact den already
# holds, or a query that belongs here. Nothing in that registry survives as a
