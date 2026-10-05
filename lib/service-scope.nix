{
  lib,
  genScope,
  # `host name -> household name`, for hosts that serve one. The graph needs it
  # to hang each host off ITS OWN household scope; a host that serves none hangs
  # off root, which is the honest shape rather than a shared node standing in.
  hostHouseholds ? { },
}:
# The scope graph over `den.services`, and the two queries over it: what a want
# can reach, and what two wants can reach IN COMMON.
#
# WHAT THE SCOPES DECLARE IS CAPABILITIES, NOT SERVICE NAMES. A service's
# `allProvides` (own `provides` plus everything its `includes` inherit, merged
# by the service type) names what it OFFERS -- a lan offers `tcp` and `udp`, a
# loopback offers `tcp`, `udp` and `unix` -- and each of those names is planted
# in the scope the provider's `reach` places it in. So a want for `tcp` resolves
# against whatever provides tcp rather than against a service called `tcp`.
#
# `reach` is PLACEMENT, not a filter: the declaration is planted in the scope
# its reach names, and a want is answered by walking `parent*` from the wanter.
# This is what makes the intersection below mean something: a provider is
# visible exactly from the scope it sits in and below, so a host-scoped
# loopback is unreachable from any other host BY CONSTRUCTION rather than by a
# check someone has to remember to write.
#
# `uses` IS `wants` IS A CONNECTION -- one relation at INSTANCE level, an edge
# from me to something I need carrying the configuration of that relation. Its
# dual at TYPE level is `gives` (`provides`/`allProvides`), which declares what
# may exist on what; `includes` is narrowing, and `@host` is placement. Those
# four are the whole model. A RESOLVED `uses` EDGE IS A CONNECTION, so
# ./service-link.nix's link record is the connection itself and not a view of
# one.
#
# A USES EDGE RESOLVES TO A LIST, NOT TO ONE ANSWER, and that is the change this
# file records. `mode = "visible"` + `single` was wrong twice over:
#
#   * `single` REFUSES on ambiguity by design (Neron et al. 2015 Fig. 3 rule
#     (V), §2.2 Duplicate Declarations), and two providers of `tcp` in one scope
#     is now LEGAL -- a household with a lan and a wireguard mesh has two, and
#     both are real.
#   * taking only the nearest answer throws away the farther ones, and the
#     farther ones are the whole point: a link between two services is the
#     INTERSECTION of what each can reach, so a candidate that is second-nearest
#     for one end can be the only shared one.
#
# So the query is `mode = "witnesses"`, which answers EVERY acyclic resolution
# path with the path attached. Priority is restored afterwards, and in three
# stated axes rather than one: the transport CLASS first (`transportOrder`), the
# number of `parent` hops second -- which is `labelOrder { endOfPath = -1; }`'s
# visibility order, stopping outranking continuing, so the nearest scope wins --
# and `layer` third. The order is preserved rather than collapsed: `resolveWant`
# is the head of that list and shadowing still works, while ./service-link.nix
# sees all of it.
#
# A LINK IS THE INTERSECTION of two services' candidate sets, over NETWORKS and
# not over provider keys -- see ./service-link.nix, which is where the second
# half of this model lives.
#
# THE RULE THAT KEEPS THIS ACYCLIC: built from DECLARED fields only --
# `serviceName`, `host`, `reach`, `abstract`, `allProvides`, `includes`'
# `serviceName`s, `address`, `prefer`, and the `uses` edge NAMES and their
# `via` narrowings.
# It never reads `resolved`, `identity`, `rawIdentity` or `children`.
# Declarations -> graph -> answers, never back. A graph built from derived
# fields deadlocks, which is the shape that produced `infinite recursion
# encountered` twice in this repo. It also never reads a host's evaluated NixOS
# config, which would close the loop the other way round
# (../aspects/services/connection.nix states that hazard where someone will
# look for it). `render` holds to both, deliberately: see its comment.
#
# Five modelling facts, each learned by hitting a refusal or a wrong answer:
#
#   * `P` must be a PARTIAL FUNCTION -- one parent per node. A star from `root`
#     to the hosts AND from the household to the hosts gives a host two parents
#     and is refused by name. The chain is service -> host -> household -> root.
#   * A SCOPE declares what is available in it; a service node declares nothing.
#     Publishing on the service's own node resolves NOTHING, silently, because
#     `parent*` only walks UP and a provider sharing a scope with its wanter is
#     a SIBLING. (gen-scope's own module-resolver example gets away with
#     per-node decls because there a submodule looks up names its ANCESTORS
#     declare.) This is also what makes shadowing work.
#   * PLACEMENT AND LOOKUP ORIGIN ARE DIFFERENT FACTS. `plantedIn` is `reach`;
#     `scopeOf` is the host. Conflating them hid a host's own loopback from
#     anything declared household- or fleet-wide on it.
#   * AN ABSTRACT TYPE PLANTS NOTHING. `lan` declares `tcp` so its narrowings
#     inherit it, and is placed nowhere and has no address.
#   * BEING ON A NETWORK MEANS HAVING AN END OF IT. Visibility is not
#     attachment: the household declares every host's lan attachment, so a host
#     with no cable still SEES the others' and must not be offered the network.
#
services:
let
  gs = genScope;

  nodeId = k: "svc:${k}";
  keyOf = id: lib.removePrefix "svc:" id;

  # HOUSEHOLD SCOPES ARE PER HOUSEHOLD, which they were not.
  #
  # There used to be ONE node called `household`, and every host in the fleet
  # was parented to it -- including hosts that serve no household at all. With
  # one house that accidentally behaves; with two it is simply wrong, and
  # `reach = "household"` cannot say WHICH. A single node standing for an
  # entity that can have several instances is not a graph, it is an enum
  # pretending to be one.
  #
  # So a household scope is `household:<name>`, a host hangs off ITS OWN
  # household (or off root when it serves none), and a service that is injected
  # into a household names the household rather than the word.
  householdScope = h: "household:${h}";

  # WHERE A DECLARATION IS PLANTED. `reach` is placement: how far this service
  # is visible, and nothing else.
  #
  # `reach` takes a HOUSEHOLD ENTITY where it used to take the string
  # "household" -- a reference, so the scope it names is the one that exists
  # rather than a word that happens to match a node someone hardcoded. The two
  # remaining strings are the graph's own ends and have no entity to reference:
  # "fleet" IS the root, and "host" is wherever this instance runs.
  plantedIn =
    s:
    if s.reach == "fleet" then
      "root"
    else if s.reach == "host" then
      (if s.host != null then "host:${s.host}" else "root")
    else if builtins.isAttrs s.reach then
      householdScope s.reach.name
    else
      throw "den.services.${s.serviceName}.reach: expected \"fleet\", \"host\", or a household entity, got ${builtins.typeOf s.reach}. A household-scoped service names WHICH household, because a scope standing for an entity with several instances cannot be a word.";

  # WHERE A LOOKUP STARTS, which is a DIFFERENT FACT and was conflated with
  # placement until the acceptance test caught it.
  #
  # A service looks up from where it RUNS. `reach = "fleet"` says the thing is
  # available fleet-wide; it does not say the service has forgotten which box
  # it is on. With the two conflated, a household-scoped Music Assistant on
  # secondpc could not see secondpc's own loopback -- that scope is not on
  # `root`'s or `household`'s parent chain -- so a consumer co-located with it
  # got the LAN route, which is the `localUrl` bug this model exists to kill.
  #
  # Splitting them costs nothing in the graph: the service node's only job is
  # to be the lookup origin (a service node declares nothing, see the header),
  # so its parent can be the origin while its declaration is planted at
  # `reach` by `declsIn`. `P` stays a partial function -- one parent each.
  scopeOf = s: if s.host != null then "host:${s.host}" else plantedIn s;

  # What a service offers, by capability name.
  #
  # THE FALLBACK LIVES HERE, not as a default on the instance: `allProvides`
  # defaulting to `[ serviceName ]` on the instance means one option's default
  # reads another option of the same submodule through the registry the
  # registry is still being built from, and that recursed on every service when
  # tried. Here `services` is a plain function argument, so the fallback is an
  # ordinary read.
  #
  # `or { }` rather than a bare read so this file still evaluates against a
  # service type that has not grown `provides` yet; an empty `allProvides` and
  # an absent one mean the same thing -- "declares no capabilities" -- and both
  # fall back to the service's own name, which is the pre-capability behaviour
  # every existing want depends on.
  # THE USES EDGES OF A SERVICE. `uses` IS `wants` IS a connection: one
  # relation at instance level -- an edge from me to something I need, carrying
  # the configuration of that relation. `gives` (`provides`/`allProvides`) is
  # its dual at TYPE level, and `includes` is narrowing; those three plus
  # placement are the whole model.
  #
  # `or { }` only, and NOT a fallback to the old `wants` spelling. There was a
  # shim here while the option was mid-rename; it is gone deliberately, because
  # the instance type is `strict = false` and freeform SILENTLY ACCEPTS the old
  # spelling as data. That is not hypothetical: with the option renamed and
  # ../aspects/services/celler-service.nix still writing `wants.celler.identifying`,
  # the four celler identities collapsed from `main/celler@secondpc` to a bare
  # `main` with no error anywhere. A shim that reads both spellings would have
  # hidden that instead of surfacing it.
  usesOf = s: s.uses or { };

  # WHERE AN ENTRY CAME FROM, for a message about a key nobody typed.
  #
  # Instances of a given type are declared on the GIVER (`gives.cache.main` on
  # the celler), and the levels below are IMPLIED by the type -- a cache gives
  # push and pull, so `push/main/celler@secondpc` exists without anyone writing
  # it. A diagnostic naming only the generated key is then a message about a
  # string the author has never seen.
  #
  # I CANNOT DERIVE THE PROVENANCE. Which entries were generated, and from
  # what, is a fact the cascade holds and this file's input does not -- it reads
  # declared fields and nothing about how they got declared. So this reads a
  # `givenBy` STAMP, with `or null`: provenance appears in every message the
  # moment the cascade stamps it, and degrades silently to today's wording
  # until then. That is the whole cost on my side, which is why it is in rather
  # than deferred. A key or an instance are both accepted, because I do not get
  # to choose which the cascade finds natural to stamp.
  givenByOf =
    s:
    let
      g = s.givenBy or null;
    in
    if g == null then
      null
    else if builtins.isString g then
      g
    else
      g.serviceName or null;

  # `key (serviceName@host, given by X)` -- the one renderer for a service in a
  # diagnostic, so the link's message and the narrowing's agree.
  describeKey =
    key:
    let
      s = services.${key};
      g = givenByOf s;
    in
    "${key} (${s.serviceName}${lib.optionalString (s.host != null) "@${s.host}"}${
      lib.optionalString (g != null) ", implied by ${g}"
    })";

  providesOf =
    s:
    let
      p = s.allProvides or { };
    in
    if p == { } then { ${s.serviceName} = { }; } else p;

  # THE TRANSPORT PRECEDENCE, as the user stated it:
  #
  #   loopback  >  host preference  >  lan  >  tailscale  >  wireguard  >  public
  #
  # `loopback` is FIRST and a host preference cannot displace it, deliberately:
  # a same-host call that goes out and comes back acquires DNS, TLS and the
  # cloudflare tunnel as dependencies, and that is an availability bug rather
  # than a slow path -- ../aspects/services/nextcloud.nix:327 scrapes
  # `http://127.0.0.1` for exactly this reason, and
  # ../aspects/hosts/secondpc/sso.nix:385 records oauth2-proxy's startup fetch
  # going through nginx because its issuer URL was the public one.
  #
  # `public` is the last band and nothing in the tree narrows to it yet; a
  # cloudflare tunnel wanting to rank there needs `includes = [ ...public ]`.
  transportOrder = [
    "loopback"
    "lan"
    "tailscale"
    "wireguard"
    "public"
  ];

  indexOf = xs: x: lib.lists.findFirstIndex (y: y == x) null xs;

  # THE CLASS OF A CONNECTION IS WHAT IT NARROWS, NOT WHAT IT IS CALLED. A
  # concrete instance is named after the NETWORK -- `ivymectin@secondpc`,
  # `lan-home@secondpc` -- so its own `serviceName` is a network name and says
  # nothing about the transport. The transport is the abstract narrowing it
  # `includes` (../aspects/services/connection.nix: loopback / lan / tailscale),
  # so the class is the first name among this instance's own and its includes'
  # that the precedence knows.
  #
  # One level of `includes`, not the transitive closure: `allProvides` is
  # transitive but no transitive include LIST is published, and the declared
  # hierarchy is exactly two deep (instance -> narrowing -> connection). A
  # three-deep narrowing would rank as unknown rather than wrongly.
  classOf =
    s:
    let
      names = [ s.serviceName ] ++ map (p: p.serviceName or "") (s.includes or [ ]);
    in
    lib.findFirst (n: lib.elem n transportOrder) null names;

  # THE ADDRESS IS AN EDGE ATTRIBUTE, AND IT INHERITS FROM THE TRANSPORT.
  #
  # `address` is a fact about an ATTACHMENT -- not about the host and not about
  # the network, but about this host's end of that network -- so it belongs on
  # the concrete entry, which under the edge reading IS the attachment. Own
  # value first, therefore.
  #
  # The fallback up the `includes` chain is what makes a loopback need no
  # declaration per host. A self-edge's address is `127.0.0.1` BY DEFINITION,
  # and the place that definition belongs is the abstract `loopback` type that
  # defines loopback -- declared once, inherited by all eleven derived edges,
  # and still overridable per edge for a host that wants `::1` or something
  # else. The alternative was for this file to know the literal, which is a
  # resolver inventing an IP address: wrong in a way you discover late, and
  # v4-only by fiat in the one file with no business holding that policy.
  #
  # One level, the same bound and for the same reason as `classOf`.
  addressOf =
    s:
    if (s.address or null) != null then
      s.address
    else
      lib.findFirst (a: a != null) null (map (p: p.address or null) (s.includes or [ ]));

  # 0 for loopback; the 10s for a declared host preference; the 100s for the
  # standing order; 1000 for a class the precedence does not name, which ranks
  # last rather than throwing -- a connection nobody has classified is still
  # usable, just never preferred.
  classRank =
    prefer: class:
    if class == "loopback" then
      0
    else
      let
        p = indexOf prefer class;
        q = indexOf transportOrder class;
      in
      if p != null then
        10 + p
      else if q != null then
        100 + q
      else
        1000;

  # capability -> [ declaration key ], per scope. A LIST, because two providers
  # of one capability in one scope is legal (see the header) -- and because
  # making it a list is what keeps the legality a property of the data rather
  # than of a check: there is no shape here that can express an ambiguity, so
  # nothing downstream has to refuse one.
  #
  # Order within a scope is `attrNames services`, i.e. codepoint order on the
  # DECLARATION key. Deterministic and derived from declared input; it is a
  # tie-break and not a priority, since everything in one scope is equally near.
  declsIn =
    scope:
    lib.foldl'
      (
        acc: key:
        lib.foldl' (acc': cap: acc' // { ${cap} = (acc'.${cap} or [ ]) ++ [ key ]; }) acc (
          lib.attrNames (providesOf services.${key})
        )
      )
      { }
      (
        lib.attrNames (
          # AN ABSTRACT TYPE PLANTS NOTHING. `lan` and `loopback` declare `tcp` so
          # that their narrowings inherit it through `allProvides`, but neither is
          # placed anywhere and neither has an address, so a want resolving to one
          # would be a connection to nowhere. ../aspects/services/connection.nix
          # records the gap and names this predicate as the fix; it cannot be
          # fixed by emptying `provides` there, because that is the channel the
          # inheritance travels on.
          lib.filterAttrs (_: s: !(s.abstract or false) && plantedIn s == scope) services
        )
      );

  hostIds = map (h: "host:${h}") (
    lib.unique (lib.filter (h: h != null) (lib.mapAttrsToList (_: s: s.host) services))
  );

  householdIds = lib.unique (map householdScope (lib.attrValues hostHouseholds));

  scopeNodes = [ "root" ] ++ householdIds ++ hostIds;

  # `P` MUST BE A PARTIAL FUNCTION -- one parent per node (Néron 2015 §2.2). A
  # host hangs off its own household, or off root when it serves none; a
  # household hangs off root. Giving every host one shared `household` parent
  # was not merely imprecise, it parented hosts to a scope they are not in.
  parentOf =
    lib.genAttrs householdIds (_: "root")
    // lib.listToAttrs (
      map (
        hid:
        let
          h = lib.removePrefix "host:" hid;
        in
        lib.nameValuePair hid (if hostHouseholds ? ${h} then householdScope hostHouseholds.${h} else "root")
      ) hostIds
    );

  parentGraph = gs.overlays (
    lib.mapAttrsToList (
      from: to:
      gs.edge {
        inherit from to;
      }
    ) parentOf
    ++ lib.mapAttrsToList (
      k: s:
      gs.edge {
        from = nodeId k;
        to = scopeOf s;
      }
    ) services
  );

  kinds = gs.mkKinds (
    map (gs.mkKind { }) [
      "root"
      "household"
      "host"
      "service"
    ]
  );

  scopeDecls = lib.genAttrs scopeNodes declsIn;

  roots = gs.buildRoots {
    inherit parentGraph kinds;
    decls = scopeDecls // lib.mapAttrs' (k: _: lib.nameValuePair (nodeId k) { }) services;
    types = {
      root = "root";
    }
    // lib.genAttrs householdIds (_: "household")
    // lib.genAttrs hostIds (_: "host")
    // lib.mapAttrs' (k: _: lib.nameValuePair (nodeId k) "service") services;
  };

  wf = gs.wellFormed {
    alphabet = [ "parent" ];
    expression = "parent*";
  };

  # `endOfPath = -1` makes stopping outrank continuing, so the NEAREST
  # declaration ranks first. Built standalone rather than handed to `resolve`:
  # `order` is read only by mode "visible" and gen-scope refuses it in any other
  # mode by name, so what this value is for here is its `pathPrecedes` -- Fig.
  # 1's visibility order over two paths from one origin, applied as the sort
  # comparator over the witnesses.
  order = gs.labelOrder {
    alphabet = [ "parent" ];
    layers = [ [ "parent" ] ];
    endOfPath = -1;
  };

  attributes = {
    children = _self: id: lib.filterAttrs (_: n: n.parent == id) roots.nodes;
    imports = _self: _id: [ ];
    marks = _: _: [ ];

    # Every provider of `cap` the node at `id` can reach, nearest scope first.
    #
    # `witnesses` is THE ACYCLIC-PATH LAW: one answer per path that never
    # re-enters a scope, each carrying `{ node; value; path; state; }`. With a
    # single-letter alphabet and `parent` a partial function there is exactly
    # one path upward, so the answers are already one-per-scope-on-the-chain and
    # the sort is a GUARD on that reading rather than a necessity -- it is also
    # what makes the ranking a stated law instead of a reliance on gen-scope's
    # documented-but-unguaranteed depth-first pre-order.
    candidates =
      self: id: cap:
      let
        answers =
          (gs.resolve {
            inherit wf;
            mode = "witnesses";
            dataFilter = node: node.decls.${cap} or null;
          } self id).answers;
        ranked = builtins.sort (x: y: order.pathPrecedes x.path y.path) answers;

        # A host that must PIN a transport, declared on the wanting service.
        # ATTRIBUTE-KEYED, not a list, for the reason the whole model is:
        # an attrset MERGES, so two contributors can each add a preference and
        # one key can be overridden without rewriting the rest.
        #
        #   prefer.tailscale = { };              # this class, before lan
        #   prefer.tailscale.priority = -1;      # and before another preference
        #
        # Read off the WANTER with `or { }`, which is the only channel this file
        # owns: a `prefer` on the HOST would be the right home for a host-wide
        # pin, and the host would then set it on the services it places. That
        # option is not mine to add; see the report.
        wanterHost = services.${keyOf id}.host or null;

        prefer =
          let
            p = services.${keyOf id}.prefer or { };
          in
          builtins.sort (
            x: y:
            let
              px = p.${x}.priority or 0;
              py = p.${y}.priority or 0;
            in
            if px != py then px < py else x < y
          ) (lib.attrNames p);

        candidate =
          a: key:
          let
            s = services.${key};
            params = (s.allProvides or { }).${cap} or { };
            class = classOf s;
          in
          {
            # The user's notation: capability, then the KIND that provides it,
            # then the instance -- `tcp.lan.lan-home`.
            path = "${cap}.${s.serviceName}.${key}";
            capability = cap;
            inherit key params class;
            # The NETWORK this instance attaches to. One tailnet has one name
            # and one instance per host on it, so `serviceName` is the network
            # and `host` is which end of it this instance is.
            network = s.serviceName;
            inherit (s) serviceName;
            host = s.host or null;
            address = addressOf s;
            hops = builtins.length a.path;
            scope = a.node;
            layer = params.layer or 0;
            rank = classRank prefer class;
          };
      in
      # A TRANSPORT YOU HAVE NO END OF IS NOT YOURS TO USE.
      #
      # The household declares every host's attachment to the house LAN, so a
      # host that is not cabled to it still SEES `lan-home@secondpc` and would
      # otherwise list a network it cannot route to -- and a want narrowed to
      # that network would then look satisfiable and fail only later, at the
      # link. Being on a network means having an END of it, so a classified
      # transport survives only if some instance of the same network is placed
      # on this wanter's own host.
      #
      # ONLY for classified transports (`class != null`). An ordinary service
      # is not a network and is routinely wanted across hosts -- a consumer on
      # auspc wanting postgres on secondpc is the normal case, and filtering
      # that by attachment would break every cross-host want in the registry.
      let
        cs = # THE PRECEDENCE, flat over every scope, because the transport CLASS
          # outranks distance and the scope chain cannot express that. The user's
          # order, confirmed verbatim:
          #
          #   loopback > host preference > lan > tailscale > wireguard > public
          #
          # tie-broken by least-layered. `hops` is second, not first: it separates
          # providers of one class (a nearer lan beats a farther one) and it is the
          # faithful rendering of gen-scope's visibility order, which the sort of
          # `answers` above establishes -- `endOfPath = -1` makes stopping outrank
          # continuing, so a shorter `parent*` path precedes a longer one and the
          # nearest scope has the fewest hops.
          #
          # `layer` is last of the numeric axes and separates two things of one
          # class at one distance. It is READ from the capability's params rather
          # than derived, because the thing that would derive it is the provider's
          # own underlay WANT ("a tunnel wants a connection"), and the capability
          # vocabulary that names such a want does not exist yet. When it does,
          # `layer` becomes the length of the provider's underlay chain and this
          # read goes away.
          builtins.sort
            (
              x: y:
              if x.rank != y.rank then
                x.rank < y.rank
              else if x.hops != y.hops then
                x.hops < y.hops
              else if x.layer != y.layer then
                x.layer < y.layer
              else
                x.path < y.path
            )
            (
              lib.concatMap (
                a:
                map (candidate a) (
                  # A provider that IS the wanter is dropped. A service placed in
                  # the same scope it wants from reaches its own declaration
                  # through its parent, so without this a lan wanting `tcp` answers
                  # itself.
                  lib.filter (key: nodeId key != id) a.value
                )
              ) ranked
            );
        onOwnEnd =
          c:
          c.class == null
          || wanterHost == null
          || lib.any (d: d.network == c.network && d.host != null && d.host == wanterHost) cs;
      in
      lib.filter onOwnEnd cs;
  };

  result = gs.eval { } attributes roots;

  # Pure reachability: every provider of `cap` visible from `key`, in
  # precedence order, with nothing filtered out. The diagnostic view.
  reachableFor = key: cap: result.get (nodeId key) "candidates" cap;

  # AGNOSTIC BY DEFAULT, NARROWED BY A SUB-SCOPE.
  #
  #   uses.tcp = { };                   # the capability. best candidate wins.
  #   uses.tcp.via.lan-home = { };      # narrowed to that carrier.
  #
  # Not a list and not a qualifier attribute beside the capability: each level
  # is a SCOPE and the inner one narrows the outer, which is the same shape as
  # `provides.to-users` and is consistent with the rest of the model rather
  # than being a constraint syntax of its own. It also merges -- two
  # contributors can each narrow, one key can be overridden, and each entry
  # carries its own parameters, none of which a list element can do.
  #
  # ONE mechanism, not two: a want with no sub-scope takes the whole candidate
  # set, a want with sub-scopes takes the filter of it. The agnostic case IS
  # the unconstrained filter, so there is no second code path to fall off.
  #
  # THE CARRIER NESTS UNDER THE RESERVED KEY `via`, AND IT HAS TO. The earlier
  # rule was "a sub-scope is any attribute of the edge whose value is an
  # attrset", which collides with the edge's PARAMETERS -- and not
  # hypothetically. An edge carries a contribution for its provider, and
  # ../aspects/services/postgres.nix collects exactly this shape from five
  # consumers:
  #
  #   uses.postgres = { database = "celler"; extensions = [ "pg_cron" ];
  #                     tcp = false; settings = { work_mem = "32MB"; }; };
  #
  # `settings` is an attrset, so the old rule read it as a carrier name and the
  # query threw with a message about a network called `settings`. Tested, not
  # guessed. `database` and `tcp` (scalars) and `extensions` (a list) were
  # safe; one attrset parameter was enough.
  #
  # `via` rather than a name-match against reachable networks, which was the
  # other candidate: a typo would then silently become a parameter instead of a
  # loud failure, and loudness is the whole point of a narrowing. `via` is also
  # already this tree's word for it -- ../aspects/hosts/laptop.nix:265 spells
  # it `celler-use.<server>.via = "tailscale"`.
  #
  # THE KEYSPACE HAS TWO READERS, which is why one reserved word is enough but
  # zero is not: `address`/`interface` on an edge describe MY end and are read
  # here, while `database`/`owner`/`settings` are a contribution read by the
  # provider's own collector. Nothing distinguishes them syntactically, so a
  # reserved name has to be reserved against both.
  #
  # AND IT FAILS LOUDLY. A narrowing that excludes every candidate is an
  # evaluation error, not a quiet fall-back to another carrier: the constraints
  # that motivate it are hard ones -- a full VBAN packet does not fit a
  # wireguard tunnel's 1420-byte MTU (../aspects/services/vban.nix), and
  # Cloudflare caps a proxied body at 100MB while `celler push` sends NARs far
  # past it -- so a silent substitution is a service that evaluates and then
  # breaks on the wire. Same shape as the throw already in the tree:
  # `celler-use: ${name} is not reachable via ${want.via}`.
  #
  # The cost, accepted: a pinned carrier means a service that moves off it
  # breaks. That break is an eval error rather than a runtime one, which is the
  # whole point of checking the narrowing against the candidate set.
  subScopesOf = key: cap: lib.attrNames ((usesOf services.${key}).${cap}.via or { });

  # REQUIREMENTS: THE CARRIER ADVERTISES A LIMIT, THE EDGE STATES ITS NEED.
  #
  #   provides.http.limits.bodyBytes = 104857600;   # the carrier's cap
  #   uses.http.needs.bodyBytes = null;             # this edge needs unbounded
  #
  # ONE RELATION, NO TABLE: satisfied iff `need <= limit`, with `null` meaning
  # unbounded on either side. That covers both hard constraints the tree
  # already works around by hand -- Cloudflare caps a proxied body at 100MB
  # while `celler push` sends NARs far past it, and a full VBAN packet does not
  # fit a 1420-byte wireguard tunnel MTU (../aspects/services/vban.nix:29) --
  # with ZERO declared vocabulary. The keys are open: a want naming `bodyBytes`
  # and a carrier advertising `bodyBytes` match with nothing registered
  # anywhere.
  #
  # WHY THIS IS BETTER AND NOT MERELY DIFFERENT, which is the whole argument
  # for it: §2.7 proposed TRAFFIC CLASSES, and `endpoints.http.class =
  # "bulk-upload"` is still sitting on push in ./celler-service.nix. A class is
  # an OPAQUE TOKEN -- `"bulk-upload"` means nothing until something central
  # says which carriers it may use, so the vocabulary needs a registry and
  # every new constraint needs an entry in it. A LIMIT IS SELF-DESCRIBING and
  # compares without one. So the traffic-class table does not move here, it
  # DISSOLVES, and with it the central registry it would have required.
  #
  # AN ABSENT LIMIT IS NO LIMIT. A lan declares no `bodyBytes` because a lan
  # does not cap body size, so absent reads as unbounded and push over the lan
  # is admitted. The hazard, and it is the price of having no vocabulary: a
  # MISSPELLED limit key on a carrier also reads as unbounded, and nothing can
  # catch it without the central list we are deliberately not keeping.
  #
  # WHAT THIS DELIBERATELY DOES NOT DO: categorical requirements ("must be
  # encrypted", "must not traverse the public internet"). Those are
  # set-membership, not bounds, and a second relation cannot come from this
  # file without becoming the registry by the back door. When one appears, the
  # CARRIER declares the relation alongside the value -- the same principle by
  # which ../aspects/services/postgres.nix normalises raw consumer data rather
  # than typing it.
  needsOf = key: cap: (usesOf services.${key}).${cap}.needs or { };

  meetsNeeds =
    needs: c:
    let
      limits = c.params.limits or { };
    in
    lib.all (
      k:
      let
        need = needs.${k};
        limit = limits.${k} or null;
      in
      if limit == null then
        true
      else if need == null then
        false
      else
        need <= limit
    ) (lib.attrNames needs);

  unmetBy =
    needs: c:
    let
      limits = c.params.limits or { };
    in
    lib.concatMapStringsSep ", " (
      k:
      "${k} needs ${
        if needs.${k} == null then "unbounded" else toString needs.${k}
      } but ${c.key} caps it at ${toString (limits.${k} or null)}"
    ) (lib.filter (k: !(meetsNeeds { ${k} = needs.${k}; } c)) (lib.attrNames needs));

  candidatesFor =
    key: cap:
    let
      all = reachableFor key cap;
      only = subScopesOf key cap;
      needs = needsOf key cap;

      pinned =
        if only == [ ] then all else lib.filter (c: lib.elem c.network only || lib.elem c.class only) all;
      kept = lib.filter (meetsNeeds needs) pinned;

      reachList = lib.concatMapStringsSep "; " (
        c: "${c.path} (network ${c.network}, class ${toString c.class})"
      ) all;
    in
    # Both filters fail LOUDLY, and in this order so the message names the more
    # specific fault: a pin that matches nothing is a different mistake from a
    # pin that matches something the edge cannot use.
    if all == [ ] then
      [ ]
    else if pinned == [ ] then
      throw "den.services.${key}.uses.${cap}.via -- ${describeKey key} -- narrowed to ${
        lib.concatMapStringsSep ", " (n: "`${n}`") only
      }, and nothing reachable carries it. Reachable from ${scopeOf services.${key}}: ${reachList}. Narrow to a network or a transport class that is actually there, or drop the sub-scope and take the best candidate."
    else if kept == [ ] then
      throw "den.services.${key}.uses.${cap}.needs -- ${describeKey key} -- no reachable carrier satisfies it. ${
        lib.concatMapStringsSep "; " (c: "${c.path} -- ${unmetBy needs c}") pinned
      }. A limit is advertised as `provides.${cap}.limits.<key>`, and an absent limit means no limit; raise a carrier's limit, lower the need, or declare a carrier that can take it."
    else
      kept;

  # The head of the candidate list -- the one resolution. Bound in the `let`
  # because the inbound index below needs it, and the published `resolveWant`
  # is this.
  targetOf =
    key: cap:
    let
      cs = candidatesFor key cap;
    in
    if cs == [ ] then null else (lib.head cs).key;

  # ── THE INBOUND HALF: WHO TAKES WHAT I GIVE ────────────────────────────────
  #
  # A provider's aspect has to walk its own given sub-tree AND its inbound
  # edges to emit config -- for celler, every cache, every push/pull under
  # each, and every taker of those, so it can mint one token per taker. This is
  # §3a's "one aspect instance per PARENT, reading children as data" with the
  # inbound direction added, and `../base/celler/_lib.nix` does it by hand
  # today through quirks.
  #
  # IT NEEDS NOTHING FROM ANYONE ELSE, which is the finding here. I expected to
  # have to ask for an inverse index, but the giver relation is already
  # expressible in DECLARED fields: an identifying edge IS the giving edge
  # (`cache-main.uses.celler.identifying = true` says main is given by celler),
  # and resolving it is what this file does. So the whole sub-tree and the whole
  # inbound set derive from `uses` plus the resolution already here -- no new
  # input, and no reading of `resolved`/`children`, which is what made the
  # existing `childIndex` unavailable for this (it inverts `rawIdentity`, a
  # derived field).
  #
  # `givenBy` is read FIRST where stamped, because the cascade will stamp it
  # and a stamp is cheaper and more direct than resolving an edge. This is not
  # the dead-spelling shim I removed earlier: both are LIVE representations of
  # one relation during a migration that is adding the second, and neither
  # silently accepts something retired.

  # One edge, resolved. `params` is the provider's CONTRIBUTION with the
  # reserved keys removed -- the three readers of the edge's attrset
  # (narrowing, requirements, contribution) separated explicitly, in the one
  # place that knows all three.
  edgesOf =
    key:
    let
      u = usesOf services.${key};
    in
    lib.concatMap (
      cap:
      let
        t = targetOf key cap;
      in
      lib.optional (t != null) {
        from = key;
        capability = cap;
        target = t;
        # The CONTRIBUTION, which is now a named region rather than whatever
        # was left after striking the reserved keys. The strip was a shared
        # keyspace with three readers and it collided once for real:
        # `uses.postgres = { settings.work_mem = "32MB"; }` parsed as a carrier
        # named `settings`. `giving` cannot collide with a directive because it
        # is not in the same namespace as one.
        params = u.${cap}.giving or { };
        identifying = u.${cap}.identifying or false;
      }
    ) (lib.attrNames u);

  giverOf =
    key:
    let
      g = givenByOf services.${key};
      ids = lib.filter (e: e.identifying) (edgesOf key);
    in
    if g != null then
      g
    else if ids == [ ] then
      null
    else
      (lib.head ids).target;

  # giver -> [ the things it gives ]. Acyclic for the same reason
  # `rawIdentity` is: the identifying chain IS the identity path, and a cycle
  # in it recurses in the type before reaching here.
  givenIndex = lib.foldl' (
    acc: key:
    let
      g = giverOf key;
    in
    if g == null then acc else acc // { ${g} = (acc.${g} or [ ]) ++ [ key ]; }
  ) { } (lib.attrNames services);

  givenTreeOf = key: [ key ] ++ lib.concatMap givenTreeOf (givenIndex.${key} or [ ]);

  takersOf =
    key:
    let
      tree = givenTreeOf key;
      inbound = lib.filter (
        e:
        lib.elem e.target tree
        # A thing I give is not a TAKER of me, it is part of me: the edge from
        # a cache to its celler is the giving relation written as a want. So
        # the sub-tree is excluded and what is left is the genuine consumers.
        && !(lib.elem e.from tree)
      ) (lib.concatMap edgesOf (lib.attrNames services));
    in
    lib.foldl' (acc: e: acc // { ${e.from} = (acc.${e.from} or [ ]) ++ [ e ]; }) { } inbound;

  link = import ./service-link.nix { inherit lib; } {
    inherit
      services
      candidatesFor
      scopeOf
      describeKey
      ;
  };
in
{
  inherit scopeOf;

  inherit givenTreeOf takersOf edgesOf;

  # `key -> wantName -> providing key`, or null when nothing in scope declares
  # it. The caller owns the error message, because it has the declaration site.
  #
  # The HEAD of the candidate list, so shadowing is unchanged: a host-local
  # provider still outranks a fleet one. What IS changed is that two providers
  # in one scope no longer refuse -- `single`'s ambiguity throw is gone with
  # `visible`, deliberately (header), and a caller that cares reads
  # `candidatesFor` and checks the length itself.
  resolveWant = targetOf;

  # The full answer: every provider of `cap` this want may use, best first --
  # `{ path; capability; key; network; serviceName; class; host; address;
  # scope; hops; layer; rank; params; }`, with the want's own `only`
  # constraint applied. `reachableFor` is the same list unconstrained, for a
  # diagnostic.
  #
  # Q8: the carrier IS part of the answer and anything may query it --
  # `tcp.lan.lan-home` is a key here on purpose. What a consumer may not do is
  # NAME it to obtain the connection: it declares `tcp` and reads
  # `link.endpoint`, so the same declaration and the same read survive the
  # winning carrier changing.
  inherit candidatesFor reachableFor;

  # The intersection query -- see ./service-link.nix.
  inherit (link) linkBetween;

  # What each scope ended up declaring -- for an error message that can list the
  # candidates a wanter could actually have reached. Capability names now, not
  # service names.
  declaredIn = lib.mapAttrs (_: n: lib.attrNames (builtins.removeAttrs n.decls [ "__edges" ])) (
    lib.filterAttrs (id: _: lib.elem id scopeNodes) roots.nodes
  );

  capabilitiesOf = key: lib.attrNames (providesOf services.${key});

  # A text rendering of the graph: the scope chain, what each scope declares,
  # the services planted in it, and for each want the candidates it reaches.
  #
  # IT READS DECLARED FIELDS AND CANDIDATES ONLY, never `identity`. Not an
  # oversight: a renderer that forces `identity` forces `resolved`, so it dies
  # on exactly the misconfiguration you are rendering it to diagnose, and
  # ../lib/service-dep-type.nix's header already records that interpolating
  # identity into a message `resolved` raises comes back as an unrelated
  # recursion. Identity is printed by the caller, from `den.services`, where
  # that is safe.
  render =
    let
      svcLine =
        pad: key:
        let
          s = services.${key};
          caps = lib.attrNames (providesOf s);
          wantLine =
            want:
            let
              # `reachableFor`, NOT `candidatesFor`: the constrained query
              # THROWS on a narrowing nothing carries, and a renderer that
              # throws is useless on exactly the misconfiguration you are
              # rendering to find. The narrowing is printed beside the list
              # instead, so the render shows both what was asked for and what
              # is there.
              cs = reachableFor key want;
              only = subScopesOf key want;
            in
            "${pad}      uses ${want}${
              lib.optionalString (only != [ ]) " (via ${lib.concatStringsSep "," only})"
            } -> ${
              if cs == [ ] then
                "NOTHING REACHABLE"
              else
                lib.concatMapStringsSep "  " (c: "${c.path} [${c.scope}, ${toString c.hops}]") cs
            }";
        in
        lib.concatStringsSep "\n" (
          [
            "${pad}    - ${key} (${s.serviceName}${
              if s.host == null then "" else "@${s.host}"
            }, reach=${s.reach}, looks up from ${scopeOf s}) provides ${lib.concatStringsSep "," caps}"
          ]
          ++ map wantLine (lib.attrNames (usesOf s))
        );

      scopeBlock =
        depth: scope:
        let
          pad = lib.concatStrings (lib.genList (_: "  ") depth);
          here = lib.attrNames (lib.filterAttrs (_: s: plantedIn s == scope) services);
          caps = scopeDecls.${scope};
        in
        lib.concatStringsSep "\n" (
          [
            "${pad}${scope}"
            "${pad}    declares ${
              if caps == { } then
                "nothing"
              else
                lib.concatStringsSep "  " (lib.mapAttrsToList (c: ks: "${c}=[${lib.concatStringsSep "," ks}]") caps)
            }"
          ]
          ++ map (svcLine pad) here
          ++ map (scopeBlock (depth + 1)) (lib.attrNames (lib.filterAttrs (_: p: p == scope) parentOf))
        );
    in
    scopeBlock 0 "root";
}
