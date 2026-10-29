{
  lib,
  config,
  den,
  checkLib,
  ...
}:
# ── THE GUARD den DOES NOT HAVE ──
#
# https://den.denful.dev/explanation/fleet/ permits a quirk value to depend on a
# host's evaluated NixOS config, and says what happens when one crosses a host
# boundary:
#
#   > For cross-host collection, these thunks are resolved eagerly against the
#   > source host's instantiated config
#
#   > The dependency graph of config-dependent cross-host thunks must be a DAG.
#   > If host A's thunk reads host B's config, and host B's thunk reads host A's
#   > config, Nix will hit infinite recursion.
#
# There is no automatic cycle detection and no topological ordering. The first
# cycle is therefore an `infinite recursion encountered` with no indication of
# which two hosts caused it -- and on this box a host evaluation is several GB
# and minutes long (../../CLAUDE.md), so the trace arrives late and says nothing.
#
# So this builds the graph from the DECLARATIONS and names the cycle before it
# can be hit. Two kinds of edge, because there are two transports for a
# config-dependent value off a box:
#
#   CAPABILITY  a giver's `gives` binds `config`, and its `reach` lets a host
#               other than its own resolve it. The sibling check
#               `cross-host-emissions-are-entity-only`
#               (../base/capability.nix) is meant to forbid this. It does not:
#               it tests `builtins.isFunction (aspect.gives or null)`, and
#               measured against the live tree, **of the 45 aspects with a
#               non-null `gives`, ZERO are a bare function** -- den wraps every
#               one in a functor, so that check is vacuously green and proves
#               nothing. This half therefore reads the arguments THROUGH the
#               wrapper (`argsOf` below) rather than relying on it.
#
#   QUIRK       an aspect emits a quirk as a function binding `config`, and some
#               policy declares a `collect`/`collectAll`/`broadcast` stage on
#               that quirk. Unlike `reach`, crossing is decided by the PIPE and
#               not by the aspect, which is the gap ../base/capability.nix's
#               header names as "worth adding when the transport lands".
#
# ── WHY THE GRAPH IS AN OVER-APPROXIMATION, AND WHY THAT IS THE RIGHT WAY ROUND ──
#
# An edge wants to be "host K reads a config-dependent emission sourced from
# host H". Two parts of that are not decidable from declarations:
#
#   - WHICH HOSTS RECEIVE. A pipe's `collect`/`collectAll` predicate is a
#     function of the candidate's scope context, and whether a given host has the
#     collecting policy in scope is a property of the assembled pipeline, not of
#     the policy. So every host the policy is callable for is treated as a
#     receiver.
#
#   - WHICH CONFIG ATTRIBUTE. The recursion is only real if the attribute the
#     thunk reads is itself downstream of the pooled value. `celler-caches` reads
#     `config.services.cellerd.expose`, which is NOT -- which is why the one
#     cycle in this fleet today (celler2 <-> secondpc, both running a celler
#     server) resolves rather than recursing, and the evidence is that a full
#     secondpc evaluation completes. Whether that holds for a given emission is
#     a property of its body, which this cannot read without the host fixpoint
#     this file exists to avoid.
#
# Both unknowns are resolved towards REPORTING, so a finding here means "this is
# the shape that recurses" rather than "this recurses". That is the useful
# direction for a guard whose failure mode is a diagnostic-free infinite
# recursion: `fix` below says what to do with a finding that turns out to be
# benign.
#
# Computed from entities, givers and policy declarations only -- no host's
# fixpoint -- so it costs a host evaluation nothing and answers in seconds, the
# same discipline ../framework/checks.nix's header sets out.
let
  inherit (checkLib) hostsOf probe;

  hostList = lib.attrValues hostsOf;

  # ── `builtins.isFunction`, AND THEN ONE UNWRAP ──
  #
  # NOT `lib.isFunction`: the lib version is also true for a FUNCTOR (an attrset
  # carrying `__functor`), and `builtins.functionArgs` rejects one outright --
  # measured, as `error: 'functionArgs' requires a function`. That is why
  # ../base/capability.nix's sibling check tests with `builtins.isFunction` and
  # skips functors.
  #
  # Skipping is not enough here. den WRAPS a declaration in a functor record
  # (`{ __functor = _: <the real function>; … }`, the shape
  # `policy-effects.nix`'s `pipelineOnly` builds), so a quirk emission read off
  # an aspect is frequently not a bare function and a check that skips functors
  # finds nothing at all -- which is exactly what the first version of this file
  # did: `findings = [ ]` because `quirkSources` was empty, not because there is
  # no cycle. A green check that is green for the wrong reason is worse than no
  # check, so this unwraps one level and asks the function that comes out.
  #
  # ── MEASURED, AND IT IS WHY THE SIBLING CHECK CANNOT FIRE ──
  #
  # Probed against `.#debug.config`: of the 45 aspects with a non-null `gives`,
  # **zero** are a bare function. Every one is
  # `ATTRS(__functionArgs,__functor)`, and every quirk emission is wrapped the
  # same way --
  # `den.aspects.celler-server.nixos.celler-caches` is
  # `ATTRS(_,__aspectChain,__contentValues,__functor,__providesForwarded,provides)`
  # whose `__functor` applied yields `FN(config,host)`.
  #
  # So `builtins.isFunction v && (builtins.functionArgs v) ? config` -- which is
  # what ../base/capability.nix's `cross-host-emissions-are-entity-only` tests
  # with -- is false for every giver in the tree and that check is vacuously
  # green. It is reported in ../../docs/eval-cost.md rather than fixed here,
  # because that file is being edited elsewhere.
  #
  # Two routes in, cheapest first:
  #
  #   `__functionArgs`  den records the wrapped function's parameter names on
  #                     the record. Reading them costs nothing and runs none of
  #                     the author's code.
  #   `__functor v`     the emission records carry no `__functionArgs`, so the
  #                     functor has to be applied. `v` and not `null`: den's
  #                     wrapper ignores its `self`, but a functor is entitled to
  #                     use it and the record it came from is the only correct
  #                     thing to pass.
  argsOf =
    v:
    if builtins.isFunction v then
      builtins.functionArgs v
    else if builtins.isAttrs v && builtins.isAttrs (v.__functionArgs or null) then
      v.__functionArgs
    else if builtins.isAttrs v && v ? __functor then
      let
        r = builtins.tryEval (v.__functor v);
      in
      if r.success && builtins.isFunction r.value then builtins.functionArgs r.value else { }
    else
      { };

  bindsConfig = v: (argsOf v) ? config;

  # A quirk name is not a declared option on every aspect, and den's strict
  # types throw on an undeclared read rather than returning the `or` default
  # (../../CLAUDE.md's first trap is the other half of this). So every
  # speculative read goes through here: absent and unreadable both answer null,
  # which `bindsConfig` then rejects.
  tryGet =
    a: k:
    let
      r = builtins.tryEval (if builtins.isAttrs a then a.${k} or null else null);
    in
    if r.success then r.value else null;

  # ── THE QUIRK NAMES, FROM BOTH SURFACES ──
  # `den` (the registry argument) and `config.den` (the option tree) are not
  # guaranteed to carry the same set, and a name missing from whichever one this
  # read happens to pick is a quirk the check cannot see -- the silent-empty
  # failure again. So: the union, and `lib.attrNames` of each under `or { }`.
  quirkNames = lib.unique (
    lib.attrNames (den.quirks or { }) ++ lib.attrNames (config.den.quirks or { })
  );
  policyList = lib.attrValues (den.policies or config.den.policies or { });

  # ── AN EMISSION IS USUALLY WRITTEN ON A `provides` CHILD ──
  #
  # Measured: this tree has five config-dependent quirk emissions. Four are on
  # top-level aspects (`backup`, on audiobookshelf / homeassistant / immich /
  # kanidm) and a walk over `den.aspects` finds those. The fifth is
  # `den.aspects.celler-server.provides.nixos.celler-caches` -- the only one
  # that is on two hosts, and therefore the only one that can close a cycle --
  # and a top-level walk misses it entirely.
  #
  # A provides child is itself an aspect (den's `providesChildren`), reachable
  # as `.provides.<k>`, and `hasAspect` answers for it, so the walk just has to
  # descend. Two levels, which covers `provides.<class>` and
  # `provides.<target>.<class>`; deeper nesting is not used here and the depth
  # is the thing to raise if it starts to be.
  aspectEntries =
    let
      childrenOf =
        a:
        let
          p = if builtins.isAttrs a then (a.provides or { }) else { };
          r = builtins.tryEval p;
        in
        if !r.success || !builtins.isAttrs r.value then
          { }
        else
          lib.filterAttrs (k: v: !(lib.hasPrefix "__" k) && k != "_" && builtins.isAttrs v) r.value;

      walk =
        depth: path: a:
        [
          {
            inherit path;
            aspect = a;
          }
        ]
        ++ lib.optionals (depth > 0) (
          lib.concatLists (
            lib.mapAttrsToList (k: v: walk (depth - 1) "${path}.provides.${k}" v) (childrenOf a)
          )
        );
    in
    lib.concatLists (lib.mapAttrsToList (n: a: walk 2 "den.aspects.${n}" a) den.aspects);

  # ── WHICH QUIRKS LEAVE THE BOX ──
  #
  # A policy is `{ __isPolicy; name; fn; }` (den's `policyRegistryType` wraps a
  # raw function into one), and its effects are plain data:
  # `{ __policyEffect = "pipe"; value = { pipeName; stages = [ { __pipeStage; } ]; }; }`.
  # So the crossing stages can be read straight off a call without running any
  # part of the pipeline -- which is also the only way to ask: `collect` and
  # `broadcast` are declared by the POLICY, never by the aspect that emits.
  #
  # `expose` is not here on purpose: it pushes a value UP its own scope chain and
  # never off the host, so a config-dependent emission behind one is resolved
  # in-scope and lazily, which is free.
  crossingStages = [
    "collect"
    "collectAll"
    "broadcast"
  ];

  # ── ONLY A POLICY WHOSE ARGUMENTS CAN BE SUPPLIED FOR REAL IS CALLED ──
  #
  # The first version handed every policy a context with `null` for each
  # argument it did not have a value for. That is not survivable, and the reason
  # is worth writing down because it undermines `probe` as well:
  #
  #   **`builtins.tryEval` catches `throw` and `assert`, and NOTHING ELSE.** A
  #   TYPE error is not catchable. Measured, twice, from inside a policy handed
  #   a null: `error: expected a string but found null` out of
  #   ../base/household-scope.nix's `den.hosts.${system}`, and `error: expected
  #   a set but found null`. `probe`/`tryEval` did not stop either; they took
  #   down the whole evaluation of `flake.checkFindings`.
  #
  # And `tryEval (fn ctx)` would not have been enough even for a `throw`: it
  # forces only the returned list's SPINE, so an effect record that throws when
  # READ throws outside it.
  #
  # So the rule is avoidance rather than recovery: a policy is called only when
  # every argument it names is one that can be given a real value, which here is
  # `host` and its `system`. Everything else is skipped, and skipping is also
  # the correct answer rather than merely the safe one -- `celler-use-to-host`
  # wants `user`, `household-to-hosts` wants `household`, `specialisations-to-
  # home` wants `home`, and a policy that only exists in a user, household or
  # home scope is not a host-level pipe. Measured against the live registry: of
  # 54 policies, the ones carrying a `collect`/`collectAll` stage --
  # `backup-sources`, `celler-caches`, `postgres-databases`,
  # `carrier-attachment-roster` -- all pass this filter.
  callable =
    pol:
    let
      fn = if builtins.isAttrs pol && (pol.__isPolicy or false) then pol.fn else pol;
    in
    lib.all (n: n == "host" || n == "system") (lib.attrNames (argsOf fn));

  pipeEffectsOf =
    pol: host:
    let
      fn = if builtins.isAttrs pol && (pol.__isPolicy or false) then pol.fn else pol;
      args = argsOf fn;
      # A policy written `_:` has no named arguments and is handed the same
      # context; `system` is in it because a policy that destructures it uses it
      # to index `den.hosts`, where null is not a key.
      ctx = lib.mapAttrs (_: _: null) args // {
        inherit host;
        system = host.system or null;
      };
      r = builtins.tryEval (fn ctx);
      raw = if r.success then r.value else [ ];
      es = if builtins.isList raw then raw else [ raw ];
    in
    # ── THE GUARD HAS TO BE PER ELEMENT, NOT PER CALL ──
    # `tryEval (fn ctx)` forces only the list's SPINE; the effect records inside
    # it stay thunks, so a policy that throws when one of them is read throws
    # OUTSIDE the tryEval. Measured, and it is not hypothetical: with a context
    # carrying `system = null`, ../base/household-scope.nix's
    # `den.hosts.${system}` fails with `expected a string but found null` from
    # inside a policy this check only wanted to look at. A policy that cannot be
    # called in a host context is not a host-level pipe, so swallowing it is also
    # the right answer rather than merely the safe one.
    lib.filter (
      e:
      let
        ok = builtins.tryEval (builtins.isAttrs e && (e.__policyEffect or "") == "pipe");
      in
      ok.success && ok.value
    ) es;

  crossesFor =
    host:
    lib.unique (
      lib.concatMap (
        pol:
        lib.concatMap (
          e:
          let
            v = e.value or { };
            stages = if builtins.isList (v.stages or null) then v.stages else [ ];
            crosses = lib.any (s: builtins.isAttrs s && lib.elem (s.__pipeStage or "") crossingStages) stages;
          in
          lib.optional (crosses && builtins.isString (v.pipeName or null)) v.pipeName
        ) (pipeEffectsOf pol host)
      ) (lib.filter callable policyList)
    );

  # ONE walk of the policy registry per host, not one per (source, host) pair.
  # `genAttrs`'s values are thunks keyed by name, so the answer for a host is
  # computed once however many sources are tested against it -- which is the
  # same reason ../framework/checks.nix keeps `hostsOf` in one place rather than
  # letting four files rebuild it.
  crossesByHost = lib.genAttrs (map (h: h.name) hostList) (n: crossesFor hostsOf.${n});

  # ── SOURCES: WHO FURNISHES A CONFIG-DEPENDENT VALUE ON A CROSSING QUIRK ──
  quirkSources =
    let
      emitters = lib.concatMap (
        entry:
        map (q: entry // { quirk = q; }) (lib.filter (q: bindsConfig (tryGet entry.aspect q)) quirkNames)
      ) aspectEntries;
    in
    lib.concatMap (
      e:
      map (h: {
        inherit (e) path quirk;
        host = h.name;
        kind = "quirk";
      }) (lib.filter (h: den.lib.capability.has h e.aspect) hostList)
    ) emitters;

  # ── SOURCES: THE CAPABILITY HALF ──
  # Empty while `cross-host-emissions-are-entity-only` holds. `gives` cannot be
  # called without a config, so which capabilities such a giver furnishes is not
  # knowable here -- which is why the reader side below is `reach` rather than a
  # taker walk.
  capSources = lib.concatMap (
    entry:
    let
      a = entry.aspect;
      reach = a.reach or "host";
    in
    lib.optionals (reach != "host" && bindsConfig (tryGet a "gives")) (
      map (h: {
        inherit (entry) path;
        host = h.name;
        quirk = "gives (reach = \"${reach}\")";
        kind = "capability";
        inherit reach;
      }) (lib.filter (h: den.lib.capability.has h a) hostList)
    )
  ) aspectEntries;

  inherit (den.lib.capability) sameHousehold;

  # ── THE EDGES ──
  # K -> H reads "building K's configuration forces H's". A self edge is dropped:
  # a host resolving its OWN emission does it in-scope and lazily, which is the
  # case den's docs say costs nothing.
  edges =
    lib.concatMap (
      s:
      lib.concatMap (
        k:
        lib.optional (k.name != s.host && lib.elem s.quirk crossesByHost.${k.name}) {
          from = k.name;
          to = s.host;
          via = s.quirk;
          at = s.path;
        }
      ) hostList
    ) quirkSources
    ++ lib.concatMap (
      s:
      let
        src = hostsOf.${s.host};
      in
      lib.concatMap (
        k:
        lib.optional (k.name != s.host && (if s.reach == "fleet" then true else sameHousehold k src)) {
          from = k.name;
          to = s.host;
          via = s.quirk;
          at = s.path;
        }
      ) hostList
    ) capSources;

  # ── WHAT IS ON A CYCLE ──
  #
  # Repeatedly drop every node with no outgoing edge and every node with no
  # incoming one. A node survives that exactly when it lies on a cycle, which is
  # the whole question -- no ordering of the fleet is needed to answer it, and
  # the iteration terminates because each round either removes a node or is the
  # fixpoint.
  #
  # `lib.converge` rather than a hand-rolled recursion: the fixpoint is the
  # definition of the answer here, not an implementation detail.
  onCycle =
    let
      step =
        ns:
        let
          live = lib.filter (e: lib.elem e.from ns && lib.elem e.to ns) edges;
        in
        lib.filter (n: lib.any (e: e.from == n) live && lib.any (e: e.to == n) live) ns;
    in
    lib.converge step (lib.unique (map (e: e.from) edges ++ map (e: e.to) edges));

  cycleEdges = lib.filter (e: lib.elem e.from onCycle && lib.elem e.to onCycle) edges;

  # ── THE PAIR, NOT THE OUT-EDGES ──
  #
  # Listing every edge out of a node on the cycle buries the one that closes it.
  # Measured on the live fleet: celler2 has five edges to secondpc (four
  # `backup` emissions and one `celler-caches`) and only `celler-caches` has a
  # return edge, so the first version's finding named four emissions that are
  # not part of any cycle. A mutual pair is the unit somebody can act on, so
  # that is the unit reported, with the edge in each direction named.
  mutualPairs =
    let
      has = a: b: lib.filter (e: e.from == a && e.to == b) cycleEdges;
    in
    lib.concatMap (
      a:
      lib.concatMap (
        b:
        lib.optional (a < b && has a b != [ ] && has b a != [ ]) {
          inherit a b;
          # EVERY edge in each direction, not the first one. celler2 reaches
          # secondpc through five emissions and only one of them has a return
          # edge; naming just `lib.head` of them made the report look arbitrary
          # about which one it had picked.
          there = has a b;
          back = has b a;
        }
      ) onCycle
    ) onCycle;

  paired = lib.concatMap (p: [
    p.a
    p.b
  ]) mutualPairs;

  # A cycle of three or more hosts has no mutual pair in it, so it would vanish
  # from the report if only pairs were listed. These are the survivors of the
  # fixpoint that no pair accounts for.
  longCycle = lib.filter (n: !(lib.elem n paired)) onCycle;
in
{
  den.checks.cross-host-config-dependence-is-acyclic = {
    description = "No two hosts' configurations depend on each other through a config-dependent cross-host emission.";

    # ── `report`, NOT `error`, AND THE REASON IS MEASURED ──
    #
    # ../framework/checks.nix defines `report` for "a check that is CORRECT and
    # currently fails"; this is the neighbouring case, a check that is correct
    # and currently over-reports, and the remedy is the same -- the finding is
    # visible to everybody without stopping everybody's builds.
    #
    # It fires today on celler2 <-> secondpc. That pair is REAL: both hosts
    # include `den.aspects.celler-server`'s `provides.nixos`, its `celler-caches`
    # emission binds `config`, and ../base/celler/push.nix collects the quirk
    # with `pipe.collectAll (_: true)`, so each host resolves the other's thunk
    # against the other's instantiated config.
    #
    # It is also benign today, and the evidence is that secondpc evaluates: a
    # full `nixosConfigurations.secondpc.config.system.build.toplevel.drvPath`
    # completes (it stops on an unrelated missing secret, after the module
    # fixpoint), which an infinite recursion would not. The reason is the one
    # this file's header calls undecidable: the emission reads
    # `config.services.cellerd.expose`, and nothing downstream of the pooled
    # `celler-caches` value feeds `expose`, so Nix's laziness resolves the pair
    # instead of chasing it.
    #
    # Promote to `error` when the pair is gone (one celler server, or the
    # addresses read off the host ENTITY), not before: an `error` that cannot be
    # made green is a check people learn to pass with `--no-build-checks`.
    severity = "report";

    fix = ''
      Read the fact off the host ENTITY (`den.hosts.<sys>.<n>`) on at least one side, so that side's emission is a plain function over entities and resolves without the other host's fixpoint. Where the value genuinely only exists after instantiation -- `aspects/base/celler/server.nix`'s `endpoint`/`addresses` are detected from the running config and its header says why they cannot move -- the way out is to leave exactly ONE host furnishing it, or to split the quirk so each direction carries only what the other side does not compute.

      Before acting on a finding, check whether it is one of the benign kind: a mutual pair recurses only if the config attribute the emission READS is downstream of the pooled value the consumer WRITES. That is not decidable here (see this file's header), so a finding states the shape and you confirm the direction. If it is benign, say so in this file with the attributes named, next to the celler note.
    '';

    findings =
      probe "cross-host-config-dependence-is-acyclic"
        "nix eval --impure --json .#debug.config --apply 'cfg: cfg.den.checks.cross-host-config-dependence-is-acyclic.findings'"
        (
          map (
            p:
            let
              vias = es: lib.concatMapStringsSep ", " (e: "`${e.via}` (${e.at})") es;
            in
            "${p.a} and ${p.b} depend on each other's CONFIGURATION: ${p.a} resolves ${p.b}'s ${vias p.there}, and ${p.b} resolves ${p.a}'s ${vias p.back}. All of these bind `config`, so each is resolved against the other host's instantiated config. Building either forces the other -- the shape den's fleet documentation says must be a DAG (https://den.denful.dev/explanation/fleet/), and the one for which Nix says only `infinite recursion encountered` with no indication of which two hosts. See `fix` before acting: a mutual pair is the shape, not the proof."
          ) mutualPairs
          ++ lib.optional (longCycle != [ ]) (
            "${lib.concatStringsSep ", " longCycle} lie on a cycle of config-dependent cross-host emissions that is longer than one pair, so no two of them depend on each other directly. The edges between them: ${
              lib.concatMapStringsSep "; " (e: "${e.from} -> ${e.to} via `${e.via}` (${e.at})") (
                lib.filter (e: lib.elem e.from longCycle && lib.elem e.to longCycle) cycleEdges
              )
            }. Breaking any one of them breaks the cycle."
          )
        );
  };
}
