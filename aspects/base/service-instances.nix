{
  den,
  lib,
  config,
  inputs,
  ...
}:
# INSTANCES ARE SPAWNED BY gen-scope. DEFINITIONS ARE WRITTEN.
#
# `den.serviceTypes.<class>` is a SCHEMA -- what that kind of service IS. A
# schema names no host, because the same schema instantiates anywhere: not all
# cellers are on secondpc; secondpc happens to have one. A host says what it
# RUNS, and the cascade turns that into instances:
#
#   den.hosts.x86_64-linux.secondpc.runs.celler.defines.cache.main = { };
#
#     -> celler@secondpc              the daemon
#        main/celler@secondpc         the cache it was told to have
#        push/main/celler@secondpc    IMPLIED -- celler-cache defines a push
#        pull/main/celler@secondpc    IMPLIED -- and a pull
#
# ── THE CASCADE IS gen-scope's, NOT OURS ──
#
# This file first hand-rolled it: a recursive `mk` with `maxDepth = 8` and a
# throw. `gen-scope/lib/cascade.nix` is that mechanism done properly, and the
# difference is not stylistic:
#
#   * `mkKind { below; spawns.<kind> = self: id: { ... }; }` declares, AT
#     REGISTRATION, which kinds a kind can expand into. Every `spawns` key must
#     already be a `below` name, and `mkKinds` resolves each `below` name only
#     against kinds minted strictly EARLIER -- so a non-descending or cyclic
#     cascade is INEXPRESSIBLE rather than detected. There is no depth bound
#     here because there is nothing for one to catch.
#   * `depth = 1 + max depth below` is minted with each kind and is the
#     termination witness (Söderberg & Hedin 2013 §7: "each new NTA has a lower
#     order than its host"). Measured on this shape:
#     push/pull 0, celler-cache 1, celler 2, host 3.
#   * THE PARENT EDGE IS STAMPED BY THE SUBSTRATE. A spawn descends exactly one
#     level, so the host IS the parent and a builder asserting otherwise is
#     refused. That is what makes the identity path graph STRUCTURE rather than
#     a string we join -- and it is what makes a sub-instance's siblings
#     reachable, since a push and its pull are two children of one cache node.
#
# ── WHY READING `runs` DOES NOT CYCLE, AND WHY ONE COLLECTION DID ──
#
# Reading a host entity's declared OPTION is lazy per option, so it does not
# force that host's `resolved`. Asking `hasAspect` instead WOULD close the loop:
# den's nix/lib/aspects/has-aspect.nix records it as a lookup over the owning
# entity's production run, and ./service.nix puts `den.policies.service-aspects`
# -- which reads `den.services` -- on `den.schema.host.includes`. Declaration,
# never predicate.
#
# The recursion this file hit was narrower and worth naming exactly: the KEYS of
# its output depended on reading `allDefines` off the registry it was defining,
# and the module system cannot hand over a merged option until every definition
# of it is known. An eager key computation inside a module fixpoint. Laziness at
# the value level does not fix that, because the keys are what is demanded;
# reading a DIFFERENT option does, which is why `den.serviceTypes` is separate
# from `den.services` -- and that separation is independently right, since a
# definition has no host.
let
  gs = inputs.gen-scope.lib;

  hosts = lib.concatMapAttrs (_: hs: hs) den.hosts;
  types = config.den.serviceTypes;

  # The scoped class a definition invents under a label. `allDefines` is keyed
  # by the scoped name (`celler` + `cache` -> `celler-cache`), and a declarer
  # writes the short label, so the cascade needs the map back. The scoped name is
  # `<definer>-<label>` by construction, so stripping the definer recovers the
  # label without a second table.
  labelsOf =
    class:
    let
      scoped = lib.attrNames (types.${class}.allDefines or { });
      unscope = s: if lib.hasPrefix "${class}-" s then lib.removePrefix "${class}-" s else s;
    in
    lib.listToAttrs (map (s: lib.nameValuePair (unscope s) s) scoped);

  # ── THE DECLARATION ORDER `mkKinds` REQUIRES ──
  #
  # `mkKinds` is a left fold over an ORDERED list: a kind's `below` names resolve
  # only against kinds minted strictly earlier, which is exactly what makes a
  # cycle unmintable. So the list has to be layered bottom-up, and this is the
  # one sort that cannot be delegated -- gen-scope needs the order in order to
  # compute the depth that would otherwise give it.
  #
  # Peeled iteratively rather than by a recursive depth function, so a cycle is a
  # NAMED refusal ("these classes define each other") instead of a bare
  # `infinite recursion encountered` from our own recursion.
  layerOrder =
    let
      all = lib.attrNames types;
      step =
        state:
        let
          ready = lib.filter (
            c: !(lib.elem c state.placed) && lib.all (d: lib.elem d state.placed) (lib.attrValues (labelsOf c))
          ) all;
        in
        if ready == [ ] then
          state
        else
          step {
            placed = state.placed ++ ready;
          };
      final = step { placed = [ ]; };
      stuck = lib.subtractLists final.placed all;
    in
    if stuck == [ ] then
      final.placed
    else
      throw ''
        den.serviceTypes: ${lib.concatStringsSep ", " stuck} define each other, directly or through a chain.
        `defines` is a downward relation -- a celler defines caches, a cache defines a push -- so a cycle
        has no bottom to start from and no instance set to spawn. Break it in `den.serviceTypes.*.defines`.
      '';

  # Every id is derived, never written. A host spawns `<class>@host:<h>`; a named
  # sub-instance is `<name>/<parent>`; a singleton is `<label>/<parent>`.
  hostId = h: "host:${h}";

  # EVERY HOST IS CONNECTED TO ITSELF and nobody should have to write that down.
  # Unioned in by the cascade rather than set as the option's default, because a
  # default is displaced by any definition at all -- see the note on `runs`.
  # A host that wants `::1` instead declares `loopback` itself and wins, since
  # its own declaration is on the right of the merge.
  alwaysRuns = {
    loopback = { };
    # EVERY HOST CAN SPEAK HTTP over whatever it is connected by. A LAYER, not
    # something you install: it resolves its own `tcp` and carries no address of
    # its own, so an http link answers whatever tcp that http got -- loopback's
    # `127.0.0.1` on the box, the lan address from elsewhere. The walk lives on
    # the LINK (`bindings` in ../../lib/service-dep-type.nix) and not on the
    # `address` option, because an option default that reads `resolved` makes a
    # DEFINITION resolve, and definitions resolve against the empty registry.
    http = { };
  };

  # ── WHAT EACH SPAWNED NODE WAS DECLARED WITH ──
  #
  # A spawn builder is `self: id: { <childId> = { ... }; }` and may close over
  # anything lexically, so the per-instance declaration is looked up by the id
  # the cascade is about to mint. Built from `runs` and `serviceTypes` only --
  # never from `den.services` -- which is what keeps the direction
  # declarations -> graph -> instances and never back.
  declOf = lib.listToAttrs (
    let
      walk =
        parentId: class: decl:
        let
          labels = labelsOf class;
          declared = decl.defines or { };
          kids = lib.concatLists (
            lib.mapAttrsToList (
              label: scopedClass:
              let
                named = declared.${label} or { };
              in
              if named == { } then
                # SINGULAR: nothing named it, so exactly one exists and the
                # label IS the instance. A cache has one push.
                walk "${label}/${parentId}" scopedClass { }
              else
                lib.concatLists (lib.mapAttrsToList (n: d: walk "${n}/${parentId}" scopedClass d) named)
            ) labels
          );
          unknown = lib.subtractLists (lib.attrNames labels) (lib.attrNames declared);
        in
        if unknown != [ ] then
          throw ''
            den.hosts.*.runs: ${parentId} declares sub-instances of ${lib.concatStringsSep ", " unknown}, which `${class}` does not define.
            It defines: ${
              if labels == { } then "nothing" else lib.concatStringsSep ", " (lib.attrNames labels)
            }.
          ''
        else
          [
            (lib.nameValuePair parentId {
              inherit class decl;
              # The instance's own label: the FIRST path segment, because ids are
              # built label-first. With `last` every singleton took the root
              # label and a cache's push and pull both came back as one
              # identity -- the silent-merge failure the identity model exists to
              # prevent.
              # ... and then the part before `@`, because a top-level id carries
              # the host scope it was spawned under (`celler@host:secondpc`).
              # Without this the identity came back as
              # `celler@host:secondpc@secondpc` -- the host twice, once from the
              # node id and once from `rawIdentity`'s own `@host` suffix.
              label = lib.head (lib.splitString "@" (lib.head (lib.splitString "/" parentId)));
            })
          ]
          ++ kids;
    in
    lib.concatLists (
      lib.concatLists (
        lib.mapAttrsToList (
          hname: h:
          lib.mapAttrsToList (class: decl: walk "${class}@${hostId hname}" class decl) (alwaysRuns // h.runs)
        ) hosts
      )
    )
  );

  # A spawn builder for one label of one class: find the ids `declOf` minted
  # under this node for that label, and return them. The substrate stamps `type`
  # and `parent`, and refuses a builder that tries to choose either.
  spawnFor =
    class: label: scopedClass: _self: id:
    lib.listToAttrs (
      map (childId: lib.nameValuePair childId { decls = { }; }) (
        lib.filter (
          k:
          let
            d = declOf.${k};
          in
          d.class == scopedClass && lib.hasSuffix "/${id}" k
        ) (lib.attrNames declOf)
      )
    );

  kindOfClass =
    class:
    let
      labels = labelsOf class;
    in
    gs.mkKind {
      below = lib.attrValues labels;
      spawns = lib.mapAttrs' (
        label: scopedClass: lib.nameValuePair scopedClass (spawnFor class label scopedClass)
      ) labels;
    } class;

  classKinds = map kindOfClass layerOrder;

  # The host kind spawns whatever each host says it runs, and `root` sits above
  # it so the chain is `instance -> ... -> host -> root` -- the same shape
  # ../../lib/service-scope.nix walks with `parent*`.
  hostKind = gs.mkKind {
    below = layerOrder;
    spawns = lib.listToAttrs (
      map (
        class:
        lib.nameValuePair class (
          _self: id:
          lib.listToAttrs (
            map (childId: lib.nameValuePair childId { decls = { }; }) (
              lib.filter (k: k == "${class}@${id}") (lib.attrNames declOf)
            )
          )
        )
      ) layerOrder
    );
  } "host";

  kinds = gs.mkKinds (
    classKinds
    ++ [
      hostKind
      (gs.mkKind { below = [ "host" ]; } "root")
    ]
  );

  hostIds = map hostId (lib.attrNames hosts);

  roots = gs.buildRoots {
    inherit kinds;
    parentGraph = gs.star "root" hostIds;
    decls = {
      root = { };
    }
    // lib.genAttrs hostIds (_: { });
    types = {
      root = "root";
    }
    // lib.genAttrs hostIds (_: "host");
  };

  ev = gs.eval { } {
    children = _self: id: lib.filterAttrs (_: n: (n.parent or null) == id) roots.nodes;
  } roots;

  # Only the spawned nodes become services; `root` and the host scopes are the
  # graph's own structure.
  spawnedIds = lib.filter (k: declOf ? ${k}) (lib.attrNames ev.allNodes);
in
{
  # The function form, not `den.schema.host.options.runs = ...` directly:
  # ./service.nix already defines `den.schema.host` as a function and the two
  # forms cannot coexist. Two function definitions merge as modules do.
  den.schema.host = _: {
    options.runs = lib.mkOption {
      type = lib.types.attrsOf lib.types.raw;
      # NOT `default = { loopback = { }; }`, and this cost an eval to find.
      #
      # A module default is DISCARDED the moment anything defines the option, so
      # the two hosts that declare a `lan-home` attachment lost their loopback
      # entirely -- `declaredIn` showed `host:auspc` and `host:secondpc`
      # declaring nothing while every other host declared
      # loopback/tcp/udp/unix. The hosts that said the most got the least, which
      # is exactly the shape that makes a default-based rule dangerous rather
      # than merely weak.
      #
      # So "every host is connected to itself" is applied by the cascade
      # (`alwaysRuns` below), where it is a union and cannot be displaced.
      default = { };
      example = lib.literalExpression "{ celler.defines.cache.main = { }; }";
      description = ''
        The service instances this host runs, keyed by DEFINITION name. The
        manifest of what the machine is, and the only place placement is stated --
        a definition never names a host.

        `defines.<label>.<name>` names instances of a sub-type the definition
        invents; a label with none named gets exactly one, because that is what
        cardinality means. Everything below is implied, so a celler with one
        cache declared here yields four instances.
      '';
    };
  };

  den.services = lib.listToAttrs (
    map (
      id:
      let
        d = declOf.${id};
        node = ev.allNodes.${id};
        parentIsService = declOf ? ${node.parent};

        # ── THE SUBTYPE'S INHERITING FIELDS, COPIED DOWN ──
        #
        # `includes` merges capabilities and obligations, never field values,
        # so a definition stating `prefix = "192.168.0.0/16"` does not reach
        # its attachments. The relation type carries no knowledge of WHICH
        # fields inherit -- it would become the union of every field anything
        # might have -- so each subtype lists its own in `inherits` and this
        # takes the nearest value down the `includes` chain.
        #
        # Nearest wins and the instance's own declaration beats both, since
        # `d.decl` is merged after this.
        inheritedFields =
          let
            chainOf = t: [ t ] ++ lib.concatMap chainOf (t.includes or [ ]);
            chain = chainOf types.${d.class};
            names = lib.unique (lib.concatMap (t: t.inherits or [ ]) chain);
            nearest = n: lib.findFirst (v: v != null) null (map (t: t.${n} or null) chain);
          in
          lib.filterAttrs (_: v: v != null) (lib.genAttrs names nearest);
      in
      lib.nameValuePair id (
        {
          # IDENTITY-PRESERVING: `serviceName` is the instance's own label,
          # which is what ../../lib/service-dep-type.nix joins into
          # `rawIdentity`. `instanceName` is left null even for a named instance
          # -- it carries the class-legible notation
          # (`celler-cache:main/celler@secondpc`), and turning it on here would
          # rewrite every identity string in the tree at the same time as
          # introducing the cascade. Opt in per instance.
          serviceName = d.label;
          instanceName = null;

          # STAMPED, NOT DERIVED. An implied instance's machine is a consequence
          # of its parent's, and `placement` derives it that way -- but
          # ../../lib/service-scope.nix builds the graph FROM `host` and reads a
          # lookup scope while resolving, so a `host` default walking the
          # identifying chain closes graph -> host -> resolved -> graph. The
          # cascade knows the host at spawn time and writes it down.
          host = lib.removePrefix "host:" (
            lib.last (lib.splitString "/" (lib.last (lib.splitString "@" id)))
          );
          reach = d.decl.reach or types.${d.class}.reach or "host";
          aspect = d.decl.aspect or null;
          includes = [ types.${d.class} ];

          # AN INSTANCE DECLARES ITS CLASS AS AN INTERFACE, which is what lets a
          # child resolve its parent BY CLASS: a push says `uses.celler-cache`,
          # and an instance's `serviceName` is its own name (`main`), not its
          # class. Stamped because "an instance of X gives X" is true of every
          # instance and a fact nobody should restate.
          gives.${d.class} = { };

          # WHAT THE DEFINITION SAYS ITS INSTANCES DO. Carried for the same
          # reason as `uses` below and found the same way: the policy that
          # delivers `implement` filters on `s.implement != null`, which is
          # FALSE on every instance while the body sits on the definition, so
          # the arm never fired and the content landed nowhere -- silently.
          # `includes` merges capabilities and obligations, not field values.
          # THE SERVICE'S OWN CLASS CONTENT. A serviceType IS an aspect with
          # relations -- not a second description of one -- so `nixos` lives on
          # the definition beside `gives`/`uses`, and the instance carries it
          # for the same reason it carries `uses`: `includes` merges
          # capabilities, never field values.
          # Every class the definition carries. A service may configure a host,
          # a Mac, or a person's environment -- and which it is, is the class's
          # name, not a flag.
          nixos = types.${d.class}.nixos or null;
          homeManager = types.${d.class}.homeManager or null;
          darwin = types.${d.class}.darwin or null;

          # THE DEFINITION'S OWN EDGES, CARRIED ONTO THE INSTANCE, merged with
          # the cascade's identifying edge in ONE place -- `//` between two
          # blocks that both wrote `uses` would have silently kept only the
          # second.
          #
          # `includes` merges capabilities and obligations and NOT field values,
          # the gap that already bit `address`, `prefix` and `public`. So
          # `den.serviceTypes.celler-cache-pull.uses.tcp` never reached
          # `pull/main/celler@secondpc`, measured as
          # `bindings.tcp: attribute 'tcp' missing` while the ancestor binding
          # beside it resolved fine. A definition that cannot say what its
          # instances NEED is not a schema, so carrying this is the cascade's
          # job rather than something every declaration restates.
          #
          # `name` is stripped because it is readOnly and defaults to the
          # attribute name on both sides; re-defining it would be setting a
          # readOnly option to a value it already has.
          # THREE SOURCES, MERGED IN ONE PLACE, because `//` between blocks
          # that each wrote `uses` keeps only the last -- the displacement that
          # already cost the `runs` loopback default and `gateway.groups`.
          #
          #   the DEFINITION's edges   what this class always needs
          #   the DECLARATION's       what this host said about this instance,
          #                           which is how a TAKER names the thing it is
          #                           a pull of
          #   the cascade's own       the identifying edge to a stamped parent
          uses =
            lib.mapAttrs (_: u: builtins.removeAttrs u [ "name" ]) types.${d.class}.uses
            // (d.decl.uses or { })
            // lib.optionalAttrs parentIsService {
              # A cache IS this celler's cache, so which celler it is on is part
              # of which cache it is. The cascade's own, because a definition
              # cannot know which parent instance it got.
              ${declOf.${node.parent}.class}.identifying = true;
            };
        }
        // lib.optionalAttrs parentIsService {
          parent = node.parent;
        }
        // inheritedFields
        // builtins.removeAttrs d.decl [
          "defines"
          "reach"
          "aspect"
          # `uses` is merged above from three sources; letting it through here
          # would overwrite that merge with the declaration alone.
          "uses"
        ]
      )
    ) spawnedIds
  );
}
