# Which lock an aspect's inputs belong in, declared on the aspect.
#
# The tag is the a priori declaration that breaks the bootstrap cycle; a trace
# check is the a posteriori proof it was true. Input *declarations* already
# route by evaluation -- ../dev/idris.nix declares `idris2Packages` on the
# aspect and it lands in the darwin sub-flake only, because only darwin hosts
# resolve it. What cannot route that way is the lock *node*, because you cannot
# evaluate to discover an input before the lock providing it exists. Hence a tag.
#
# Named `layers`, not `classes`: den already owns `den.aspects.<x>.classes`
# for class content. Most values are host classes anyway, so the tag still says
# something true about the aspect rather than pure bookkeeping.
#
# Emits no `config` -- the generator reads the options. That sidesteps the
# introspection trap ../framework/roles.nix documents (`den.schema.aspect` is
# also read standalone, where `includes` is undeclared, so `mkIf` faults) rather
# than having to avoid it carefully.
#
# Nothing propagates DOWNWARD, deliberately, in two places:
#
#   * root -> layer. A layer never re-owns an input the root provides; the
#     resolver drops every root name from a layer's own set (`exclude` in
#     ../../lib/inputs.nix), so a shared pin resolves from the root and
#     from nowhere else. A layer's copy exists only so its `follows` can name it.
#   * aspect -> included aspects. `layerOf` intersects the tags of the aspects
#     that DECLARE an input, never those of aspects they pull in. An aspect's
#     tag says where its own inputs are locked; it says nothing about anything
#     it includes.
#
# Widening still happens upward, and must: an untagged declarer contributes the
# empty set, so the intersection collapses to the root. A forgotten tag makes an
# input shared rather than hiding it from a host that needs it.
#
# Soft spot worth knowing: an aspect pulled in conditionally (`den.lib.whenAspect`,
# a role gate) can resolve on a class its tag did not anticipate. A trace check
# catches that; a static tag structurally cannot.
{
  lib,
  den,
  inputs,
  config,
  rootPath,
  ...
}:
let
  # Written literally, like `roles` in ./roles.nix.
  #
  # `darwin` and `nixos` are den host classes; `ios` is absent because a device
  # nix cannot build has no inputs to lock.
  #
  # The rest have no host class behind them, which is why this option is
  # `layers` and not `classes`:
  #
  #   homebrew -- ../darwin/homebrew.nix generates one `flake = false` input per
  #     tap, a large set that churns on its own schedule. Out of the darwin lock,
  #     updating a tap touches nothing else.
  #   gui -- orthogonal to platform: both darwin and nixos have headless hosts,
  #     and none should fetch stylix, fonts or a browser.
  #   dev -- likewise orthogonal: toolchains, language servers and the rust/js
  #     input sets have no business in a host that only serves.
  #   packages -- the ./packages tree. Already a shared bucket today (every host
  #     needs its overlays, which is why partition-map.nix lists it under
  #     `deps`), so a layer every system loads rather than a platform one.
  #
  # `shared` is NOT in this list: it is the name of the EMPTY tag set, derived
  # rather than declared. Writing `layers = [ ]` and declaring nothing mean the
  # same thing, so there is nothing to spell.
  #
  # `gui` and `dev` mirror the roles of the same name in ./roles.nix, but are
  # not the same mechanism: a role gates CONTENT delivery to a host+user pair,
  # a layer decides which LOCK an input is written to.
  layers = [
    "darwin"
    "nixos"
    "homebrew"
    "gui"
    "dev"
    "packages"
  ];

  layerList = lib.types.listOf (lib.types.enum layers);

  _evalProbe = builtins.trace "LAYERS_MODULE_EVAL" null;

  # Which layer this evaluation is generating for, or null for a normal build.
  #
  # An env var rather than a module arg because the generator drives it from
  # outside `nix eval`, and because ../../lib/inputs.nix already
  # establishes the pattern with `PATCH_HASHES=ignore`. `getEnv` returns "" in
  # pure eval, so a real build is never affected by it.
  generatingFor =
    let
      v = builtins.getEnv "DENDRITIC_LAYER";
    in
    if v == "" then null else v;

  # A tag set names a layer directory. Sorted so [ "gui" "nixos" ] and
  # [ "nixos" "gui" ] are the same layer.
  #
  # The empty set is `shared`, NOT the root. The root flake.lock is reserved for
  # the bootstrap set only -- nixpkgs, and the inputs whose flakeModules the top
  # flake.nix imports directly (flake-parts, flake-file, den, treefmt-nix,
  # devshell, agenix-rekey, flake-compat). Those have to be in the root because
  # the root evaluates them before any layer is resolved; nothing else does, so
  # nothing else belongs there. An input reached across two layers is shared,
  # which is a layer like any other -- it just has no tags.
  # `patch` and `meta` are DECLARATIONS consumed by the module system reading
  # these specs back -- neither is a flake input attribute, and nix rejects a
  # nested one outright ("attribute 'patch' is a thunk while a string, Boolean,
  # or integer is expected"). flake-file strips them from the inputs IT owns
  # because they are declared options there; aspect-declared specs arrive as
  # raw attrsets and skip that, so they are stripped here instead -- from the
  # root flake.nix and from every layer's.
  # flake-file's own serialiser. `inputsExpr` NORMALISES an input spec (attrset
  # -> attrset); `nixCode` is what turns the result into text. Interpolating
  # `inputsExpr` straight into a string fails with "cannot coerce a set to a
  # string" -- which is why nothing this module rendered was ever correct.
  ffLib = import "${inputs.flake-file}/dev/modules/_lib" lib;

  # The same styling flake-file gives the root's `inputs` block, so a layer
  # flake is formatted like one and its diffs stay readable. `collapseAttrs =
  # false` keeps one input per line -- collapsed, the whole block is a single
  # line and every regeneration looks like a total rewrite.
  layerStyles =
    let
      inherit (config.flake-file.style) sep sortPriority;
    in
    [
      {
        attrSortPriority = sortPriority.inputs;
        attrSep = sep.inputs;
        collapseAttrs = false;
      }
      {
        attrSortPriority = sortPriority.inputSchema;
        attrSep = sep.inputSchema;
      }
      { attrSortPriority = sortPriority.inputs; }
    ];

  followsOf =
    spec: lib.filter (f: f != null) (lib.mapAttrsToList (_: v: v.follows or null) (spec.inputs or { }));

  rootLock = builtins.fromJSON (builtins.readFile (rootPath + "/flake.lock"));

  # Every input the plan places anywhere, plus whatever still declares at file
  # level, so a follows-target that is simply a TYPO is not quietly turned into
  # a stub that resolves to nothing.
  knownNames =
    lib.attrNames (lib.foldl' (x: y: x // y) { } (builtins.attrValues config.flake.layerPlan))
    ++ lib.attrNames (rootLock.nodes.${rootLock.root}.inputs or { });

  # Names a layer refers to through `follows` but does not own. ONE definition:
  # the rendered flake.nix and the stub directories the generator creates must
  # agree exactly, and computing them twice is how they would stop agreeing.
  carriedFor =
    specs:
    lib.filter (n: !(specs ? ${n}) && lib.elem n knownNames) (
      lib.unique (lib.concatMap followsOf (lib.attrValues specs))
    );

  declOnly =
    spec:
    builtins.removeAttrs spec [
      "patch"
      "meta"
    ];

  layerName =
    tags:
    let
      clean = lib.filter builtins.isString (if builtins.isList tags then tags else [ ]);
    in
    if clean == [ ] then "shared" else lib.concatStringsSep "-" (lib.sort (a: b: a < b) clean);
in
{
  # Written, not declared: `flake.lib` is already a freeform option, and
  # ../framework/mobile.nix sets `flake.lib.mobileSystem` the same way.
  # Declaring a sub-option under it collides.
  # The layer is declared HERE, on the system, and inherited by everything it
  # includes. Defaults to the host's class, so an ordinary nixos host needs no
  # declaration at all and `gui` or `homebrew` is the only thing worth writing.
  config.den.schema.host =
    { config, ... }:
    {
      options.layers = lib.mkOption {
        type = layerList;
        # Defaults to the host's own class when that is a layer, so an ordinary
        # darwin or nixos host needs no declaration at all -- `gui`, `dev` or
        # `homebrew` is the only thing worth writing by hand. `ios` is not a
        # layer (a device nix cannot build has no inputs to lock), so the phone
        # gets [ ].
        default = lib.optionals (lib.elem (config.class or "") layers) [ config.class ];
        description = ''
          Layers this system is in. Every aspect it resolves inherits them, and
          so does every input those aspects declare. Never propagates upward:
          tagging an aspect does not make a system anything.
        '';
      };
    };

  # Holding pen for `inputs`-class declarations. Serialized nowhere -- the
  # generator reads it and decides which of root / partitions/<layer> each name
  # belongs to.
  options.inputsRaw = lib.mkOption {
    type = lib.types.lazyAttrsOf lib.types.raw;
    default = { };
    description = "Raw aspect-declared inputs, before layer placement.";
  };

  # Where a layer's inputs land instead of `flake-file.inputs`. The routing
  # policy in ./flake-file.nix picks the path per host, so a darwin-only input
  # is never ADDED to the root's inputs rather than being added and pruned.
  options.flake-file-layers = lib.mkOption {
    type = lib.types.lazyAttrsOf lib.types.raw;
    default = { };
    description = "Per-layer input declarations, keyed by layer name.";
  };

  # Both are CLASS content, not options: an option cannot be read back through a
  # capture without forcing the aspect fixpoint the capture is resolving.
  config.den.classes.layers = { };
  config.den.classes.inputLayers = { };

  config.den.schema.aspect =
    { config, options, ... }:
    {
    options = {
      # `layers` on an aspect is CLASS content (den.classes.layers above), not an
      # option -- an option here cannot be read back through a capture without
      # forcing the aspect fixpoint. Declared the same way `inputs` is.

      inputLayers = lib.mkOption {
        type = lib.types.attrsOf (lib.types.nullOr layerList);
        default = { };
        description = ''
          Per-input override of `layers`, for an aspect whose `ff` block mixes
          shared and platform-specific inputs. ../hosts/secondpc/default.nix is
          the real case: `celler` is shared while `arion`, `impermanence`,
          `nix-flatpak` and `nixos-mailserver` are nixos-only.
        '';
      };
    };

    # NOTE: per-aspect input provenance is NOT solved. Four mechanisms tried:
    #
    #   * `mkIf` suppression of `flake-file` -- registers the key on every
    #     aspect; ones that never declared inputs fault with "accessed but has
    #     no value defined".
    #   * `optionalAttrs` suppression -- must force `config.layers` to decide
    #     what to define. "The set of freeform keys is the union of every
    #     module's config attribute names", so guarding on a key needs the key
    #     set to produce the key set (../security/age-scope.nix says this).
    #   * a per-aspect `den.lib.policy.route` into `flake.inputsByAspect.<name>`
    #     -- evaluates, collects nothing, with collectSubtree either way.
    #   * the age-scope.nix pattern (always emit the key, emptiness inside the
    #     value, read `(config.<key> or null).__contentValues or [ ]`) -- STACK
    #     OVERFLOW.
    #
    # The last one is the informative failure, and it is why this is harder than
    # the `scoped` case it was copied from. `scoped` reads `config.secrets`,
    # which feeds nothing upstream. Reading `config.flake-file` feeds the very
    # fixpoint being evaluated: flake-file produces `inputs`, `inputs`
    # parameterise every aspect, and those aspects are where flake-file is read
    # back. `age.scoped` has no such loop.
    #
    # So provenance for THIS class cannot come from the same evaluation that
    # consumes it -- which is the real argument for a separate generation-time
    # evaluation, not a convenience.
  };

  # Exposed so the generator (and inspection) can read it: the route lands
  # layer content at `config.flake-file-layers.<layer>` inside the module
  # system, which is not reachable as a flake output on its own.
  # Exposed so the generator can read declarations as plain data.
  config.flake.inputsRaw = config.inputsRaw;

  config.flake.layerContent = config.flake-file-layers;

  # Fleet-wide input collection, via den's own capture API.
  #
  # NOT per-host: resolving from `den.hosts` only sees what each host declares at
  # its OWN scope, which is both incomplete and WRONG. Measured: the laptop
  # reported 6 inputs (all homebrew) and missed coolabah/paneru/idris2Packages,
  # which arrive at user scope; and `crane` was placed in `nixos` because only
  # nixos hosts declared it at host scope -- even though darwin needs it too.
  # A misplaced shared input breaks one platform silently.
  #
  # `captureFleet` walks the ENTIRE scope tree -- flake -> fleet -> environment
  # -> host -> user -- so flake-level scopes (`flake-system`, where apps and the
  # packages tree live) are included alongside host and user ones. It runs its
  # own pipeline rather than reading the fixpoint it feeds, which is why this

  # (partitionPatchContext temporarily removed to bisect an infinite recursion)

  config.flake.scopeInputs =
    let
      cap = den.lib.capture.captureFleet { class = "inputs"; };
      sci = cap.scopedClassImports or { };
      kinds = cap.scopeEntityKind or { };

      # Scope ids are `k=v` pairs: `host=macmini,system=aarch64-darwin`.
      fieldsOf =
        scope:
        lib.listToAttrs (
          lib.concatMap (
            part:
            let
              kv = lib.splitString "=" part;
            in
            lib.optional (lib.length kv == 2) {
              name = lib.head kv;
              value = lib.elemAt kv 1;
            }
          ) (lib.splitString "," scope)
        );

      hostByName = lib.listToAttrs (
        map (h: {
          name = h.name or "?";
          value = h;
        }) (builtins.concatMap builtins.attrValues (builtins.attrValues den.hosts))
      );

      # A flake-level scope has no host, so its inputs are unconditionally root.
      # host/user scopes inherit the layers of the host named in the scope id.
      layersOfScope =
        scope:
        let
          f = fieldsOf scope;
          h = hostByName.${f.host or ""} or null;
        in
        if h == null then [ ] else h.layers or [ ];

      # The scope's OWN context has to be passed through. These are wrapped class
      # modules expecting entity args (`host`, `user`), and den SKIPS a module
      # whose entity arg is unsatisfied -- documented in ../security/age-scope.nix.
      # Passing only `inputs` silently skipped all 26 of them and yielded `{ }`,
      # which read as "collection failed" when collection was fine.
      ctxs = cap.scopeContexts or { };

      # For the `inputs` class the content IS the input set -- an aspect writes
      # `inputs.disko.url`, so the class body is `{ disko.url = ...; }` and the
      # evaluated `config` is the attrset itself. Reading `config.inputs` here
      # (as the old `flake-file` shape required) looks one level too deep and
      # silently yields `{ }`.
      # Each element of a scope's class list is an ENTRY record --
      # { __loc; __rawEntry; aspectPolicy; class; ctx; globalPolicy; identity;
      #   isContextDependent; module; } -- not a module. Handing the entry
      # straight to evalModules makes those field names the config keys, which
      # is what produced a bogus `root` bucket of `__loc`/`identity`/`module`.
      #
      # The module is `.module`, and each entry carries its OWN `ctx`, which is
      # more precise than the scope-level one: den skips a module whose entity
      # arg is unsatisfied, so the args have to match the entry that emitted it.
      evalEntryIn =
        scope: e:
        builtins.removeAttrs
          (lib.evalModules {
            modules = [
              (e.module or e)
              { freeformType = lib.types.attrs; }
            ];
            specialArgs = { inherit inputs; } // (e.ctx or ctxs.${scope} or { });
          }).config
          [ "_module" ];

      # Same, but satisfying any class-content args the module declares.
      evalDeclFn =
        scope: e:
        let
          mod = e.module or e;
        in
        builtins.removeAttrs
          (lib.evalModules {
            modules = [
              mod
              { freeformType = lib.types.attrs; }
            ];
            specialArgs =
              { inherit inputs; } // (e.ctx or ctxs.${scope} or { }) // classArgsFor scope mod;
          }).config
          [ "_module" ];

      # Args a declaration function needs are satisfied from the SAME capture,
      # never by re-resolving. `den.aspects.homebrew.inputs = { brew, ... }: ...`
      # derives its taps from `brew`, which is class content at this very scope:
      # reading it out of `sci` is a lookup, whereas letting den resolve it would
      # re-enter the aspect being collected and recurse.
      #
      # Class content collects as a LIST of per-definition values, which is the
      # shape these functions expect (`concatMap (x: ...) brew`).
      classArgsFor =
        scope: mod:
        let
          wanted = builtins.attrNames (
            if builtins.isFunction mod then builtins.functionArgs mod else { }
          );
          available = lib.filter (n: (sci.${scope} or { }) ? ${n}) wanted;
        in
        lib.genAttrs available (
          cls: map (e2: evalEntryIn scope e2) ((sci.${scope} or { }).${cls} or [ ])
        );

      # Each declaration evaluated ONCE per scope, paired with the entry that
      # produced it.
      #
      # `evalDeclFn` is a full `lib.evalModules`, and `inputs` and `owner` below
      # both walk the same entry list -- calling it separately in each ran every
      # declaration through the module system twice, since nix does not memoise
      # a function application. 19 scopes worth of that is the bulk of what
      # `layerPlan` costs, and `layerPlan` is what makes `write-flake` slow.
      declsOfScope =
        scope:
        map (e: {
          entry = e;
          value = evalDeclFn scope e;
        }) ((sci.${scope} or { }).inputs or [ ]);
    in
    lib.genAttrs (builtins.attrNames sci) (
      scope:
      let
        decls = declsOfScope scope;
      in
      {
      kind = kinds.${scope} or "?";
      layers = layersOfScope scope;
      inputs = lib.foldl' (acc: d: acc // d.value) { } decls;

      # input name -> the aspect that declared it, and aspect -> its declared
      # tags. Both come off the entry records' `identity`, so a tag declared by
      # one aspect is never attributed to another's inputs -- which is what
      # produced names like `darwin-dev-gui-homebrew`.
      owner = lib.foldl' (
        acc: d:
        acc // lib.genAttrs (builtins.attrNames d.value) (_: d.entry.identity or "<anon>")
      ) { } decls;

      # Per-input overrides: `inputLayers.crane = [ ]` pins crane to the root even
      # though `rust` declares `dev`, because celler/kanata/age-plugin-gpg all
      # `follows` it and live at root. An empty list means root.
      overrideBy = lib.foldl' (
        acc: e: acc // { ${e.identity or "<anon>"} = evalEntryIn scope e; }
      ) { } ((sci.${scope} or { }).inputLayers or [ ]);

      declaredBy = lib.listToAttrs (
        map (e: {
          name = e.identity or "<anon>";
          value =
            let
              v = e.module or e;
            in
            if builtins.isList v then lib.filter builtins.isString v else [ ];
        }) ((sci.${scope} or { }).layers or [ ])
      );
    });

  # Placement: which layer each input's flake.nix gets written to.
  #
  # An input belongs in layer L only if EVERY host declaring it is in L, so the
  # rule is the intersection of the declaring hosts' layer sets -- darwin n nixos
  # is empty, which is the root, and darwin n darwin is `darwin`. This is the
  # same rule `layerOf` was tested against, now fed by real per-host data instead
  # of a routing path.
  # The generated placement, read as plain DATA.
  #
  # This is what evaluation uses: `layerPlan` below walks the aspect tree and is
  # only ever forced by the generator. A build reads this file instead, so no
  # host evaluation traverses aspects to work out where an input is locked --
  # faster, and it is why placement cannot recurse into the fixpoint it feeds.
  # Same arrangement as ../../patched-inputs.nix.
  config.flake.layerPlan =
    builtins.seq _evalProbe
    (let
      byHost = config.flake.scopeInputs;
      hostNames = builtins.attrNames byHost;
      declaring = n: lib.filter (h: (byHost.${h}.inputs or { }) ? ${n}) hostNames;
      allNames = lib.unique (
        lib.concatMap (h: builtins.attrNames (byHost.${h}.inputs or { })) hostNames
      );

      # Declared layers, read from the same capture as inputs. `layers` is class
      # content so the entry's module IS the declared list -- no evalModules, no
      # option forcing, no recursion.
      # Tags declared by the aspect that declared THIS input, not every aspect
      # that happens to share a scope with it.
      declaredFor =
        n:
        lib.unique (
          lib.concatMap (
            h:
            let
              sc = byHost.${h};
              owner = sc.owner.${n} or null;
            in
            if owner == null then [ ] else sc.declaredBy.${owner} or [ ]
          ) (declaring n)
        );

      # NOTE (superseded): `layers` is an option on
      # den.schema.aspect, and reading it back through a capture forces the very
      # aspect fixpoint the capture is resolving -- infinite recursion, confirmed
      # both with a second captureFleet and with a single shared one.
      #
      # So placement is inheritance-only for now: an input belongs to the layers
      # every host declaring it has in common. Declared tags (`gui` on coolabah,
      # `dev` on idris, `homebrew`) need a source that is not derived from the
      # evaluation they feed -- either carried in the input spec itself, or
      # written to a generated file alongside layer-bound.nix.

      overrideFor =
        n:
        let
          hits = lib.concatMap (
            h:
            let
              sc = byHost.${h};
              owner = sc.owner.${n} or null;
              ov = if owner == null then null else (sc.overrideBy.${owner} or { }).${n} or null;
            in
            if ov == null then [ ] else [ ov ]
          ) (declaring n);
        in
        if hits == [ ] then null else lib.head hits;

      placementOf =
        n:
        let
          sets = map (h: byHost.${h}.layers) (declaring n);
        in
        if overrideFor n != null then
          lib.filter builtins.isString (overrideFor n)
        else
        # UNION of what the aspect declares and what its hosts imply. An aspect
        # says what it IS (`gui`, `dev`, `homebrew`); the platform falls out of
        # which hosts include it. So `coolabah` declaring `gui`, included only by
        # darwin hosts, becomes darwin-gui without restating the platform.
        lib.unique (
          declaredFor n
          ++ (if sets == [ ] then [ ] else lib.foldl' lib.intersectLists (lib.head sets) (lib.tail sets))
        );

      specOf = n: (byHost.${lib.head (declaring n)}.inputs).${n};

      followsTargets =
        spec: lib.filter (f: f != null) (lib.mapAttrsToList (_: v: v.follows or null) (spec.inputs or { }));
      referrers = n: lib.filter (m: lib.elem n (followsTargets (specOf m))) allNames;

      # An input has to be at least as widely available as everything that
      # FOLLOWS it.
      #
      # `nur` is declared beside `zen-browser` on a `gui` aspect that only darwin
      # hosts include, so on declaration alone it lands in `darwin-gui`. But
      # `stylix` lives in `gui` -- nixos hosts reach it too -- and follows `nur`.
      # Left in `darwin-gui`, a nixos host resolving stylix reaches into a darwin
      # layer, which is the thing the layering exists to prevent.
      #
      # So intersect an input's own tags with its referrers': {darwin,gui} ∩
      # {gui} = {gui}. Exactly the rule already applied across hosts, with a
      # follows treated as another consumer. An input followed from two disjoint
      # layers intersects to [ ] and becomes shared, which is correct.
      #
      # `converge` because refining one input can refine another through a
      # follows chain; intersection only ever shrinks over a finite domain, so it
      # terminates.
      tagsFixpoint =
        let
          step =
            cur:
            lib.mapAttrs (
              n: t:
              let
                rs = referrers n;
              in
              if rs == [ ] then t else lib.foldl' lib.intersectLists t (map (m: cur.${m}) rs)
            ) cur;
        in
        lib.converge step (lib.genAttrs allNames placementOf);

      rawTags = n: tagsFixpoint.${n};
      allTagSets = lib.unique (map rawTags allNames);

      # Drop tags that carry no information. `homebrew` appears in exactly one
      # tag set, so the `darwin` beside it distinguishes nothing and the layer is
      # simply `homebrew`. `gui` appears in darwin-gui AND gui-nixos, so there
      # the platform is the only thing telling them apart and it stays.
      setsWith = t: lib.filter (st: lib.elem t st) allTagSets;
      uniqueTags = st: lib.filter (t: lib.length (setsWith t) == 1) st;
      flatten = st: let u = uniqueTags st; in if st == [ ] then [ ] else if u != [ ] then u else st;

      # A name another layer `follows` is NOT hoisted to the root. A `follows`
      # cannot cross a flake boundary, so the referring layer declares a pinned
      # copy -- but at merge ../../lib/inputs.nix resolves that copy to whichever
      # source OWNS the name, layer or root alike. Shared does not mean root.
      target = n: let p = flatten (rawTags n); in if p == [ ] then "root" else layerName p;
      names = lib.unique (map target allNames);
    in
    lib.genAttrs names (t: lib.genAttrs (lib.filter (n: target n == t) allNames) specOf));



  # The text of each `partitions/<layer>/flake.nix`, rendered in Nix rather than
  # re-serialised from JSON in the shell -- flake-file's own `inputsExpr` is what
  # produces the exact shape a flake.nix needs.
  #
  # `carried`: a `follows` cannot name an input outside its own flake, so every
  # follows-target a layer's inputs reference has to be declared in the layer
  # too, pinned to its OWNER's locked rev -- which may be the root's lock or
  # another layer's. Their own `follows` are dropped (they would dangle in
  # turn), and ../../lib/inputs.nix resolves the carried node to the owning
  # source at merge, so the copy exists but is never what a host resolves.
  config.flake.layerFlakeText =
    let
      render =
        layer: specs:
        let
          own = lib.mapAttrs (_: declOnly) specs;
          carriedNames = carriedFor own;
          # A STUB, not a pinned copy of the real input.
          #
          # A `follows` cannot cross a flake boundary, so a name this layer only
          # refers to has to be declared here -- but it does not have to be the
          # real thing. `flake = false` on a local path costs no fetch when the
          # layer is locked, and records no rev, so there is no second pin that
          # can drift from the owner's. At merge ../../lib/inputs.nix replaces
          # the node wholesale with the value from whichever source owns the
          # name, so `nixpkgs` is fetched once and evaluated once no matter how
          # many layers refer to it.
          #
          # ONE stub for the whole repo -- ../../lib/stub -- shared by every
          # layer and by the root.
          #
          # Nix keys a root input's lock node by the input NAME, not by content,
          # so every input pointing here still gets its own node (`nixpkgs`,
          # `crane`, ...) and the merge's node -> name mapping stays
          # unambiguous. Verified, not assumed.
          #
          # Reaching out of the layer directory works because the layer is
          # locked by BARE PATH inside the git repo, so the store path is the
          # whole repo and ../../lib/stub stays within it. Locking with a `path:`
          # url instead copies only the layer directory, and then nix rejects
          # this with "relative path points outside of its parent's store path".
          carried = lib.genAttrs carriedNames (_: {
            url = "path:../../lib/stub";
            flake = false;
          });
        in
        ''
          # GENERATED by `nix run .#write-flake` -- do not edit by hand.
          #
          # Holds this layer's inputs and their lock, nothing else. The root reads
          # this lock as DATA (lib/inputs.nix) and never calls `outputs`.
          #
          # Entries with no `follows` of their own are CARRIED: a `follows` cannot
          # cross a flake boundary, so a name this layer only refers to still has
          # to be declared here, pinned to its owner's locked rev. At merge those
          # resolve to whichever source OWNS the name -- root or another layer --
          # so the node below exists but is never the value anything resolves.
          {
            outputs = _: { };

            inputs = ${
              ffLib.nixCode {
                expr = ffLib.inputsExpr (own // carried);
                styles = layerStyles;
              }
            };
          }
        '';
    in
    # From `layerPlan`, not `flake-file-layers`. That option is filled by a
    # ROUTE, and the `inputs` class is deliberately unrouted (./flake-file.nix)
    # -- so it is always empty, and rendering from it produced no layers at all.
    # Placement is the plan's job; this only serialises it.
    lib.mapAttrs render (builtins.removeAttrs config.flake.layerPlan [ "root" ]);


  # Root's serialized inputs: everything the plan does not place in a layer,
  # computed from the same evaluation that places them. No file is read and no
  # source is parsed -- placement is a property of the aspects, so it is derived,
  # not recorded.
  #
  # Additive, never pruning: an input placed in a layer was never routed here in
  # the first place. The `inputs` class is deliberately unrouted (see
  # ./flake-file.nix), so without this step converted aspects would contribute
  # nothing to the root flake.nix at all -- which is how `crane` went missing and
  # left `age-plugin-gpg/crane follows a non-existent input`, a state nix rejects
  # outright and that cannot be repaired by regenerating, because the flake no
  # longer evaluates.
  #
  # A root input following a name that lives in a layer still needs SOMETHING to
  # resolve against, since a `follows` cannot cross a flake boundary. Those get
  # the shared stub: `flake = false` on a local path, costing no fetch, which
  # ../../lib/inputs.nix replaces with the owning source's real input at merge.
  config.flake-file.preProcess =
    serialized:
    let
      plan = config.flake.layerPlan;
      placed = lib.concatMap builtins.attrNames (
        builtins.attrValues (builtins.removeAttrs plan [ "root" ])
      );
      # `serialized` is already normalised (flake-file hands preProcess
      # `inputsExpr flake-file.inputs`); the plan's specs are raw aspect
      # declarations and have to go through the same normalisation, or they land
      # in flake.nix in whatever shape the aspect happened to write.
      withRoot =
        (builtins.removeAttrs serialized placed)
        // ffLib.inputsExpr (lib.mapAttrs (_: declOnly) (plan.root or { }));

      wanted = lib.unique (lib.concatMap followsOf (lib.attrValues withRoot));
      # The same shared stub the layers use, reached from the root.
      stubs = lib.genAttrs (lib.filter (n: !(withRoot ? ${n})) wanted) (_: {
        url = "path:./lib/stub";
        flake = false;
      });
    in
    withRoot // stubs;

  # Inputs an aspect DECLARES that no lock provides yet.
  #
  # This is the bootstrap state, and it is legitimate: enabling an aspect that
  # declares a new input means the input does not exist until `write-flake` has
  # placed it in a bucket and locked it. The plan can be computed anyway --
  # `layerPlan` captures declarations as DATA and never runs an aspect's body --
  # so the order is: declare, write-flake, build.
  #
  # Between those steps any evaluation that dereferences the input fails, and
  # the failure is opaque ("attribute 'nixvim' missing", from wherever the body
  # happens to touch it). This says it plainly instead, and needs nothing beyond
  # the plan that has already been computed.
  config.flake.missingInputs =
    let
      planned = lib.attrNames (
        lib.foldl' (x: y: x // y) { } (builtins.attrValues config.flake.layerPlan)
      );
    in
    lib.filter (n: !(inputs ? ${n})) planned;

  config.flake.lib.layers = {
    inherit layers;

    # A tag set names a layer; the layer is the INTERSECTION lattice over them.
    #
    #   [ ]                -> the root lock, loaded by everything
    #   [ "nixos" ]        -> partitions/nixos
    #   [ "gui" ]          -> partitions/gui
    #   [ "nixos" "gui" ]  -> partitions/nixos-gui
    #
    # A host loads every layer whose tag set is a SUBSET of its own tags, so
    # `nixos-gui` is picked up by hosts that are both, and by nothing else.
    # That is what lets ../desktop/plasma.nix be nixos-only AND gui-only at once
    # instead of having to pick the less true of the two.
    # An input is locked in a layer only if every host that can reach it is in
    # that layer. Aspects declaring it contribute the layer sets they inherited;
    # anything spanning two layers, or reached by an untagged host, is shared and
    # goes to the root -- widening, never hiding.
    # Layers are declared on the SYSTEM and inherited by everything it
    # includes. A host says `layers = [ "nixos" ]`; every aspect it resolves is
    # thereby a nixos aspect, and so is every input those aspects declare.
    #
    # It does NOT propagate up: an aspect being nixos says nothing about any
    # host, and tagging an aspect never makes a system anything.
    #
    # This is the `idris2Packages` rule made explicit -- "the input set follows
    # which hosts pull this aspect in, and so does its partition" -- and it is
    # why the whole refactor needs one merged environment: working out which
    # hosts resolve an aspect is exactly the `den.hosts` traversal a partition
    # boundary makes impossible.
    #
    # Where an aspect's inputs get locked. Two different jobs, deliberately:
    #
    #   * a DECLARED tag SELECTS the lock outright. ../darwin/homebrew.nix says
    #     `homebrew` and its taps land in partitions/homebrew, whatever hosts
    #     happen to reach it. No combinatorial directories.
    #   * with nothing declared, placement is INHERITED from reachability: the
    #     layers every host reaching the aspect has in common.
    #
    # Inheritance INTERSECTS. An input belongs in layer L only if every host
    # that can reach it is in L -- otherwise a host outside L would need an
    # input it cannot see. Aspects reached from both a darwin and a nixos system
    # intersect to nothing and land in `shared`, which is `git` and is exactly
    # right. `shared` is a layer, not the root: see `layerName`.
    #
    # `byHost`   = { <hostName>   = { layers; aspects = [ <aspectName> ]; }; }
    # `declared` = { <aspectName> = [ <layer> ]; }
    layerFor =
      { byHost, declared ? { } }:
      let
        hosts = lib.attrValues byHost;
        reaching = aspect: lib.filter (h: lib.elem aspect h.aspects) hosts;
        inherited =
          aspect:
          let
            sets = map (h: h.layers) (reaching aspect);
          in
          if sets == [ ] then [ ] else lib.foldl' lib.intersectLists (lib.head sets) (lib.tail sets);
        allAspects = lib.unique (lib.concatMap (h: h.aspects) hosts ++ lib.attrNames declared);
      in
      lib.genAttrs allAspects (
        aspect:
        if (declared.${aspect} or [ ]) != [ ] then
          layerName declared.${aspect}
        else
          layerName (inherited aspect)
      );

    # Hoisting and shadowing are different bugs. An input two layers use IS
    # shared -- that is how crane, celler, stylix and rust-overlay reached the
    # root lock, and `hoistedNames` in ./partitions.nix already does it
    # correctly. What must error is two layers claiming one name with
    # *different* specs, which today resolves arbitrarily.
    shadowed =
      specsByLayer:
      let
        names = lib.unique (lib.concatMap lib.attrNames (lib.attrValues specsByLayer));
        claimsOf =
          name:
          lib.filter (s: s != null) (lib.mapAttrsToList (_: specs: specs.${name} or null) specsByLayer);
        conflicting = name: lib.length (lib.unique (claimsOf name)) > 1;
      in
      lib.filter conflicting names;
  };
}
