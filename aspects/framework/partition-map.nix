{
  options,
  config,
  lib,
  den,
  inputs,
  rootPath,
  ...
}:
# Which bucket each aspect file WOULD go in, derived from den: a file whose
# aspects are pulled in only by Darwin hosts belongs to `darwin`, only by NixOS
# hosts to `nixos`, and anything both platforms pull in stays at base.
#
# Runs in the `all` partition -- the only evaluation that can see every host and
# every input, and therefore the only one that can classify anything. The result
# comes back out as `flake.partitionMap` (partitionedAttrs sends it to `all`).
#
# NOT yet wired to write ../../partition-map.nix, because host reachability is
# the wrong relation for the question the base evaluation actually asks. "Which
# hosts USE this aspect" is not "which files NAME this aspect": aspects/hosts/
# laptop.nix reads `den.aspects.browsers.zen` at base even on a run where no
# base host uses it, so moving browsers/zen.nix out on reachability grounds
# fails the base evaluation with
#
#   error: Aspect 'browsers' has no key 'zen' (checked direct and provides)
#
# Inert stubs for every moved aspect (which is what `stubs` below is for) fix
# the plain `attribute '<x>' missing` case but not this one -- base files
# navigate INTO partitioned aspects, not just name them. Closing the gap needs
# a static reference closure over `den.aspects.*` lookups, which den does not
# currently expose. Until then ../../partition-map.nix stays hand-written and
# this is the report that tells you what it is costing you.
let
  diagram = inputs.den-diagram.lib;
  hostAspects = import ../../lib/host-aspects.nix { inherit lib den diagram; };

  allHosts = lib.mergeAttrsList (builtins.attrValues den.hosts);
  # den resolves a `class` ("nixos"/"darwin") onto every host — the authoritative
  # platform, rather than string-matching the system tuple.
  platformOf = host: host.class;

  # platform -> every aspect label any host of that platform actually uses.
  usedBy =
    lib.foldlAttrs
      (
        acc: hostName: host:
        let
          platform = platformOf host;
          names = (hostAspects.forHost hostName host (hostAspects.classesFor host)).names;
        in
        acc // { ${platform} = (acc.${platform} or [ ]) ++ names; }
      )
      {
        darwin = [ ];
        nixos = [ ];
      }
      allHosts;

  usedSet = lib.mapAttrs (_: names: lib.genAttrs names (_: true)) usedBy;

  # The module system already records which file every `den.*` definition came
  # from, so no naming convention is needed to get from an aspect back to the
  # aspect file that has to leave the base import list.
  #
  # A host file declares `den.hosts.<system>.<name>` rather than aspects, and is
  # classified by the systems it declares.
  localDefs = builtins.filter (d: (builtins.match ".*/aspects/.*" d.file) != null) (
    options.den.definitionsWithLocations
  );

  relPath = file: builtins.head (builtins.match ".*/aspects/(.*)" file);

  # A definition may declare a NESTED aspect (`den.aspects.browsers.zen`), which a
  # base file navigates into directly (`den.lib.whenAspect den.aspects.browsers.zen`).
  # A flat `browsers` stub (`{}`) would then lose the `.zen` key and the base
  # evaluation dies with `Aspect 'browsers' has no key 'zen'`. So the stub list
  # carries the FULL path (`browsers/zen`) and partitions.nix rebuilds a nested
  # inert stub. An attrset is a namespace to recurse into only while it carries no
  # aspect content: real content always brings `includes` or one of den's class
  # keys, whereas a namespace's keys are just further aspect names. Functions and
  # empty attrsets are leaves as they stand.
  aspectContentKeys = [
    "includes"
    "provides"
    "meta"
    "den"
    "when"
    "systems"
    "nixos"
    "darwin"
    "homeManager"
    "homeModules"
    "home"
    "overlays"
    "inputs"
    "flake-file"
    "ff"
    "flake-mod"
    "perSystem"
    "flake"
    "packages"
    "os"
  ];
  aspectLeafPaths =
    prefix: v:
    if
      !(builtins.isAttrs v)
      || v == { }
      || builtins.any (k: builtins.elem k aspectContentKeys) (builtins.attrNames v)
    then
      [ (lib.concatStringsSep "/" prefix) ]
    else
      builtins.concatMap (name: aspectLeafPaths (prefix ++ [ name ]) v.${name}) (builtins.attrNames v);

  contributions = lib.foldl' (
    acc: d:
    let
      path = relPath d.file;
      prev =
        acc.${path} or {
          aspects = [ ];
          nestedAspects = [ ];
          platforms = [ ];
        };
      declared = d.value.aspects or { };
    in
    acc
    // {
      ${path} = {
        aspects = prev.aspects ++ builtins.attrNames declared;
        nestedAspects =
          prev.nestedAspects
          ++ builtins.concatMap (name: aspectLeafPaths [ name ] declared.${name}) (builtins.attrNames declared);
        # A file's raw `den.hosts.<system>.<name>` definition carries no resolved
        # `class` (that is den-injected), so look the host up in the resolved
        # `allHosts` by name to read its authoritative class.
        platforms =
          prev.platforms
          ++ map (name: platformOf allHosts.${name}) (
            builtins.concatMap builtins.attrNames (builtins.attrValues (d.value.hosts or { }))
          );
      };
    }
  ) { } localDefs;

  # A file joins a bucket only if everything it contributes belongs to that one
  # platform. Anything shared, or used by no system at all, stays at base: it
  # costs a root input, but moving it would break the other platform's hosts.
  bucketOf =
    contribution:
    let
      # A file that declares hosts is classified by those hosts' platforms ALONE.
      # The declared system is authoritative, and the file's own aspects are
      # host-specific (they must travel with the host regardless). Folding in
      # aspect-usage here would let reachability's cross-platform over-approximation
      # (a nixos host's trace still lists darwin host aspects, hence classSlice)
      # mark a darwin host file multi-platform and strand it at base -- which then
      # breaks that host's den fleet spawn (`spawnNode spawn root equals its parent
      # scope`). Aspect-usage classification is only for files that declare no host.
      platforms =
        if contribution.platforms != [ ] then
          lib.unique contribution.platforms
        else
          lib.unique (
            builtins.concatMap (
              name:
              builtins.filter (p: usedSet.${p} ? ${name}) [
                "darwin"
                "nixos"
              ]
            ) contribution.aspects
          );
      touchesNothing = contribution.aspects == [ ] && contribution.platforms == [ ];
    in
    if touchesNothing || builtins.length platforms != 1 then null else builtins.head platforms;

  # aspects/framework/* is the machinery the root flake itself runs on -- it is
  # what defines `flake-file`, den's schema and the partitions. Several of those
  # files also declare a token aspect, which is enough for the rule above to call
  # them single-platform (flakeExtra.nix classifies as darwin), and partitioning
  # any of them takes the whole evaluation down. They are never derived; a
  # framework file that genuinely belongs in a bucket goes in `manual`.
  neverDerive = path: lib.hasPrefix "framework/" path;

  derived = lib.foldlAttrs (
    acc: path: contribution:
    let
      bucket = if neverDerive path then null else bucketOf contribution;
    in
    if bucket == null then acc else acc // { ${bucket} = (acc.${bucket} or [ ]) ++ [ path ]; }
  ) { } contributions;
  # Files that contribute no den content at all -- raw `flake`/`perSystem`
  # modules -- have no aspect graph to classify them, so they are named here.
  # Everything else is derived.
  manual.dev = [
    "tooling/deploy.nix"
    "tooling/ci-matrix.nix"
    "docs/default.nix"
    "framework/partition-map.nix"
  ];
  # tooling/extraPackages.nix is nothing but `imports = ../../packages |> ...`,
  # so it contributes no den content of its own and cannot be derived either.
  manual.packages = [ "tooling/extraPackages.nix" ];

  # Which buckets are evaluated with another bucket's aspects imported. Not
  # derived -- it is a statement about what the partition mechanism has to carry,
  # not about who uses what. Every host needs the ./packages overlays, and hosts
  # live in the platform buckets. See ../../partition-map.nix and partitions.nix.
  deps = {
    darwin = [ "packages" ];
    dev = [ "packages" ];
    nixos = [ "packages" ];
  };

  bucketMap = lib.mapAttrs (_: lib.sort (a: b: a < b)) (
    lib.zipAttrsWith (_: lib.concatLists) [
      derived
      manual
    ]
  );

  # Every aspect name a partitioned file defines. A file that stays at base may
  # still write `den.aspects.<x>` for one of them -- reachability says no NixOS
  # host uses karabiner-driver, but a base file still names it -- so base gets an
  # inert stub for each. Without this the base evaluation dies with
  # `attribute '<x>' missing` as soon as a definition moves into a bucket.
  #
  # The `packages` bucket needs a second source. Its files live in ./packages,
  # outside the aspect tree, so `localDefs` filters them out and the path-based
  # walk above finds nothing to stub -- while a dozen base aspects do write
  # `den.aspects.packages.<name>`. Read those definitions straight off their own
  # `den.*` locations instead.
  packageStubs = builtins.concatMap (
    d:
    let
      declared = d.value.aspects or { };
    in
    builtins.concatMap (name: aspectLeafPaths [ name ] declared.${name}) (builtins.attrNames declared)
  ) (builtins.filter (d: (builtins.match ".*/packages/.*" d.file) != null) options.den.definitionsWithLocations);

  stubs = lib.sort (a: b: a < b) (
    lib.unique (
      builtins.concatMap (path: (contributions.${path} or { nestedAspects = [ ]; }).nestedAspects) (
        builtins.concatLists (builtins.attrValues bucketMap)
      )
      ++ lib.optionals (bucketMap ? packages) packageStubs
    )
  );
  # ── Serialize the derived map back to ../../partition-map.nix ──────────────
  # `nix run .#write-partition-map` writes the file the base flake reads. The
  # classification is reachability-derived (see the header): it is a STARTING
  # POINT, not guaranteed drop-in. A file that stays at base may still navigate
  # INTO a moved aspect (`den.aspects.browsers.zen`), which stubs cannot repair,
  # so after regenerating always re-run `nix run .#write-flake` and confirm the
  # base evaluation still succeeds before committing.
  fileHeader = ''
    # GENERATED by `nix run .#write-partition-map` -- reconcile by hand if the base
    # evaluation breaks (see aspects/framework/partition-map.nix for why the derived
    # classification is a starting point, not a guarantee).
    #
    # Which aspects live in a flake-parts partition instead of the base evaluation.
    # An aspect listed here is NOT imported by the root flake, so its `ff.*` input
    # declarations never reach the root `flake.nix`/`flake.lock`; they are written to
    # `partitions/<bucket>/flake.nix` instead. See aspects/framework/partitions.nix.
    #
    # Entries are paths relative to ./aspects. A directory entry claims everything
    # under it. `stubs` are the aspect names bucketed files define, stubbed inert at
    # base so a base file naming a moved aspect still resolves. `deps` are buckets
    # evaluated with another bucket's aspects imported as well.'';

  bucketBlock =
    name: paths:
    "    ${name} = [\n${lib.concatMapStringsSep "\n" (p: "      \"${p}\"") paths}\n    ];";

  depsBlock =
    name: names: "    ${name} = [ ${lib.concatMapStringsSep " " (n: "\"${n}\"") names} ];";

  partitionMapText = ''
    ${fileHeader}
    {
      buckets = {
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList bucketBlock bucketMap)}
      };

      deps = {
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList depsBlock deps)}
      };

      stubs = [
    ${lib.concatMapStringsSep "\n" (s: "    \"${s}\"") stubs}
      ];
    }
  '';
in
{
  # Advisory: the classification den can derive, NOT the map in force. See the
  # header comment for why the two are not the same thing yet.
  #   nix eval .#partitionMap.map --json | jq
  flake.partitionMap = {
    inherit stubs deps;
    map = bucketMap;
  };

  perSystem =
    { pkgs, ... }:
    {
      apps.write-partition-map = {
        type = "app";
        program = lib.getExe (
          pkgs.writeShellApplication {
            name = "write-partition-map";
            runtimeInputs = [ pkgs.coreutils ];
            text = ''
              set -euo pipefail
              repo="$PWD"
              if [ ! -e "$repo/flake.nix" ]; then
              	echo "error: run from the flake root (no flake.nix in $repo)" >&2
              	exit 2
              fi
              cat >"$repo/partition-map.nix" <<'PARTITIONMAP'
              ${partitionMapText}
              PARTITIONMAP
              echo "wrote $repo/partition-map.nix"
              echo "now run 'nix run .#write-flake' and confirm the base evaluation still succeeds." >&2
            '';
          }
        );
      };
    };
}
