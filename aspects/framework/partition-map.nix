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
  platformOf = host: if lib.hasSuffix "darwin" (host.system or "") then "darwin" else "nixos";

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

  contributions = lib.foldl' (
    acc: d:
    let
      path = relPath d.file;
      prev =
        acc.${path} or {
          aspects = [ ];
          platforms = [ ];
        };
    in
    acc
    // {
      ${path} = {
        aspects = prev.aspects ++ builtins.attrNames (d.value.aspects or { });
        platforms =
          prev.platforms
          ++ map platformOf (
            builtins.concatMap builtins.attrValues (builtins.attrValues (d.value.hosts or { }))
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
      platforms = lib.unique (
        contribution.platforms
        ++ builtins.concatMap (
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
  stubs = lib.sort (a: b: a < b) (
    lib.unique (
      builtins.concatMap (path: (contributions.${path} or { aspects = [ ]; }).aspects) (
        builtins.concatLists (builtins.attrValues bucketMap)
      )
    )
  );
in
{
  # Advisory: the classification den can derive, NOT the map in force. See the
  # header comment for why the two are not the same thing yet.
  #   nix eval .#partitionMap.map --json | jq
  flake.partitionMap = {
    inherit stubs;
    map = bucketMap;
  };
}
