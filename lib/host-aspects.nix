# The set of aspects a host ACTUALLY uses, as display labels.
#
# `den.lib.capture` traces the whole aspect tree for every host regardless of
# whether an aspect applies to it -- a plain NixOS host's raw trace still lists
# darwin-base, darwin-finder, pam-touchid and friends, identically across every
# host. Anything deriving "is this aspect used by a NixOS system?" from the raw
# trace therefore concludes "everything is used by everything".
#
# den-diagram's `classSlice` is the filter for exactly this: it keeps a node if
# it -- or anything under it -- actually contributes to a given class, via
# ancestor closure, so organisational nodes that anchor real content are kept
# rather than left dangling. This is the same pipeline aspects/docs/default.nix
# renders its per-host diagrams from; both call in here so the two cannot drift.
{
  lib,
  den,
  diagram,
}:
{
  # Classes traced for a host, keyed off its system.
  classesFor =
    host:
    if lib.hasSuffix "darwin" (host.system or "") then
      [
        "darwin"
        "homeManager"
      ]
    else
      [
        "nixos"
        "homeManager"
      ];

  # The filtered aspect graph for one host. Callers that only want names take
  # `.names`; aspects/docs/default.nix renders `.graph`.
  forHost =
    hostName: host: classes:
    let
      captured = den.lib.capture.captureWithPathsWith {
        inherit classes;
        root = den.lib.resolveEntity "host" { inherit host; };
        ctx = { inherit host; };
      };

      g = diagram.context {
        entries = captured.entries;
        ctxTrace = captured.ctxTrace;
        name = hostName;
      };

      # `filterUserAspects` rather than `simplified`: the latter drops every
      # edge tagged style = "provide", which for several nodes is their only
      # connection, leaving them as disconnected floating nodes.
      gBase = diagram.graph.filterUserAspects g;

      isPolicyNode = n: (builtins.match "<policy:.*" n.label) != null;
      policyKeptIds = lib.listToAttrs (
        map (n: {
          name = n.id;
          value = true;
        }) (builtins.filter (n: !(isPolicyNode n)) gBase.nodes)
      );
      gNoPolicy = gBase // {
        nodes = builtins.filter (n: policyKeptIds ? ${n.id}) gBase.nodes;
        edges = builtins.filter (e: policyKeptIds ? ${e.from} && policyKeptIds ? ${e.to}) gBase.edges;
      };

      # classSlice takes one class at a time, so union the slices across the
      # host's traced classes.
      perClassSlices = map (c: diagram.graph.classSlice c gNoPolicy) classes;
      keptIds = lib.foldl' (
        acc: slice: lib.foldl' (acc': n: acc' // { ${n.id} = true; }) acc slice.nodes
      ) { } perClassSlices;

      graph = gNoPolicy // {
        nodes = builtins.filter (n: keptIds ? ${n.id}) gNoPolicy.nodes;
        edges = builtins.filter (e: keptIds ? ${e.from} && keptIds ? ${e.to}) gNoPolicy.edges;
      };
    in
    {
      inherit graph;
      # Host/user scoped duplicates (e.g. "shell" applied at both host and user
      # level) collapse to one entry since they share a label; the root itself
      # is excluded.
      names = lib.unique (map (n: n.label) (builtins.filter (n: n.id != graph.rootId) graph.nodes));
    };
}
