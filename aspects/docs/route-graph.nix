{
  den,
  lib,
  capabilityGraph,
  routeLib,
  ...
}:
# ── ROUTES AND SEGMENTS, AS A den-diagram GRAPH ──
#
# The same IR ./capability-graph.nix overlays onto a host graph, so
# den-diagram's own renderers draw it: hosts and containers are entity
# instances (subgraphs), segments sit outside them, a carrier's route is a
# `provide` edge to the segment it reaches, and every route take shows the
# route it won and each one that lost with the clause it lost on -- the
# verdicts `den.lib.capability.judge` gave, read from `capabilityGraph.resolutionsFor`.
let
  inherit (capabilityGraph) mkNode mkEdge;
  hosts = lib.concatMapAttrs (_: hs: hs) den.hosts;
  id = s: lib.replaceStrings [ "-" "." "@" "/" ":" " " ] [ "_" "_" "_" "_" "_" "_" ] s;
  inst = kind: name: "${kind}:${name}";

  # ── WHERE A SEGMENT LIVES ──
  # One that spans hosts is one node in the shared network group; one that
  # stops at a host or a machine (loopback, the bridges) is a node per machine,
  # inside that machine's group. A segment named only by a route (`internet`)
  # has no aspect and spans everything.
  machineOf = h: lib.defaultTo h.name (h.parent or null);
  spansOf = s: (den.aspects.${s} or { }).spans or "many-hosts";
  shared = s: spansOf s == "many-hosts";
  # A container's own group, a host's own group.
  kindOf =
    h:
    {
      true = "container";
      false = "host";
    }
    .${lib.boolToString ((h.parent or null) != null)};
  segAt =
    h: s:
    {
      many-hosts = {
        id = "seg_${id s}";
        instance = inst "network" "fleet";
      };
      one-machine = {
        id = "seg_${id s}_${id (machineOf h)}";
        instance = inst "host" (machineOf h);
      };
      one-host = {
        id = "seg_${id s}_${id h.name}";
        instance = inst (kindOf h) h.name;
      };
    }
    .${spansOf s};
  segId = h: s: (segAt h s).id;

  linkOf =
    h: aspect:
    den.lib.capability.linkFor aspect "route" {
      host = h;
      peer = h;
    };

  entityNodes =
    kind: name: h:
    let
      at = {
        entityKind = kind;
        entityInstance = inst kind name;
      };
      routes = den.lib.capability.providersFor h "route";
      takes = lib.filter (r: r.cap == "route") (capabilityGraph.resolutionsFor { host = h; });
      plan = routeLib.tailnetPlan h;
      self = "${id name}__self";
      routeNode = c: "${id name}__route_${id c}";
      takeNode = t: "${id name}__take_${id t}";
      tags =
        l:
        lib.concatStringsSep ", " (
          lib.optional l.private "private"
          ++ lib.optional (l.table != null) "table ${toString l.table}"
          ++ lib.optional l.advertise "advertised"
        );
    in
    {
      nodes = [
        (mkNode (
          at
          // {
            id = self;
            label = name;
          }
        ))
      ]
      ++ lib.mapAttrsToList (
        c: _:
        mkNode (
          at
          // {
            id = routeNode c;
            label = "route: ${c}";
            shape = "stadium";
          }
        )
      ) routes
      ++ map (
        r:
        mkNode (
          at
          // {
            id = takeNode r.taker;
            label = "${r.taker} takes a route";
            shape = "trapezoid";
          }
        )
      ) takes;

      edges =
        # On each segment it is attached to.
        lib.mapAttrsToList (
          s: _:
          mkEdge {
            from = self;
            to = segId h s;
            label = "on";
          }
        ) (den.lib.capability.providersFor h "segment")
        # Each route to the segment it reaches.
        ++ lib.mapAttrsToList (
          c: aspect:
          let
            l = linkOf h aspect;
          in
          mkEdge {
            from = routeNode c;
            to = segId h l.segment;
            style = "provide";
            # A `provide` edge always has a label; an untagged route says so.
            label = lib.findFirst (t: t != "") "route" [ (tags l) ];
          }
        ) routes
        # Each take: the route that won, and every one that lost and why.
        ++ lib.concatMap (
          r:
          lib.optional (r.who.winner != null) (mkEdge {
            from = takeNode r.taker;
            to = routeNode r.who.winner.giver;
            style = "provide";
            label = "won";
          })
          ++ map (
            x:
            mkEdge {
              from = takeNode r.taker;
              to = routeNode x.giver;
              style = "excluded";
              label = lib.concatStringsSep "; " x.why;
            }
          ) (lib.filter (x: routes ? ${x.giver}) r.who.rejected)
        ) takes
        # What it carries for the tailnet.
        ++ map (
          p:
          mkEdge {
            from = self;
            to = segId h "tailscale";
            style = "provide";
            label = "advertises ${p}";
          }
        ) plan.advertiseRoutes
        ++ lib.optional plan.advertiseExit (mkEdge {
          from = self;
          to = segId "tailscale";
          style = "provide";
          label = "exit node";
        })
        ++ lib.optional (plan.exitNode != null) (mkEdge {
          from = self;
          to = "${id plan.exitNode}__self";
          style = "provide";
          label = "uses exit node";
        });
    };

  containersOf = h: lib.attrValues (h.containers or { });

  parts =
    lib.mapAttrsToList (n: h: entityNodes "host" n h) hosts
    ++ lib.concatMap (h: map (c: entityNodes "container" c.name c) (containersOf h)) (
      lib.attrValues hosts
    );

  everyone = lib.attrValues hosts ++ lib.concatMap containersOf (lib.attrValues hosts);
  segments = lib.unique (
    lib.concatMap (
      h:
      map (s: segAt h s // { label = s; }) (
        lib.attrNames (den.lib.capability.providersFor h "segment")
        ++ lib.mapAttrsToList (_: a: (linkOf h a).segment) (den.lib.capability.providersFor h "route")
      )
    ) everyone
  );

  graph = {
    rootName = "routes";
    rootId = "routes";
    direction = "LR";
    entityKinds = [ ];
    entityEdges = [ ];
    entityInstances =
      lib.mapAttrsToList (n: _: {
        id = "inst_${id n}";
        kind = "host";
        name = n;
        label = n;
        parent = null;
      }) hosts
      ++ [
        {
          id = "inst_network";
          kind = "network";
          name = "fleet";
          label = "shared segments";
          parent = null;
        }
      ]
      ++ lib.concatMap (
        h:
        map (c: {
          id = "inst_${id c.name}";
          kind = "container";
          name = c.name;
          label = "container: ${c.name}";
          parent = "inst_${id h.name}";
        }) (containersOf h)
      ) (lib.attrValues hosts);
    nodes =
      map (
        s:
        mkNode {
          inherit (s) id;
          label = "segment: ${s.label}";
          shape = "hexagon";
          entityKind = lib.head (lib.splitString ":" s.instance);

          entityInstance = s.instance;
        }
      ) segments
      ++ lib.concatMap (p: p.nodes) parts;
    edges = lib.concatMap (p: p.edges) parts;
  };
in
{
  _module.args.routeGraph = { inherit graph; };
}
