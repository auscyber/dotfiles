{
  den,
  lib,
  capabilityGraph,
  ...
}:
# The whole fleet as one mermaid flowchart, written directly rather than
# through den-diagram's IR, which can only label `provide` edges.
#
#   machine, container   subgraph (a container inside its host)
#   segment              hexagon; a machine-local one inside its machine
#   service              node, only where a link touches it
#   attachment           undirected line, machine --- segment, labelled address
#   capability link      edge, taker --> giver, labelled `cap · carrier`
#   route                thick edge, machine ==> segment; a route take is
#                        taker ==> segment `via <route>`
let
  cap = den.lib.capability;
  entities = cap.hostsOf;
  isContainer = e: (e.parent or null) != null;
  machines = lib.filterAttrs (_: e: !isContainer e) entities;
  containersOf = m: lib.filterAttrs (_: e: e.parent or null == m) entities;

  tryOr =
    d: v:
    let
      r = builtins.tryEval v;
    in
    if r.success then r.value else d;

  id = s: lib.replaceStrings [ "-" "." "@" "/" ":" " " ] [ "_" "_" "_" "_" "_" "_" ] s;
  q = s: ''"${lib.replaceStrings [ ''"'' "|" ] [ "#quot;" "/" ] (toString s)}"'';

  entId = n: "m_${id n}";
  svcId = e: a: "${id e}__${id a}";

  # ── SEGMENTS ──
  spansOf = s: (den.aspects.${s} or { }).spans or "many-hosts";
  prefixOf = s: (den.aspects.${s} or { }).prefix or null;
  machineOf = e: lib.defaultTo e.name (e.parent or null);
  segId =
    e: s:
    {
      many-hosts = "seg_${id s}";
      one-machine = "seg_${id s}_${id (machineOf e)}";
      one-host = "seg_${id s}_${id e.name}";
    }
    .${spansOf s};

  # Loopback is every entity's, so it is not drawn: a local link says so.
  segmentsOf = e: removeAttrs (tryOr { } (cap.providersFor e "segment")) [ "loopback" ];

  addressOn =
    e: a:
    let
      l = tryOr null (
        cap.linkFor a "tcp" {
          host = e;
          peer = e;
        }
      );
    in
    if l == null then null else l.address or null;

  # ── ROUTES ──
  routesOf =
    e:
    lib.mapAttrs (
      _: a:
      tryOr null (
        cap.linkFor a "route" {
          host = e;
          peer = e;
        }
      )
    ) (tryOr { } (cap.providersFor e "route"));

  routeLabel =
    n: l:
    lib.concatStringsSep " · " (
      [ n ]
      ++ lib.optional (l.table or null != null) "table ${toString l.table}"
      ++ lib.optional (l.private or false) "private"
      ++ lib.optional (l.advertise or false) "advertised"
    );

  # ── LINKS ──
  resolutions = lib.concatMap (
    e:
    map (r: r // { from = e; }) (
      lib.filter (r: tryOr false (r.who.winner != null && (r.cap != "route" || r.how != null))) (
        tryOr [ ] (capabilityGraph.resolutionsFor { host = e; })
      )
    )
  ) (lib.attrValues (lib.filterAttrs (_: e: (e.id_hash or "") != "external:${e.name}") entities));

  routeTakes = lib.filter (r: r.cap == "route") resolutions;
  links = lib.filter (r: r.cap != "route") resolutions;

  carrierOf = r: if r.who.winner.sameHost then "local" else (r.how.winner.carrier or "?");

  # Every node a link touches, keyed by entity.
  services = lib.foldl' (
    acc: r:
    lib.recursiveUpdate (lib.recursiveUpdate acc { ${r.from.name}.${r.taker} = true; }) {
      ${r.who.winner.on}.${r.who.winner.giver} = true;
    }
  ) { } (links ++ routeTakes);

  # ── RENDERING ──
  serviceLines =
    n:
    lib.mapAttrsToList (a: _: "  ${svcId n a}[${q a}]:::svc") (
      removeAttrs (services.${n} or { }) (lib.attrNames (routesOf entities.${n}))
    );

  localSegments =
    e:
    lib.mapAttrsToList (s: _: "  ${segId e s}{{${q s}}}:::seg") (
      lib.filterAttrs (s: _: spansOf s != "many-hosts") (segmentsOf e)
    );

  containerBlock =
    c:
    [
      "  subgraph ${entId c.name}[${q "${c.name} · container"}]"
      "  direction TB"
    ]
    ++ serviceLines c.name
    ++ [ "  end" ];

  machineBlock =
    n: m:
    [
      "subgraph ${entId n}[${q "${n} · ${m.class or "external"}"}]"
      "  direction TB"
    ]
    ++ serviceLines n
    ++ localSegments m
    ++ lib.concatMap containerBlock (lib.attrValues (containersOf n))
    ++ [ "end" ];

  sharedSegments = lib.unique (
    lib.concatMap (e: lib.filter (s: spansOf s == "many-hosts") (lib.attrNames (segmentsOf e))) (
      lib.attrValues entities
    )
    ++ lib.concatMap (
      e:
      lib.filter (s: spansOf s == "many-hosts") (
        map (l: l.segment) (lib.filter (l: l != null) (lib.attrValues (routesOf e)))
      )
    ) (lib.attrValues entities)
  );

  networkBlock = [
    "subgraph net[${q "networks"}]"
    "  direction TB"
  ]
  ++ map (
    s:
    "  seg_${id s}{{${
        q (lib.concatStringsSep "<br/>" ([ s ] ++ lib.optional (prefixOf s != null) (prefixOf s)))
      }}}:::seg"
  ) sharedSegments
  ++ [ "end" ];

  # Edges, each with the style class it is drawn in.
  attachments = lib.concatMap (
    e:
    lib.mapAttrsToList (s: a: {
      line = "${entId e.name} ---|${q (lib.defaultTo s (addressOn e a))}| ${segId e s}";
      style = "attach";
    }) (segmentsOf e)
  ) (lib.attrValues entities);

  routeEdges = lib.concatMap (
    e:
    lib.mapAttrsToList (n: l: {
      line = "${entId e.name} ==>|${q (routeLabel n l)}| ${segId e l.segment}";
      style = "route";
    }) (lib.filterAttrs (_: l: l != null) (routesOf e))
  ) (lib.attrValues entities);

  routeTakeEdges = map (
    r:
    let
      l = (routesOf r.from).${r.who.winner.giver} or null;
    in
    {
      line = "${svcId r.from.name r.taker} ==>|${q "via ${r.who.winner.giver}"}| ${
        if l == null then entId r.who.winner.on else segId r.from l.segment
      }";
      style = "route";
    }
  ) routeTakes;

  linkEdges = map (r: {
    line = "${svcId r.from.name r.taker} -->|${q "${r.cap} · ${carrierOf r}"}| ${svcId r.who.winner.on r.who.winner.giver}";
    style = carrierOf r;
  }) links;

  edges = attachments ++ routeEdges ++ routeTakeEdges ++ linkEdges;

  colours = {
    attach = "stroke:#6c7086,stroke-width:1px";
    route = "stroke:#fab387,stroke-width:3px";
    local = "stroke:#9399b2,stroke-width:1px,stroke-dasharray:3 3";
    lan-home = "stroke:#a6e3a1,stroke-width:2px";
    house-wifi = "stroke:#94e2d5,stroke-width:2px";
    tailscale = "stroke:#89b4fa,stroke-width:2px";
    nspawn-bridge = "stroke:#f9e2af,stroke-width:2px";
    container-bridge = "stroke:#f9e2af,stroke-width:2px";
    publish-ivymect-in = "stroke:#cba6f7,stroke-width:2px";
  };

  linkStyles = lib.imap0 (
    i: e: "linkStyle ${toString i} ${colours.${e.style} or "stroke:#f38ba8,stroke-width:2px"}"
  ) edges;

  mermaid = lib.concatStringsSep "\n" (
    [
      ''%%{init: {"flowchart": {"curve": "basis", "nodeSpacing": 24, "rankSpacing": 70}}}%%''
      "flowchart LR"
      "classDef svc fill:#313244,stroke:#89b4fa,color:#cdd6f4"
      "classDef seg fill:#45475a,stroke:#f9e2af,color:#f9e2af"
    ]
    ++ networkBlock
    ++ lib.concatLists (lib.mapAttrsToList machineBlock machines)
    ++ map (e: e.line) edges
    ++ linkStyles
  );
in
{
  _module.args.fleetGraph = { inherit mermaid; };
}
