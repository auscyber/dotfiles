# ── THE CAPABILITY LAYER, DRAWN INTO THE HOST'S OWN ASPECT GRAPH ──
#
# ../../lib/host-aspects.nix answers "which aspects does this host use" and
# ./default.nix renders that. Neither says anything about RESOLUTION: an aspect
# declaring `takes.udp = { name = "music-assistant"; mtu = m: m >= 1500; }`
# appears as a box with no outgoing edge, and the thing a reader actually wants
# -- which giver answered it, over which carrier, and what else was in reach and
# lost -- is nowhere in the picture.
#
# ── WHY THE LOSERS ARE THE POINT ──
#
# A graph of `aspect A gives tcp` is a picture of the declarations, which are
# already readable in the file. The question that costs time is why a take
# resolved the way it did, and ../base/capability.nix already computes the
# answer: `judge` returns one verdict per candidate, `{ cand, why, ok }`, and
# `explainTake` folds that into the string a throw prints. This file renders the
# SAME verdict list, so a losing candidate is a node whose label is the clause
# it failed on.
#
# Nothing here re-decides anything. Every verdict comes from `judge`; the two
# stages below are `dial`'s own split (`whoTake` / `howTake`), kept because
# judging a whole take at either stage rejects everything -- which is the
# failure `dial`'s header describes.
#
# ── WHY IT IS AN OVERLAY AND NOT A SECOND GRAPH ──
#
# A taker and its giver are ALREADY nodes in the host graph. Drawing the
# resolution beside it would duplicate both and relate neither, so this adds
# nodes and edges to the graph that is already there, keyed onto the existing
# aspect node ids. One graph, with the capability layer in it.
#
# ── AND WHY IT IS NOT FOLDED INTO `forHost` ──
#
# ../../lib/host-aspects.nix returns `.names` as well as `.graph`, and `.names`
# is the "which aspects does this host really use" answer. Capability nodes are
# not aspects, so adding them inside `forHost` would put `udp -> tailscale ·40`
# in that list. The overlay is therefore applied by the renderer
# (./default.nix), on the way to `toMermaid`, and `forHost` is untouched.
#
# ── IR VOCABULARY, AND THE ONE THING IT CANNOT DO ──
#
# den-diagram's mermaid renderer labels an edge only for `style = "provide"`
# (`-.->|label|`); a `normal` edge renders bare and an `excluded` edge renders
# `-.-x` with no label. Its DOT renderer drops every edge label but "replaced".
# So the REASON a candidate lost cannot live on the edge that represents it. It
# lives in the candidate's own NODE label instead, which every renderer prints
# verbatim -- and the losing edge keeps `style = "excluded"` so it is dashed and
# crossed in both. Mermaid is consequently the better output here and
# ./default.nix writes that; see docs/capability-graphs/README.md.
{
  lib,
  den,
  capability,
  ...
}:
let
  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;

  # A FAILED RESOLUTION IS NOT A FAILED DIAGRAM. `judge` throws on a take whose
  # clause names something the capability neither furnishes nor accepts, which
  # is correct where it is read and wrong here: this walks every take on the
  # host, so one bad clause would take the whole picture down rather than
  # showing itself as one unresolved node. Same reasoning, same shape, as
  # ../base/service-reach.nix's `try`.
  tryOr =
    d: v:
    let
      r = builtins.tryEval v;
    in
    if r.success then r.value else d;

  threw =
    v:
    let
      r = builtins.tryEval v;
    in
    !r.success;

  # ── RENDERER-SAFE TEXT ──
  # A mermaid node label sits inside `["..."]` and a provide-edge label inside
  # `-.->|...|`, so a quote or a pipe in a clause value ends the construct. DOT
  # has the same problem with the quote.
  safe =
    s:
    lib.replaceStrings
      [
        "\""
        "|"
        "\n"
        "["
        "]"
        "{"
        "}"
      ]
      [
        "'"
        "/"
        " "
        "("
        ")"
        "("
        ")"
      ]
      (toString s);

  sanitizeId =
    s:
    let
      ok = lib.stringToCharacters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
    in
    lib.concatMapStrings (c: if lib.elem c ok then c else "_") (lib.stringToCharacters s);

  showVal =
    v:
    if lib.isFunction v then
      "<fn>"
    else if v == null then
      "null"
    else if lib.isBool v then
      (if v then "true" else "false")
    else if lib.isList v then
      "(${lib.concatMapStringsSep "," showVal v})"
    else if lib.isAttrs v then
      "(...)"
    else
      toString v;

  # The clauses worth printing beside the capability name. `gives` is structure
  # (a nested sub-request, walked by `resolveChain`) rather than a filter, and a
  # `where` predicate has no printable form.
  clauseSummary =
    take:
    let
      shown = removeAttrs take [
        "gives"
        "where"
      ];
      nonNull = lib.filterAttrs (_: v: v != null) shown;
    in
    lib.concatStringsSep ", " (lib.mapAttrsToList (k: v: "${k}=${showVal v}") nonNull);

  # ── THE SCOPE PREDICATE, WRITTEN ONCE ──
  # `visibleFrom` applies this against each giver's own `reach` and is the real
  # answer. It is restated here for two things `visibleFrom` cannot express: the
  # set of givers PLACED somewhere but out of reach (so an unresolvable take
  # says which machines could have answered it), and the `reachOverride`
  # what-if below.
  sameHousehold =
    a: b:
    (a.household.name or null) != null && (a.household.name or null) == (b.household.name or null);

  inReach =
    reach: from: on:
    if reach == "fleet" then
      true
    else if reach == "household" then
      sameHousehold from on
    else
      on.name == from.name;

  # Every giver of `cap` that sits on some host in the fleet, whether or not it
  # is in reach of the asker.
  placedGivers =
    cap:
    lib.concatLists (
      lib.mapAttrsToList (
        _: h:
        lib.mapAttrsToList (n: a: {
          giver = n;
          aspect = a;
          on = h;
        }) (tryOr { } (capability.providersFor h cap))
      ) hostsOf
    );

  # Every take an aspect on this host declares.
  takeSites =
    host:
    lib.concatLists (
      lib.mapAttrsToList (
        taker: aspect:
        lib.mapAttrsToList (cap: take: {
          inherit
            taker
            aspect
            cap
            take
            ;
        }) (aspect.takes or { })
      ) (lib.filterAttrs (_: a: (a.takes or { }) != { } && host.hasAspect a) den.aspects)
    );

  # ── ONE TAKE, RESOLVED IN THE TWO STAGES `dial` RESOLVES IT IN ──
  #
  # WHO: which giver, on which host -- `reach`, the identity clauses and the
  # giver's preference order. HOW: which carrier between the two boxes, with the
  # flow clauses (`mtu`, `maxBodyBytes`) applied to the COMPOSED link, because
  # they do not exist until the endpoint half and the carrier half are put
  # together.
  #
  # Both stages keep the whole verdict list, not just the winner.
  resolutionOf =
    {
      host,
      reachOverride ? { },
    }:
    site:
    let
      inherit (site) cap;
      t = capability.emptyTake // site.take;

      # WHICH INSTANCE, and nothing about the wire -- for a routed capability,
      # whose flow clauses wait for the carrier. Anything else (a `route`) is
      # judged whole on the giver's own furnish, as `resolveFrom` judges it.
      routed = tryOr false (capability.isRouted cap);
      whoTake =
        {
          true = capability.emptyTake // {
            inherit (t)
              name
              within
              sameHost
              where
              ;
          };
          false = t;
        }
        .${lib.boolToString routed};
      # HOW IT IS CARRIED, and nothing about which one it is.
      howTake = t // {
        name = null;
      };

      vis = tryOr { } (capability.visibleFrom host cap);

      # ── THE WHAT-IF ──
      # A giver named in `reachOverride` is treated as though its `reach` were
      # the stated value, which is how the graph shows a route that used to
      # fail: `{ mosquitto = "host"; }` reproduces the picture from before
      # ../services/homeassistant/mosquitto.nix widened it to `household`.
      # Display only -- it moves a giver from the candidate set into the
      # out-of-reach set and changes nothing else.
      pretendOut = lib.filterAttrs (
        n: a:
        reachOverride ? ${n}
        && !(lib.any (h: h.hasAspect a && inReach reachOverride.${n} host h) (lib.attrValues hostsOf))
      ) vis;
      visible = removeAttrs vis (lib.attrNames pretendOut);

      # `resolveFrom`'s candidate list, unfiltered: one entry per (giver, host
      # it is on), in the givers' own preference order. The link is furnished
      # FOR THE ASKER, which is why `peer = host` and not the provider's host.
      whoCands = lib.sort (x: y: x.via.preference < y.via.preference) (
        lib.concatLists (
          lib.mapAttrsToList (
            gName: giver:
            map
              (ph: {
                carrier = gName;
                via = giver;
                host = ph;
                sameHost = ph.name == host.name;
                link = capability.linkFor giver cap {
                  inherit host;
                  peer = host;
                };
              })
              (lib.filter (h: capability.has h giver && capability.reaches host giver h) (lib.attrValues hostsOf))
          ) visible
        )
      );

      whoV = tryOr [ ] (capability.judge cap whoTake host whoCands);
      whoWon = lib.filter (v: v.ok) whoV;
      winner = if whoWon == [ ] then null else (lib.head whoWon).cand;

      # Out of reach: placed somewhere, not visible from here. Only computed
      # when nothing answered, which is both the only time it is interesting
      # and the only time its fleet-wide walk is worth paying for.
      unreachable =
        if winner != null || whoCands != [ ] then
          [ ]
        else
          # `pretendOut` is reported on its own (`pretended` below) and would
          # otherwise appear twice -- once as the what-if and once here with
          # its REAL reach, which reads as a contradiction.
          lib.filter (p: !(visible ? ${p.giver}) && !(pretendOut ? ${p.giver})) (placedGivers cap);

      pretended = lib.mapAttrsToList (n: _: {
        giver = n;
        reach = reachOverride.${n};
      }) pretendOut;

      # ── STAGE TWO, FOR THE GIVER THAT WON ──
      giverHost = if winner == null then null else winner.host;

      endpoint =
        if winner == null then
          null
        else
          tryOr null (
            capability.endpointHalf winner.via cap {
              inherit host;
              peer = host;
            }
          );

      # A zone is a carrier attached on the provider side only, so "which zone
      # does the serving box publish under" is a question about that host.
      zones =
        if giverHost == null then
          { }
        else
          tryOr { } (
            lib.filterAttrs (
              _: a: a.domain != null && a.attachment == "provider" && giverHost.hasAspect a
            ) den.aspects
          );

      published =
        if endpoint == null || (endpoint.subdomain or null) == null || zones == { } then
          null
        else
          "${endpoint.subdomain}.${(lib.head (lib.attrValues zones)).domain}";

      carriers =
        if giverHost == null || !routed then
          [ ]
        else
          tryOr [ ] (capability.linksBetween (capability.carrierCapFor cap) host giverHost);

      composed = map (
        c:
        c
        // {
          link = capability.composeLink cap {
            inherit endpoint published;
            carrier = c.link;
          };
        }
      ) carriers;

      howV = if composed == [ ] then [ ] else tryOr [ ] (capability.judge cap howTake host composed);
      howWon = lib.filter (v: v.ok) howV;
      carrierWon = if howWon == [ ] then null else (lib.head howWon).cand;

    in
    # ── PLAIN DATA, ALL THE WAY DOWN ──
    # Deliberately carries no aspect and no host entity: a record holding one
    # is unprintable (a `mtu` clause is a lambda) and, worse, serialising it
    # walks the whole entity -- `--json` on a record carrying `winner.host`
    # does not come back. Names and strings only, which is also everything
    # the overlay below needs.
    {
      inherit (site) taker cap;
      clauses = clauseSummary site.take;
      # The instance the take asked for, which is what every `is named ...`
      # rejection is a miss against.
      wanted = t.name;
      inherit routed published;
      # `judge` threw on this take, which means a clause names something the
      # capability neither furnishes nor accepts. The aspect's own content
      # still raises it where it reads.
      broken = threw (capability.judge cap whoTake host whoCands);

      who = {
        winner =
          if winner == null then
            null
          else
            {
              giver = winner.carrier;
              on = winner.host.name or "?";
              inherit (winner) sameHost;
              reach = winner.via.reach or "host";
              preference = winner.via.preference or 0;
            };
        rejected = map (v: {
          giver = v.cand.carrier;
          on = v.cand.host.name or "?";
          preference = v.cand.via.preference or 0;
          inherit (v) why;
        }) (lib.filter (v: !v.ok) whoV);
        unreachable = map (p: {
          inherit (p) giver;
          on = p.on.name or "?";
          reach = p.aspect.reach or "host";
        }) unreachable;
        inherit pretended;
      };

      how = {
        winner =
          if carrierWon == null then
            null
          else
            {
              carrier = carrierWon.carrier;
              preference = carrierWon.via.preference or 0;
              inherit (carrierWon) sameHost;
              address = carrierWon.link.address or null;
            };
        rejected = map (v: {
          carrier = v.cand.carrier;
          preference = v.cand.via.preference or 0;
          inherit (v) why;
        }) (lib.filter (v: !v.ok) howV);
      };

      # The same table `explainTake` prints into a throw, for the stage that
      # actually decided. Kept so a textual report and the graph cannot
      # disagree about why something lost.
      explain = {
        who = tryOr "<threw>" (capability.explainTake cap whoTake host whoCands);
        how =
          if composed == [ ] then "" else tryOr "<threw>" (capability.explainTake cap howTake host composed);
      };
    };

  resolutionsFor = args@{ host, ... }: map (resolutionOf args) (takeSites host);

  # ── IR NODE DEFAULTS ──
  # Mirrors den-diagram's own `emptyNode`: a renderer reads `hasClass`,
  # `isExcluded`, `entityInstance` and friends off every node, and one missing
  # field is a `attribute ... missing` from inside the renderer rather than
  # anything that points here.
  emptyNode = {
    id = "";
    label = "";
    fullLabel = "";
    pathKey = "";
    shape = "rect";
    style = "default";
    entityKind = null;
    entityInstance = null;
    classes = [ ];
    class = "";
    perClass = { };
    fnArgNames = [ ];
    isParametric = false;
    isProvider = false;
    providerPath = [ ];
    hasClass = true;
    isExcluded = false;
    isReplaced = false;
    isPolicyDispatch = false;
    policyName = null;
    from = null;
    to = null;
    # NOT part of den-diagram's schema: the marker `capabilityNodes` below
    # filters on, so a caller can slice the enriched graph down to the
    # resolution layer with `diagram.graph.neighborhoodOf`.
    isCapability = true;
  };

  mkNode =
    n:
    emptyNode
    // n
    // {
      fullLabel = n.fullLabel or n.label or "";
      pathKey = n.pathKey or n.fullLabel or n.label or "";
      isExcluded = (n.style or "default") == "excluded";
    };

  mkEdge =
    e:
    {
      style = "normal";
      label = null;
    }
    // e;

  instKeyOf = h: "host:${h}";

  # ── THE OVERLAY ──
  #
  # Keyed onto the nodes that are already there: a taker and a same-host giver
  # are aspects the host graph has drawn, so the resolution edges attach to
  # THOSE ids rather than to copies. A giver on another machine gets a node of
  # its own, in a subgraph for its host -- which is what makes a cross-host take
  # read as crossing a boundary.
  overlay =
    {
      host,
      graph,
      reachOverride ? { },
      resolutions ? resolutionsFor { inherit host reachOverride; },
    }:
    let
      hostName = host.name or graph.rootName or "?";
      selfInst = instKeyOf hostName;

      # fullLabel -> id, preferring a node in this host's own scope: an aspect
      # delivered at both host and user level has two nodes and the host one is
      # the one a capability edge means.
      idOf =
        let
          byLabel = lib.foldl' (
            acc: n:
            let
              k = n.fullLabel or n.label;
              better = (n.entityInstance or null) == selfInst || !(acc ? ${k});
            in
            if better then acc // { ${k} = n.id; } else acc
          ) { } graph.nodes;
        in
        name: byLabel.${name} or null;

      remoteHosts = lib.unique (
        lib.filter (h: h != null && h != hostName) (
          lib.concatMap (
            r:
            (if r.who.winner == null then [ ] else [ r.who.winner.on ])
            ++ map (x: x.on) r.who.rejected
            ++ map (x: x.on) r.who.unreachable
          ) resolutions
        )
      );

      # One subgraph per remote host the resolution reaches into. den-diagram's
      # mermaid renderer groups by `entityInstance` and renders cross-instance
      # edges outside every subgraph, so this is the vocabulary's own way of
      # saying "that giver is on another box".
      extraInstances =
        map
          (h: {
            id = "ctx_host_${sanitizeId h}";
            kind = "host";
            name = h;
            label = "host: ${h}";
          })
          (
            lib.filter (
              h: !(lib.any (i: i.name == h && i.kind == "host") graph.entityInstances or [ ])
            ) remoteHosts
          );

      instFor = h: if h == null || h == hostName then selfInst else instKeyOf h;

      perTake =
        r:
        let
          base = "cap_${sanitizeId r.taker}_${sanitizeId r.cap}";

          # ── A TAKER THAT IS NOT IN THE PICTURE GETS DRAWN ──
          #
          # MEASURED on auspc: `celler-cache-pull` and `samba-user` declare
          # takes and have no node in the filtered graph -- ../../lib/host-
          # aspects.nix keeps a node only if it or something under it
          # contributes to a traced class, and an aspect whose whole
          # contribution is a take contributes to none. Keying the `takes` edge
          # onto a null id silently dropped the edge (the overlay filters edges
          # whose endpoints do not exist), which left the request node floating
          # with nothing explaining who asked.
          existingTakerId = idOf r.taker;
          takerId = if existingTakerId != null then existingTakerId else "${base}_t";
          takerNodes = lib.optional (existingTakerId == null) (mkNode {
            id = takerId;
            label = r.taker;
            entityKind = "host";
            entityInstance = selfInst;
          });

          resolved = r.who.winner != null;

          # The REQUEST. Hexagon because it is the parametric thing in this
          # picture -- the take's clauses are its arguments.
          reqNode = mkNode {
            id = base;
            label = "${r.cap}${lib.optionalString (r.clauses != "") " ${safe r.clauses}"}";
            shape = "hexagon";
            style = if resolved && !r.broken then "default" else "excluded";
            entityKind = "host";
            entityInstance = selfInst;
          };

          # ── THE GIVER THAT WON ──
          # A same-host giver is already a node; a remote one is synthesized
          # into its own host's subgraph.
          w = r.who.winner;
          localGiverId = if w == null then null else idOf w.giver;
          remoteGiverId = if w == null then null else "${base}_g_${sanitizeId w.giver}";
          useLocal = w != null && w.sameHost && localGiverId != null;
          giverId = if w == null then null else (if useLocal then localGiverId else remoteGiverId);

          giverNodes = lib.optional (w != null && !useLocal) (mkNode {
            id = remoteGiverId;
            label = "${w.giver} on ${w.on}";
            shape = "trapezoid";
            entityKind = "host";
            entityInstance = instFor w.on;
          });

          # ── HOW IT IS CARRIED ──
          # `sameHost` is a different KIND of answer from a wire and is labelled
          # as one: a loopback is not a carrier anybody chose, it is the absence
          # of a crossing.
          h = r.how.winner;
          carrierLabel =
            if h != null then
              "${h.carrier} ${toString h.preference}${
                if h.sameHost then " (same host)" else " (${hostName} to ${w.on})"
              }"
            else if !r.routed then
              "not routed"
            else if w != null && w.sameHost then
              "same host"
            else
              "resolved";

          winEdges = lib.optionals (w != null) (
            [
              (mkEdge {
                from = takerId;
                to = base;
                style = "provide";
                label = "takes";
              })
            ]
            ++ [
              (mkEdge {
                from = base;
                to = giverId;
                style = "provide";
                label = safe carrierLabel;
              })
            ]
          );

          # ── THE LOSERS, WITH THE CLAUSE THEY LOST ON ──
          # One node per rejected candidate, labelled with `judge`'s own `why`.
          # The edge is `excluded` -- dashed and crossed in mermaid, dashed in
          # DOT -- and carries no label, because neither renderer prints one on
          # that style. The reason is in the node.
          lost =
            prefix: kind: items:
            lib.imap0 (i: x: {
              node = mkNode {
                id = "${base}_${prefix}${toString i}";
                label = safe "${x.name} ${x.detail}";
                style = "excluded";
                entityKind = "host";
                entityInstance = instFor (x.inst or null);
              };
              edge = mkEdge {
                from = base;
                to = "${base}_${prefix}${toString i}";
                style = "excluded";
              };
              inherit kind;
            }) items;

          # ── THE BULK IS FOLDED, THE DECISIONS ARE NOT ──
          #
          # MEASURED on ../services/vban.nix's take: the WHO stage rejects 19
          # candidates and 19 of them say `is named <host>, want
          # music-assistant`. That is the resolver picking an instance out of
          # every loopback in the fleet -- a search, not a decision -- and
          # drawing nineteen boxes for it buries the one rejection that is a
          # decision (`tailscale: mtu is 1280`) under a wall of identity
          # misses.
          #
          # So a candidate rejected ONLY on the instance name is counted, and
          # everything else -- `reach`, `within`, `carrier`, `sameHost`, a
          # `where` predicate, a clause, a capability rule -- gets its own
          # node with its own reason. The full list is still in the record for
          # a caller that wants it.
          identityOnly = x: x.why != [ ] && lib.all (w: lib.hasPrefix "is named " w) x.why;
          whoDecided = lib.filter (x: !(identityOnly x)) r.who.rejected;
          whoByName = lib.filter identityOnly r.who.rejected;

          whoLost =
            lost "wx" "giver" (
              map (x: {
                name = "${x.giver} on ${x.on}";
                detail = lib.concatStringsSep "; " x.why;
                inst = x.on;
              }) whoDecided
            )
            ++ lost "wn" "identity" (
              lib.optional (whoByName != [ ]) {
                name = "${toString (lib.length whoByName)} other instances";
                detail = "wrong instance: want ${if r.wanted == null then "?" else r.wanted}";
                inst = hostName;
              }
            );

          howLost = lost "hx" "carrier" (
            map (x: {
              name = "${x.carrier} ${toString x.preference}";
              detail = lib.concatStringsSep "; " x.why;
              inst = hostName;
            }) r.how.rejected
          );

          outOfReach = lost "ox" "reach" (
            map (x: {
              name = "${x.giver} on ${x.on}";
              detail = "out of reach: reach = ${x.reach}";
              inst = x.on;
            }) r.who.unreachable
            ++ map (x: {
              inherit (x) giver;
              name = x.giver;
              detail = "out of reach (what-if): reach = ${x.reach}";
              inst = null;
            }) r.who.pretended
          );

          losers = whoLost ++ howLost ++ outOfReach;

          # A take nothing answered still gets its request node, so an
          # unresolved route is a visible dead end rather than a missing box.
          deadEnd = lib.optionals (w == null) [
            (mkEdge {
              from = takerId;
              to = base;
              style = "excluded";
            })
          ];
        in
        {
          nodes = takerNodes ++ [ reqNode ] ++ giverNodes ++ map (x: x.node) losers;
          edges = winEdges ++ deadEnd ++ map (x: x.edge) losers;
        };

      parts = map perTake resolutions;

      newNodes = lib.concatMap (p: p.nodes) parts;
      byId = lib.listToAttrs (
        map (n: {
          name = n.id;
          value = n;
        }) (graph.nodes ++ newNodes)
      );

      # Dedup by id -- two takes of the same capability from one host would
      # otherwise declare the same remote giver twice.
      dedupedNew = lib.attrValues (
        lib.listToAttrs (
          map (n: {
            name = n.id;
            value = n;
          }) newNodes
        )
      );

      newEdges = lib.filter (e: byId ? ${e.from} && byId ? ${e.to}) (lib.concatMap (p: p.edges) parts);
    in
    graph
    // {
      nodes = graph.nodes ++ dedupedNew;
      edges = graph.edges ++ newEdges;
      entityInstances = (graph.entityInstances or [ ]) ++ extraInstances;
    };

  isCapabilityNode = n: n.isCapability or false;

  # ── THE READABLE VIEW OF THE SAME GRAPH ──
  #
  # secondpc's aspect graph is 135 nodes, and the resolution layer added to it
  # is a few dozen more. Both renderings are the same IR; this one keeps only
  # the capability nodes and the aspects directly attached to them, which is
  # what a reader debugging ONE take wants to open.
  #
  # NOT `diagram.graph.neighborhoodOf`: that filter runs `filterUserAspects`
  # first (folding wrapper nodes and rewiring their edges) and then clears
  # `entityInstances`, so it would both reshape the synthesized nodes and throw
  # away the per-host subgraphs that make a cross-host take legible. Plain
  # filtering here, no den-diagram dependency, same IR out.
  sliceOf =
    {
      graph,
      extra ? (_: false),
    }:
    let
      seed = lib.filter (n: isCapabilityNode n || extra n) graph.nodes;
      seedIds = lib.listToAttrs (map (n: lib.nameValuePair n.id true) seed);
      touched = lib.foldl' (
        acc: e:
        if seedIds ? ${e.from} || seedIds ? ${e.to} then
          acc
          // {
            ${e.from} = true;
            ${e.to} = true;
          }
        else
          acc
      ) seedIds graph.edges;
      kept = lib.filter (n: touched ? ${n.id}) graph.nodes;
      keptIds = lib.listToAttrs (map (n: lib.nameValuePair n.id true) kept);
    in
    graph
    // {
      nodes = kept;
      edges = lib.filter (e: keptIds ? ${e.from} && keptIds ? ${e.to}) graph.edges;
      # Only the instances something kept actually lives in, or mermaid emits
      # an empty subgraph for every other host in the fleet.
      entityInstances = lib.filter (
        i:
        lib.any (
          n: (n.entityInstance or null) == (if i.kind == i.name then i.name else "${i.kind}:${i.name}")
        ) kept
      ) (graph.entityInstances or [ ]);
    };
in
{
  _module.args.capabilityGraph = {
    inherit
      resolutionsFor
      overlay
      sliceOf
      isCapabilityNode
      takeSites
      mkNode
      mkEdge
      ;
  };
}
