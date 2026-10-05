{ lib }:
# A LINK IS THE INTERSECTION OF TWO ENDPOINTS' CANDIDATE SETS.
#
# `./service-scope.nix` answers "what can this service reach that provides
# `tcp`" with a ranked LIST (`tcp.lan.lan-home`, `tcp.tailscale.ts`, ...). A link
# is the next question, and it is a question about two services at once: A and B
# can talk over `tcp` iff some ONE provider of `tcp` is reachable from BOTH.
# Neither side can answer it alone, which is why this is not a field on the
# service type -- a want is unary, a link is binary.
#
# The link itself is AGNOSTIC about the transport. What it guarantees is that
# the chosen provider exists and is reachable from both ends; what the provider
# guarantees (an IP address, a socket path) is its own `params`, and the port is
# the link's, contributed by whoever creates it. That split is the user's:
# "the tcp link itself is agnostic, but a tcp link does have a guarantee of an
# ip address, and the tcp link itself that we create has a port".
#
# WHY THE INTERSECTION IS THE WHOLE POINT. postgres does not want `tcp`; it
# wants "a connection of some sort" -- unix, tcp or udp, over loopback, lan or
# tailscale. Asking per-capability and intersecting is how that is expressed
# without a central table of which transport reaches which host: the reachable
# set already encodes it, because `reach` is PLACEMENT and a provider is visible
# exactly from the scope it sits in and below.
#
# LOOPBACK WINS A SAME-HOST PAIR, AND IT FALLS OUT OF PLACEMENT RATHER THAN OUT
# OF A SPECIAL CASE:
#
#   * a host's loopback is declared with `reach = "host"`, so it is planted in
#     `host:<h>` and is on the parent chain of everything planted there -- at
#     one hop, which is nearer than anything a household or the fleet declares;
#   * and it is planted in exactly ONE host's scope, so it is not reachable
#     from any other host at all. A cross-host pair cannot accidentally choose
#     a loopback, because the intersection is empty for it by construction.
#
# The `preferLocal` step below restates that as a rule anyway, because the
# arithmetic reading of it ("fewest hops wins") is an accident of the current
# scope chain and the guarantee is not: ../aspects/services/nextcloud.nix:327
# and ../aspects/hosts/secondpc/sso.nix:385 both record what a same-host call
# cost when it went out and came back -- metrics collection that depended on
# DNS, TLS and the cloudflare tunnel being up, and an oauth2-proxy whose startup
# fetch could only succeed if nginx was already serving. Neither is a
# performance note; both are availability bugs.
#
# THE ONE CASE PLACEMENT CANNOT ANSWER, recorded rather than hidden: `reach` is
# where a service's DECLARATION is planted and therefore also where its own
# lookups START, because they are one node in this graph. So two services that
# are both ON host h but both declared `reach = "fleet"` look up from `root` and
# cannot see h's loopback -- it is not on their chain. The honest alternatives
# were (a) split the two meanings, resolving a want from `host:<h>` while
# planting the declaration at `reach`, and (b) say so loudly. (a) is probably
# right and is a change to every want's resolution origin, so it is not made
# here; (b) is `hazard` below.
{
  services,
  candidatesFor,
  scopeOf,
  describeKey,
}:
let
  # PLACEMENT, NOT `host`. `host` is where a service DECLARES it runs, and only
  # a service placed directly on a machine declares one -- a cache on a celler
  # and a push on a cache both leave it null, because what machine they are on
  # is a consequence of what they are of. `placement` walks the identifying
  # chain to the nearest declared host, so `push/main/celler@secondpc` answers
  # `secondpc` while its own `host` is null.
  #
  # Reading `host` here meant every link involving an implied instance had a
  # null end and threw. It could not be fixed by stamping `host` down the
  # chain: the scope graph is built FROM `host`, so a `host` default reading
  # `resolved` closes graph -> host -> resolved -> graph, which is the deadlock
  # recorded twice in ./service-dep-type.nix's header. `placement` is safe for
  # the mirror reason -- nothing the graph reads ever reads it.
  hostOf = key: services.${key}.placement or services.${key}.host or null;

  describe = c: "${c.path} [${c.scope}, ${toString c.hops} hop${if c.hops == 1 then "" else "s"}]";

  # An endpoint in a diagnostic. `describeKey` carries the provenance stamp, so
  # a message about an IMPLIED instance names what implied it rather than only
  # the generated key -- see ./service-scope.nix's `givenByOf`.
  where = key: "${describeKey key}, looks up from ${scopeOf services.${key}}";

  linkBetween =
    capability: keyA: keyB:
    let
      ca = candidatesFor keyA capability;
      cb = candidatesFor keyB capability;

      # ── THE INTERSECTION IS OVER NETWORKS, AND THE ADDRESS IS AN ATTACHMENT.
      #
      # ../aspects/services/connection.nix settled the shape: a concrete
      # connection is ONE INSTANCE PER HOST, named after the network
      # (`ivymectin@secondpc`, `loopback@auspc`), carrying that host's own
      # `address`. So "A and B can talk over tcp" is not "A and B reach one
      # instance" -- they each reach their OWN end of the network -- and it is
      # not "A and B reach instances of one network" either, because every
      # host's loopback is an instance of the network called `loopback` and no
      # two of them are connected.
      #
      # The condition that is right, and that both wrong readings fail:
      #
      #   A is ON network N          -- A reaches some instance of N, and
      #   A can SEE B's end of N     -- A reaches the instance of N whose
      #                                 `host` is B's host.
      #
      # The second clause is what A actually needs, because what A needs is B's
      # ADDRESS. It excludes a cross-host loopback for free: A on auspc reaches
      # `loopback-auspc` and never `loopback-secondpc`, which is planted in
      # `host:secondpc` -- so N = loopback has no visible end on B's host and
      # drops out. It also excludes a LAN that B is not cabled to: B simply has
      # no instance of it for A to see.
      # A's OWN end of N, and B's end of N AS A SEES IT. Both must exist.
      #
      # "A is on N" cannot be "A reaches some instance of N": the household
      # declares every host's lan attachment, so auspc -- which is not cabled
      # to the house LAN at all -- reaches `lan-home@secondpc` and
      # `lan-home@laptop` and would read as being on a network it has no
      # attachment to. The acceptance test caught exactly that: the cross-host
      # link chose the LAN and handed out `192.168.1.10`, an address auspc
      # cannot route to. Being ON a network means having an END of it.
      endOn =
        cs: n: h:
        lib.findFirst (c: c.network == n && c.host != null && c.host == h) null cs;

      hostA = hostOf keyA;
      hostB = hostOf keyB;
      sameHost = if hostA != null && hostA == hostB then hostA else null;
      localScope = if sameHost == null then null else "host:${sameHost}";

      bByKey = lib.listToAttrs (map (c: lib.nameValuePair c.key c) cb);

      # One entry per shared NETWORK, carrying both ends. `far` is the instance
      # whose address A will use; `near` is A's own attachment, which is what
      # proves A is on the network. `far` must also be among B's candidates, so
      # the visibility is mutual and not just A looking at B.
      # IS THIS NETWORK A SEGMENT -- something several hosts attach to and are
      # thereby connected through? A loopback is not. Every host has an
      # instance of the network called `loopback` and no two of them are
      # connected, so matching on the network NAME alone would make any two
      # hosts "reachable over loopback".
      #
      # The discriminator is already structural and needs no new field: a
      # segment narrows `den.services.network`, which obliges a `prefix` -- "a
      # /16 is a fact about the wire, identical for everything on it". A
      # loopback deliberately does NOT include `network`, because it is not a
      # segment. So inheriting the prefix obligation IS being a segment.
      # Read off the ATTACHMENT, not off the network's definition. It used to be
      # `services.${n}.allObliges`, which looked the network up in the instance
      # registry -- and once definitions moved to `den.serviceTypes` that lookup
      # silently returned `{ }`, so NOTHING was a segment and every cross-host
      # link was refused with a message about missing ends. A lookup that cannot
      # fail loudly is the wrong lookup.
      #
      # An attachment inherits its segment's `prefix` through `includes`, so
      # having one IS being on a segment -- no second table, and the fact sits
      # on the thing the link already holds.
      isSegment = c: ((services.${c.key} or { }).prefix or null) != null;

      # One entry per shared NETWORK, carrying both ends. `far` is the instance
      # whose address A will use; `near` is A's own attachment, which is what
      # proves A is on the network.
      #
      # `far` COMES FROM B'S OWN CANDIDATE SET, not from A's. It used to be
      # `endOn ca n hostB` -- A looking for B's attachment among the things A
      # can see -- and that cannot hold once an attachment is `reach = "host"`,
      # which is what an attachment should be: auspc's address is auspc's own,
      # not a service it offers the fleet. With both attachments correctly
      # host-scoped the intersection came back EMPTY for every real cross-host
      # pair, and the diagnostic said so in `reach`-is-placement terms, which
      # read as a modelling error rather than as the over-strict condition it
      # was. Measured, with the first concrete lan attachments to exist.
      #
      # Each side asserting its OWN end is also the honest reading of the
      # relation: "A and B are siblings on N" is two local facts, not a
      # visibility question. What the earlier `ca`-only reading was guarding
      # against -- auspc, not cabled to the house LAN, reading as being on it --
      # is still excluded, by `near`: being ON a network means having an END of
      # it, and that clause is unchanged.
      withBoth =
        if hostA == null || hostB == null then
          [ ]
        else
          lib.concatMap (
            n:
            let
              near = endOn ca n hostA;
              far = endOn cb n hostB;
            in
            if near == null || far == null || (sameHost == null && !(isSegment far)) then
              [ ]
            else
              [
                (
                  far
                  // {
                    near = near.key;
                    hops = {
                      a = near.hops;
                      b = bByKey.${far.key}.hops;
                    };
                  }
                )
              ]
          ) (lib.unique (map (c: c.network) ca));

      local = lib.filter (c: c.scope == localScope) withBoth;
      preferLocal = sameHost != null && local != [ ];
      pool = if preferLocal then local else withBoth;

      # The SAME precedence the unary query uses -- `rank` (loopback, then a
      # host preference, then lan/tailscale/wireguard/public), then distance
      # summed over both ends, then layering, then the path. See
      # ./service-scope.nix's `transportOrder`.
      cost = c: c.hops.a + c.hops.b;
      ranked = builtins.sort (
        x: y:
        if x.rank != y.rank then
          x.rank < y.rank
        else if cost x != cost y then
          cost x < cost y
        else if x.layer != y.layer then
          x.layer < y.layer
        else
          x.path < y.path
      ) pool;

      # Not a throw. A same-host pair routed over the household lan is
      # suboptimal; routed over a tunnel it is the availability bug above --
      # and this link cannot tell those two apart, because the vocabulary that
      # would (§2.7's traffic classes) does not exist yet. So the fact is
      # published for a consumer to assert on rather than guessed at here.
      hazard =
        if sameHost != null && local == [ ] && withBoth != [ ] then
          "${keyA} and ${keyB} are both on ${sameHost}, but no provider of '${capability}' is declared in host:${sameHost} that both reach, so this link leaves the box and comes back. Chosen: ${describe (lib.head ranked)}. Declare a loopback provider of '${capability}' with `reach = \"host\"` on ${sameHost}, or lower a `reach` so both ends look up from host:${sameHost} (they look up from ${scopeOf services.${keyA}} and ${scopeOf services.${keyB}})."
        else
          null;

      # One line per candidate either side reaches, saying WHICH side reaches
      # it. Labelled from both indexes: an earlier version tested only the B
      # index over a list that already included B's own candidates, so every
      # B-only candidate reported "reached by both" -- the diagnostic lying in
      # exactly the case it exists for.
      aByKey = lib.listToAttrs (map (c: lib.nameValuePair c.key c) ca);
      reasons = lib.concatMapStringsSep "\n" (
        c:
        "    ${describe c} -- reached by ${
              if (aByKey ? ${c.key}) && (bByKey ? ${c.key}) then
                "both, but ${keyA} and ${keyB} do not each have an END of ${c.network}"
              else if aByKey ? ${c.key} then
                "${keyA} only"
              else
                "${keyB} only"
            }"
      ) (ca ++ lib.filter (c: !(aByKey ? ${c.key})) cb);

      empty =
        if hostA == null || hostB == null then
          "${
            if hostA == null then keyA else keyB
          } sets no `host`, and a network is joined by a host -- an unplaced service has no end of one"
        else if ca == [ ] && cb == [ ] then
          "neither end reaches any provider of '${capability}'"
        else if ca == [ ] then
          "${keyA} reaches no provider of '${capability}' at all"
        else if cb == [ ] then
          "${keyB} reaches no provider of '${capability}' at all"
        else
          "both ends reach a provider of '${capability}', but no NETWORK carries both: for every network ${keyA} is on, ${keyA} cannot see ${keyB}'s end of it";

      chosen =
        if ranked != [ ] then
          lib.head ranked
        else
          throw ''
            den.services: no '${capability}' link between ${keyA} and ${keyB} -- ${empty}.
              ${where keyA}
              ${where keyB}
            Candidates, and why none is shared:
            ${if ca == [ ] && cb == [ ] then "    none" else reasons}
            `reach` is PLACEMENT (../lib/service-scope.nix): a provider is visible exactly from the scope it is planted in and BELOW, so a host-scoped provider is unreachable from another host and a fleet-scoped wanter cannot see a household-scoped provider. Raise the provider's `reach`, or declare one in a scope both ends look up from.'';

      # ── THE CONSUMER SURFACE. Two values, and a consumer uses ONLY these. ──
      #
      # "it being tailscale is not necessarily visible to the service it just
      # knows it has some way to connect over tcp". So `address` and `endpoint`
      # carry no transport name, no scheme and nothing to branch on: the same
      # expression yields `127.0.0.1` when the two ends share a host and the
      # tailnet address when they do not, with no change at the call site.
      #
      # This is what makes ../aspects/services/audiobookshelf.nix's hardcoded
      # `http://127.0.0.1:8095` derivable. That line is correct only because
      # Music Assistant and audiobookshelf both happen to sit on secondpc, and
      # it is silently wrong AT RUNTIME -- not at eval -- the moment either
      # moves; the same file calls it the `localUrl` escape hatch. Written
      # against a link it is `"http://${link.endpoint 8095}"` and it follows.
      #
      # `chosen` and `candidates` are the DEBUG view and the throw's evidence.
      # A consumer that reads `chosen.serviceName` to test for tailscale has
      # reintroduced exactly what this hides.
      #
      # The address is the FAR END'S OWN, read off the instance that is B's
      # attachment to the chosen network (`address` on the service, freeform
      # under `strict = false`, declared and never read from a host's evaluated
      # config -- ../aspects/services/connection.nix's recursion hazard). The
      # intersection already guarantees that instance exists and that A can see
      # it, so a null here is a provider that never said where it is.
      address =
        # `chosen` first, so an empty intersection raises the intersection's
        # diagnostic rather than a message about a missing address.
        lib.seq chosen (
          if chosen.address == null then
            throw "den.services: the '${capability}' link from ${keyA} to ${keyB} resolved to ${chosen.path} -- ${chosen.key} is ${hostB}'s end of the network '${chosen.network}' -- but neither it nor anything it `includes` declares an `address`, so there is nothing to connect to. Declare `address` on that attachment, or once on the transport it narrows and let every attachment inherit it: `den.services.loopback.address = \"127.0.0.1\";` is the whole fix for every host's loopback."
          else
            chosen.address
        );
    in
    {
      inherit
        capability
        sameHost
        preferLocal
        hazard
        ;
      from = keyA;
      to = keyB;

      # Every shared provider, best first. Empty is not an error here -- only
      # forcing `chosen` is -- so a caller may intersect speculatively.
      candidates = ranked;

      # The resolution's answer, and the throw site. DEBUG and diagnostics
      # only -- see `address`.
      inherit chosen address;

      # `host:port`, for the one thing the link itself owns: "the tcp link
      # itself that we create has a port". The port is the caller's because it
      # is a property of the link and not of the transport, which is why this
      # is a function rather than a field.
      endpoint = port: "${address}:${toString port}";

      # What each side could reach on its own, for a diagnostic the caller
      # writes itself.
      reached = {
        a = ca;
        b = cb;
      };
    };
in
{
  inherit linkBetween;
}
