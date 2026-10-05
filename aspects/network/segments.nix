{ den, carrierAttachment, ... }:
# THE NETWORK SEGMENTS, AS ASPECTS.
#
# This replaced ../services/connection.nix (deleted) -- a registry of abstract
# types plus
# eleven derived `loopback-<host>` keys, and the `runs.lan-home.address` /
# `runs.tailnet` declarations on each host. A host is on a segment because it
# INCLUDES the segment's aspect -- that is all membership is, and it cannot
# disagree with reality the way a declared flag can.
#
# ../base/capability.nix reads `gives`, `prefix` and `addressOf` off these.
#
# WHY A LOOPBACK IS NOT A SEGMENT, and why `capability.nix` tests for `prefix`
# rather than for a name: every host has a loopback and no two of them are
# connected, so matching on the capability alone would make any pair of hosts
# "reachable over loopback". A segment is a thing SEVERAL hosts attach to, and
# what says so is carrying a prefix -- a /16 is a fact about the wire, identical
# for everything on it. So `loopback` below deliberately has no `prefix`, and
# that absence is load-bearing rather than an omission.
#
# WHY `addressOf` RATHER THAN A STORED ADDRESS. An address is a property of the
# ATTACHMENT between a host and a network -- not a fact about either end -- which
# is why `lanAddress` sat awkwardly on the host record and why a stored
# `transports.<n>.address` felt wrong when it was tried. The provider computes
# it from the host instead, which is §2.7's `addressOf` recovered now that the
# resolver lives inside den.
{
  # ── THE HOUSE LAN ──
  #
  # `addressOf` reads the one transport fact nothing can derive:
  # ../hosts/fleet.nix's `lanAddress`, declared per host because an IP handed
  # out by a router is not a function of anything. A host with none returns
  # null, which is the honest answer for a machine not cabled to this segment --
  # and is why membership is inclusion AND an address, not just inclusion.
  #
  # THE PREFIX IS `/16`, AND IT IS NOW MEASURED. It was carried over from the
  # deleted ../services/connection.nix and went unchecked for a long time, with
  # every cross-host link that resolves over this segment resting on it: it
  # decides whether auspc (192.168.1.11) and secondpc (192.168.0.26) are
  # siblings at all, and at /24 they are not, so the correct answer for a link
  # between them would be NONE.
  #
  # `ip -br addr` on auspc reports `192.168.1.11/16`. The interface itself
  # carries the prefix, so the two hosts are on one link and the `/24` reading
  # that several comments in this tree hedge about is simply wrong for this
  # house.
  den.aspects.lan-home = {
    # THE HOUSE ROUTER, declared once on the segment that has it. `gatewayAddress`
    # is a reserved aspect key (../base/capability-types.nix) and ./container-bridge.nix
    # already uses it for its own bridge, so this is the existing slot rather
    # than a new one. It was written out per host in each `networking.defaultGateway`
    # and in each wifi profile -- the same string in several files, able to
    # disagree.
    gatewayAddress = "192.168.0.1";

    # The giver furnishes what a consumer receives -- see ./tailscale.nix for
    # why `gives` takes `self`. Here the address is the one transport fact
    # nothing can derive, so the link reads ../hosts/fleet.nix's declared
    # `lanAddress`; a host not cabled to this segment answers null, which is
    # honest rather than a hole.
    gives =
      {
        self,
        host ? { },
        ...
      }:
      let
        # ── BOTH HALVES OF THE ATTACHMENT, FROM THE TWO ENDS THAT HOLD THEM ──
        #
        # The ADDRESS is the peer's: it is where this host dials THAT one.
        # The INTERFACE is this host's own: it is where this host HEARS that
        # carrier, which is what ../base/firewall.nix opens a port on. Reading
        # the interface off the peer would open secondpc's ports on auspc's
        # NIC name, which is the kind of wrong that looks right in a diff.
        #
        # Both come from `attachments.lan-home` on the respective entity
        # (../hosts/fleet.nix) -- declared data, so no host has to be
        # evaluated to answer either one.
        link =
          { peer, ... }:
          {
            # ── `${self.name}`, NOT THE LITERAL `lan-home` ──
            # The aspect's own name, from the reference that already carries
            # it. Written out, this was the aspect's identity restated inside
            # itself, and the two can disagree exactly once and silently:
            # `attachments.<renamed>` is then MISSING, and the `or` beside it
            # swallows the hole and hands back a null address. A literal is
            # right for a foreign name; it is wrong for one of ours.
            address = peer.attachments.${self.name}.address or peer.lanAddress or null;
            interface = host.attachments.${self.name}.interface or null;
            # ETHERNET, so the one carrier that can take `realtime-audio`:
            # ../services/vban.nix refuses a tunnel because "a full VBAN packet
            # does not fit a 1420-byte tunnel MTU", and 1500 is what makes that a
            # comparison rather than a comment.
            #
            # READ OFF THE ASPECT (`mtu` below) rather than written here: it is
            # a fact about the wire and the same for every pair on it, so
            # ./carrier-attachment.nix's roster reads the one declaration
            # instead of restating it.
            inherit (self) mtu;
            # No proxy in the path, so nothing caps a body.
            inherit (self) maxBodyBytes;
            # No `carrier`: ../base/capability.nix derives it from the aspect
            # that furnished the link, which is this one.
          };
      in
      {
        tcp = link;
        udp = link;

        # WHAT MAKES THIS A SEGMENT SEVERAL HOSTS SHARE, which is the fact
        # ../base/capability.nix's link search tests for. `spans` is the test
        # now, declared rather than inferred from carrying a prefix.
        segment = _: {
          inherit (self) prefix spans;
        };

        # The house lan, offered to tailnet peers that are away from it.
        route = _: {
          segment = self.name;
          inherit (self) prefix;
          advertise = true;
        };

        # NO `hostname`, and the absence is a finding rather than an omission:
        # nothing serves a zone for this segment, so there is no name a host on
        # the lan can resolve for another. §2.7a's dnsmasq `svc.internal` zone
        # is what would fill it. Until then a consumer wanting a name over the
        # lan finds NO PROVIDER -- loud -- where a nullable `hostname` field on
        # the tcp link would have handed back null and been read as "no name
        # today" rather than "this carrier has no namespace".
      };

    # Preference is ASCENDING DEPTH OF LAYERING, not speed: a wire has no
    # underlay to be up, a tunnel does. See the ordering note in
    # ../base/capability.nix.
    preference = 20;

    # ONE network, and every host on this wire is a part of it.
    spans = "many-hosts";

    # ── IS THAT PEER ON THE WIRE RIGHT NOW ──
    # ICMP is the right test for a wire and the wrong test for a service --
    # measured on faggot.sh, where ping answered 2/2 while the port was
    # refused. This is the wire level, so ICMP is what it asks; the service
    # level is the capability's own probe
    # (../base/capability-types.nix's `probe`), and `probesFor` runs this one
    # first.
    probe = { link, ... }: "ping -c1 -W2 ${link.address}";

    prefix = "192.168.0.0/16";

    # ── THE CARRIER'S OWN TRANSPORT FACTS, OFF THE PER-PAIR FUNCTION ──
    # ETHERNET, so the one carrier that can take `realtime-audio`:
    # ../services/vban.nix refuses a tunnel because "a full VBAN packet does
    # not fit a 1420-byte tunnel MTU", and 1500 is what makes that a
    # comparison rather than a comment. Declared here because it is the same
    # number for every pair on this wire, which is exactly what a link's
    # per-pair function cannot say.
    mtu = 1500;
    maxBodyBytes = null;
  };

  # ── THIS SEGMENT'S HALF OF AN ATTACHMENT ──
  #
  # EMPTY ON PURPOSE. ./carrier-attachment.nix derives every field from
  # `attachesVia`, which defaults to the carrier's own name -- so the address
  # is the first `attachments.<key>.address` that is not null and the
  # interfaces are every `attachments.<key>.interface`, which is what `gives`
  # above does by hand for one key. ./house-wifi.nix adds its own key to that
  # list rather than presenting a second segment with this prefix.
  #
  # ── AND `lanAddress` IS GONE FROM THE ROSTER, MEASURED RATHER THAN ASSUMED ──
  #
  # `gives` above keeps the two-level fall-through
  # `attachments.<n>.address or lanAddress or null`, because that is what
  # resolution does today. The roster drops the second level, and it costs
  # nothing: the entity's `lanAddress` is NULL ON ALL ELEVEN HOSTS. secondpc
  # looks like a counterexample and is not -- its `lanAddress = "192.168.0.26"`
  # (../hosts/secondpc/default.nix) is a `let` binding feeding
  # `attachments.lan-home.address`, not the entity option. So the second level
  # of that chain has never fired for any host in this fleet, and ONE place an
  # address can live is the whole of it.
  den.carrierAttachments.lan-home = { };

  den.aspects.lan-home.carrier-attachment = { host, ... }: [
    (carrierAttachment.half "lan-home" host)
  ];

  # ── EVERY HOST IS CONNECTED TO ITSELF ──
  #
  # `unix` is what a loopback has and no routed segment does, which is what
  # makes a unix socket unable to leave the box. That is not a rule anything
  # enforces: a consumer declaring `uses.unix` can only ever resolve to its own
  # host's loopback, so "auspc must use its own postgres, never secondpc's" --
  # the failure `reach` was introduced to prevent -- becomes unrepresentable
  # rather than guarded.
  #
  # No `prefix`, deliberately: see the header.
  den.aspects.loopback = {
    # ── `{ peer, ... }` AND NOT `_:`, WHICH IS THE WHOLE CORRECTION HERE ──
    #
    # This giver used to discard its arguments, so it claimed `127.0.0.1` for
    # ANY peer -- which made the loopback look like a wire between auspc and
    # secondpc, and `127.0.0.1` means a different machine at each end.
    #
    # It is not a theoretical wrong answer. Deriving firewall exposure by
    # taking the lowest-preference carrier that furnishes a non-null address
    # for the peer picked THIS carrier for a REMOTE peer and opened the port on
    # `lo`: `{"ifaces":{"lo":[6742]}}`, where the answer was the LAN interface.
    # `linksBetween`'s own answer was right, because `satisfies ... "segment"`
    # removes it one line later -- so the defect was invisible everywhere
    # except in an intermediate, which is exactly where it was read.
    #
    # ── AND A NULL ADDRESS IS ALREADY THE LAYER'S WORD FOR "DOES NOT CONNECT" ──
    #
    # ./house-wifi.nix got this right from the start: `address =
    # peer.attachments.${self.name}.address or null`, so a peer that is not
    # attached gets null and the carrier self-filters. "Does this carrier
    # connect A to B" is the CARRIER's question, and a resolver predicate
    # compensating for a carrier that answers it wrong is a filter that can be
    # forgotten -- and was.
    gives =
      {
        self,
        host ? { },
        ...
      }:
      let
        # `host.name` MUST BE TESTED FOR NULL, not just compared: the resolver
        # calls a giver's outer level with no host in some paths (a key-set
        # probe, ../base/capability.nix's `givenBy` with an empty ctx), and
        # `null == null` would make every such call "self" and hand the address
        # straight back.
        isSelf = peer: (host.name or null) != null && (peer.name or null) == host.name;

        link = {
          # ── WHICH WIRE, AND THERE IS ONLY EVER ONE ──
          # Every host has exactly one loopback and it is always called `lo`,
          # which is why these are literals here and a declared attachment for
          # the lan. The CONDITION is the new part, not the values.
          address = "127.0.0.1";
          interface = "lo";
          # The loopback MTU on Linux; nothing fragments and nothing caps --
          # read off the aspect for ./carrier-attachment.nix's reason.
          inherit (self) mtu maxBodyBytes;
        };

        # NOT REACHABLE, said in the two fields that can say it. A consumer
        # reading `interface` off a link whose address is null would otherwise
        # get `lo` for a remote peer, which is the same wrong answer one field
        # along.
        nowhere = {
          address = null;
          interface = null;
          inherit (self) mtu maxBodyBytes;
        };
      in
      {
        tcp = { peer, ... }: if isSelf peer then link else nowhere;
        udp = { peer, ... }: if isSelf peer then link else nowhere;

        # The socket has no address, which is exactly why it cannot leave the
        # box -- ../base/capability-types.nix: "Cannot leave the host, because
        # it is not a narrowing of `address`". `peer` is bound all the same, so
        # that this giver states its self-only nature rather than merely
        # happening to be filtered, and `socketDir` is `types.str` with no null
        # to hand back for a foreign peer.
        unix =
          { peer, ... }:
          {
            socketDir = "/run";
          };

        # The one name that resolves on every host without anything serving it,
        # so it is the carrier's to furnish rather than a literal in each
        # consumer.
        #
        # ⚠ AND IT IS STILL WRONG FOR A FOREIGN PEER, UNREPRESENTABLY SO.
        # `capability.namesFor` walks `providersFor host "hostname"` with NO
        # segment filter, so `namesFor auspc secondpc` returns `[ "localhost",
        # "secondpc.tailef3db9.ts.net", "secondpc" ]` -- `localhost` as a name
        # for secondpc. The fix this file can make everywhere else is a null,
        # and `hostname` is `types.str` with no default: there is no value that
        # means "no name for this peer", and omitting the key makes the typed
        # half THROW on access rather than read as absent. So `peer` is bound,
        # the defect is named, and the fix belongs in
        # ../base/capability-types.nix -- `hostname` nullable, or `namesFor`
        # filtering as `linksBetween` does.
        hostname =
          { peer, ... }:
          {
            hostname = "localhost";
          };

        # ── A SEGMENT WITH ONE MEMBER, DECLARED ──
        #
        # This give used to be ABSENT, and the absence was what stopped every
        # pair of hosts being "reachable over loopback" -- a fact carried by a
        # missing attribute, which two files hedged about and three predicates
        # expressed as "not a provider and gives no segment".
        #
        # A loopback IS a network: it is simply a whole network that stops at
        # one box, where a lan or a tunnel is one network several hosts are
        # each a part of. `spans` says which, so the cross-host question is
        # asked of a declared value instead of an omission.
        #
        # NO `prefix`: there is no CIDR shared by anything, because there is
        # nothing else on this wire.
        segment = _: {
          inherit (self) spans;
        };
      };

    # Nothing beats a loopback where it is usable, because a longer route
    # acquires dependencies the call did not have.
    preference = 0;

    # A WHOLE NETWORK, ON ONE BOX. See ../base/capability-types.nix's `spans`.
    spans = "one-host";

    # The loopback MTU on Linux; nothing fragments and nothing caps. On the
    # aspect rather than inside the link for ./carrier-attachment.nix's reason:
    # it is the same number for every pair, so it is not part of the relation.
    mtu = 65536;
    maxBodyBytes = null;
  };

  # ── A LOOPBACK'S HALF, WHICH IS THE SAME ON EVERY HOST ──
  # Every host has exactly one, it is always 127.0.0.1 on `lo`, and `localhost`
  # always resolves -- so the half carries constants and the roster still gets
  # the one fact that matters: WHICH hosts have it. A loopback is deliberately
  # not a segment (see the header), so the fold's own segment clause is what
  # keeps two different hosts from being siblings over it -- exactly as
  # ../base/capability.nix's does.
  den.carrierAttachments.loopback = {
    # NO `attachesVia`: there is no `attachments.loopback` to read and there
    # should not be. Every host has exactly one loopback and nobody should have
    # to write that down, which is the same reason the aspect itself is a
    # `den.default` include.
    attachesVia = [ ];
    dialAt = _: "127.0.0.1";
    interfacesOn = _: [ "lo" ];
    hostnameAt = _: "localhost";

    # `socketDir` is the one field the generic assembler has no slot for: a
    # unix socket has no address, which is exactly why it cannot leave the box.
    extra.unix.socketDir = "/run";
  };

  den.aspects.loopback.carrier-attachment = { host, ... }: [
    (carrierAttachment.half "loopback" host)
  ];

  # Every host has a loopback and nobody should have to write that down. A
  # `den.default` include rather than a line per host, which is what the eleven
  # derived keys in the deleted ../services/connection.nix were doing the hard way.
  den.default.includes = [ den.aspects.loopback ];
}
