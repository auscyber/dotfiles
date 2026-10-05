{
  den,
  lib,
  config,
  capability,
  checkLib,
  ...
}:
# ── THE CARRIER ATTACHMENT ROSTER: A POOLED QUIRK, AS A SHADOW OF RESOLUTION ──
#
# A SHADOW. Nothing here replaces ../base/capability.nix. `linksBetween`,
# `dial`, `resolveFrom` and `reachEndpoint` are untouched and remain the only
# path anything in this tree resolves through. What this file adds is a SECOND
# way to answer the same question, plus a check that the two agree over the
# whole fleet -- which is the only thing that could ever make swapping one for
# the other safe.
#
# ── THE INSIGHT: A CARRIER IS A RELATION, AND IT DECOMPOSES ──
#
# A carrier is not a value. ../network/house-wifi.nix's giver reads
# `peer.attachments.house-wifi.address` and `host.attachments.house-wifi
# .interface`, so it furnishes a DIFFERENT link for every (host, peer) pair.
# Resolution therefore calls that function once per pair per call site, and
# there are 59 of those in this tree, 25 of them inside
# `capability.contributions`.
#
# But the relation decomposes, and every carrier in this tree decomposes the
# same way:
#
#   ADDRESS     a fact about the FAR end  -- where others dial that host
#   INTERFACE   a fact about the NEAR end -- where this host hears the carrier
#   mtu, maxBodyBytes, namespaced, prefix, preference, attachment
#               facts about the CARRIER, the same for every pair on it
#
# So each carrier can emit ONE HALF about each host that has it -- "host H is
# attached to carrier C, dialled at A, heard on I" -- which is a plain ENTITY
# fact and never config-dependent. Pool those halves fleet-wide and the
# per-pair question becomes a LOCAL FOLD over pooled data: one roster per host
# rather than a giver walk per call site.
#
# ── THE HAZARD, AND WHY IT CANNOT FIRE HERE ──
#
# ../base/capability.nix's `cross-host-emissions-are-entity-only` check exists
# because an emission that binds `config` and crosses a host boundary is
# resolved EAGERLY against the SOURCE host's instantiated configuration
# (den's `resolveEntry` in `assemble-pipes.nix`: a thunk whose arguments are
# not all in the scope context is `isConfigDependent`, and a crossing path
# hands it `config = hostConfigs.<source>`). On a fleet of eleven that turns a
# cheap gather into eleven host instantiations, several GB each (../../CLAUDE.md).
#
# The emits below take `{ host, ... }` and NOTHING ELSE. `host` is in every
# host scope's context, so den classifies them `isPipelineParametric` and
# resolves them from the entity alone -- no `config`, no `pkgs`, no fixpoint.
# That is not a convention anybody has to keep: the half is made of
# `host.attachments.<carrier>` and `host.name`, which are declared data on the
# entity (../hosts/fleet.nix), so there is nothing a `config` could add.
#
# ── WHY `collectAll` AND NOT `collect`, AND NOT `broadcast` ──
#
# `collect` is wrong, and MEASURED wrong rather than wrong in principle. The
# cross-scope guide: "by default, hosts sit under a `flake-system` scope per
# system, so a host's siblings are the hosts on the same system". This fleet
# spans FOUR systems --
#
#   aarch64-darwin   Ivys-MacBook-Pro, macmini
#   aarch64-ios      iphone
#   aarch64-linux    lora-pi
#   x86_64-linux     auspc, birdsarentreal, celler2, pentestvm, secondpc,
#                    surfacelaptop, wsl-nixos
#
# -- and the roster's whole point is that the laptop can resolve a link to
# secondpc. `collect` would hand the laptop a roster containing macmini and
# nothing else, which is not a smaller answer but a wrong one.
#
# `broadcast` would carry the right data today and is the wrong SHAPE. Two
# reasons, and the second is the one that would bite:
#
#   1. DIRECTION. A roster is a pull: every host wants every record, so the
#      predicate belongs to the CONSUMER ("give me every host's halves"), not
#      to eleven producers each naming its audience. The guide's own example
#      for exactly this -- "a record every host needs about every other host
#      (an /etc/hosts roster)" -- is `collectAll`.
#
#   2. "a broadcaster pushes its own raw emits only -- data exposed UP into it
#      (e.g. a host broadcaster's users' `pipe.expose` data) does not ride
#      along." Today every half is emitted at HOST scope, so there is nothing
#      exposed up and a broadcast would be equivalent. The moment a carrier is
#      a container's or a user's -- a per-user tailnet, a netns -- its half
#      arrives at the host by `pipe.expose` and a broadcast would silently
#      drop it while every file still looked correct. `collectAll` reads
#      `allExposed` at each source scope (`collectTagged` in
#      `assemble-pipes.nix`), so an exposed half crosses as data.
#
# `withProvenance` because WHOSE half it is must not be a field anybody can
# get wrong. The pipe already knows the source scope; `source.host.name` is
# that answer, and the `host` written into the record below is kept only so a
# consumer that has already dropped provenance can still be read. The check
# compares them.
let
  inherit (lib) mkOption types;
  inherit (den.lib.policy) pipe;

  capabilities = config.den.capabilities;

  # ── THE PROJECTIONS ../base/capability.nix DOES NOT EXPORT ──
  #
  # `capSurface`, `capClosure` and `sideOptions` are private to that file, and
  # the shadow needs all three to reproduce what a consumer actually RECEIVES
  # rather than what a giver furnished -- `carrierHalf` keeps only the keys
  # belonging to the carrier side and types those, so a zone matched at `dns`
  # hands back no `address` at all and a fold that returned one would disagree
  # with resolution over a field neither side meant.
  #
  # Re-derived from `den.capabilities`, which is the same source of truth those
  # three read; the equivalence check below is what says the re-derivation is
  # faithful. A swap would export them from that file instead of keeping this
  # copy, and this comment is the note to delete when it does.
  capClosure =
    cap:
    let
      def = capabilities.${cap} or null;
      parents = if def == null then [ ] else map (c: c.name) def.includes;
    in
    [ cap ] ++ lib.concatMap capClosure parents;

  capSurface =
    cap:
    let
      def = capabilities.${cap} or null;
      edges = if def == null then [ ] else map (c: c.name) (def.includes ++ def.takes);
    in
    lib.unique ([ cap ] ++ lib.concatMap capSurface edges);

  sideOptions =
    want: cap:
    lib.foldl' (
      acc: c:
      let
        def = capabilities.${c} or null;
      in
      if def == null || (def.side != want && def.side != "either") then acc else acc // def.options
    ) { } (lib.reverseList (capSurface cap));

  # ── WHICH CAPABILITIES A CARRIER FURNISHES ──
  #
  # The KEY SET of `gives`, and a carrier fact rather than a per-host one: the
  # outer level of every carrier's `gives` returns a fixed set of keys, and
  # only the values inside depend on the peer. Called with a neutral host for
  # that reason -- `lib.attrNames` forces no value, so nothing a host could
  # supply is read. `carrier-attachment-caps-are-host-independent` below is the
  # check that says so rather than this comment saying so.
  capsOfWith =
    host: name:
    let
      aspect = den.aspects.${name};
      args = {
        inherit host;
        self = aspect;
        resolve = _: _: null;
      };
      g = aspect.gives or null;
    in
    if g != null && den.lib.canTake.atLeast args g then lib.attrNames (g args) else [ ];

  capsOf = capsOfWith { };

  # ── THE NON-PAIR FACTS, READ OFF THE CARRIER ASPECT ──
  # Not pooled, because they are not about a host: every pair on one carrier
  # sees the same mtu and the same prefix. `preference`, `prefix`,
  # `attachment` and `domain` were already declared there; `mtu`,
  # `maxBodyBytes` and `namespaced` are lifted out of each `gives` by this
  # change so that both paths read ONE declaration and cannot drift.
  factsOf =
    name:
    let
      a = den.aspects.${name};
    in
    {
      inherit (a)
        preference
        prefix
        attachment
        domain
        mtu
        maxBodyBytes
        ;

      inherit (a) namespaced;
      inherit (a) spans;
      caps = capsOf name;
    };

  carriers = config.den.carrierAttachments;
  carrierNames = lib.attrNames carriers;

  # ── ONE HALF, ABOUT ONE HOST, ON ONE SEGMENT ──
  #
  # Three strings and two lists. Everything else about the link is either the
  # far end's half or a carrier fact.
  #
  # `interfaces` IS THE PLURAL, AND IT IS THE POINT. A host attaches to one
  # segment over as many wires as it has: auspc is on the house lan over
  # `enp6s0` and over `wlo1`, and both are the same network. `hearsOn` is the
  # first of them, which is the single value `capability.linksBetween`'s link
  # can hold -- and the gap between the two is exactly what
  # ../services/openrgb.nix's `expose = [ "lan-home" "house-wifi" ]` is working
  # around.
  halfOf = name: host: {
    carrier = name;
    host = host.name;
    dialAt = carriers.${name}.dialAt host;
    interfaces = carriers.${name}.interfacesOn host;
    hearsOn = carriers.${name}.hearsOn host;
    hostnameAt = carriers.${name}.hostnameAt host;
    aliasesAt = carriers.${name}.aliasesAt host;
  };

  # ── THE SAME POOL, BUILT AT FLAKE LEVEL ──
  #
  # The pipe delivers this into a host's class content; a check cannot afford
  # to instantiate eleven hosts to read it (../framework/checks.nix). So the
  # pool is also reconstructible from entities, and membership -- which is what
  # the pipe gives for free, since an aspect only emits where it is included --
  # is `hasAspect` here.
  #
  # The two must agree, and `.#checkFindings` cannot ask a host. The one-shot
  # comparison is `nixosConfigurations.<h>.config.system.build.carrierRoster`
  # against this; see the report.
  poolFor =
    hostEntities:
    lib.concatMap (
      h: map (n: halfOf n h) (lib.filter (n: h.hasAspect den.aspects.${n}) carrierNames)
    ) hostEntities;

  # ── HOST-LOCAL: SHARING A NAME IS NOT SHARING A WIRE ──
  #
  # `lan-home` is ONE segment that many hosts attach to; the tailnet is one
  # tailnet; the zone is one zone. For all of those, same name means same wire
  # and ../base/capability.nix's name-matching membership is exactly right.
  #
  # `loopback` and `container-bridge` are not one thing. There are N loopbacks,
  # one per host, each a genuinely different wire that merely shares a name and
  # an address -- and ../network/container-bridge.nix's header says the same of
  # itself: "`127.0.0.1` inside a container is the container". So for those two
  # alone, same name does NOT mean same wire.
  #
  # WHAT FIXES IT IS THE CARRIER, NOT A PREDICATE HERE. Each of those givers
  # now binds `peer` and furnishes a NULL ADDRESS for a peer that is not the
  # host (./segments.nix, ./container-bridge.nix), which is the signal
  # ./house-wifi.nix has always used. This predicate survives only to STATE the
  # invariant in `host-local-carriers-stay-on-one-host` below, where it can
  # fail loudly -- it is not part of the fold.
  #
  # Derived, not declared, from the fact ./segments.nix's header already calls
  # load-bearing: "a segment is a thing SEVERAL hosts attach to, and what says
  # so is carrying a prefix. So `loopback` below deliberately has no `prefix`,
  # and that absence is load-bearing rather than an omission."
  # ── AND IT IS DECLARED NOW, SO THE DOUBLE NEGATIVE IS GONE ──
  # This was "not a provider, and gives no `segment`" -- the absence the
  # comment above calls load-bearing. ../base/capability-types.nix's `spans`
  # says it instead, and a container bridge is the case that proves the old
  # test was never about the prefix: it carries one and is still one host's.
  # Mirrors ../base/capability.nix's: an entity's machine is its `parent`
  # where it has one and itself where it does not.
  # `defaultTo`, NOT `or`: `parent` is a declared option defaulting to null,
  # so it always EXISTS and `h.parent or h.name` would hand back the null.
  machineOf = h: lib.defaultTo (h.name or null) (h.parent or null);

  hostLocal =
    name:
    let
      f = factsOf name;
    in
    # `!= "many-hosts"`, NOT `== "one-host"`: a container bridge is
    # `one-machine` now (../network/container-bridge.nix) and is still not a
    # wire several machines share, so it must stay inside this test. Written as
    # the equality it replaced, this check stopped covering the bridge the
    # moment that value changed -- silently, since nothing is `one-machine`
    # cross-machine yet.
    f.attachment != "provider" && f.spans != "many-hosts";

  # Halves of one host, keyed by carrier -- the shape the fold wants.
  indexBy =
    roster: hostName:
    lib.listToAttrs (
      map (e: {
        name = e.carrier;
        value = e;
      }) (lib.filter (e: e.host == hostName) roster)
    );

  # ── THE FOLD: THE SAME QUESTION `capability.linksBetween` ANSWERS ──
  #
  # Every clause below mirrors one in that function, deliberately and in the
  # same order, because the check compares them literally:
  #
  #   satisfies      a carrier answers `cap` if some key it furnishes narrows to it
  #   shared         membership is by CARRIER NAME, except a provider needs only
  #                  the serving end (../network/cloudflare.nix's reason)
  #   usable         off-host, a non-provider must also be a SEGMENT -- which is
  #                  what stops every pair of hosts being siblings over loopback
  #   ranked         ascending preference: depth of layering, not speed
  #   link           far end's address, near end's interface, carrier's constants
  #
  # What it does NOT do is call a giver. `satisfies` is a list membership test
  # over a carrier fact, and the link is an attrset merge over two pooled
  # records. No `gives` is applied per pair, which is the whole point.
  linksFrom =
    roster: cap: hostA: hostB:
    let
      a = indexBy roster hostA.name;
      b = indexBy roster hostB.name;

      answers = e: lib.any (g: lib.elem cap (capClosure g)) (factsOf e.carrier).caps;

      bFor = lib.filterAttrs (_: answers) b;

      aFor = lib.filterAttrs (_: answers) a;

      # ── MEMBERSHIP, MIRRORING `linksBetween` CLAUSE FOR CLAUSE ──
      #
      # Name-matching, and that is CORRECT rather than merely copied. For every
      # carrier that genuinely connects two hosts there is one wire with one
      # name -- `lan-home` is one segment, the tailnet is one tailnet -- so the
      # same name at both ends IS the same wire. The two carriers where it
      # would not be, the loopback and the container bridge, now say so
      # themselves: each binds `peer` and furnishes a NULL ADDRESS when the
      # peer is not the host (./segments.nix, ./container-bridge.nix). "Does
      # this carrier connect A to B" is the carrier's question, answered in the
      # carrier, and a null address is the layer's established word for no.
      #
      # A provider needs only the serving end -- ../network/cloudflare.nix: "a
      # client reaching `cache.ivymect.in` from a cafe includes nothing and
      # needs to".
      shared = lib.filterAttrs (n: _: (factsOf n).attachment == "provider" || aFor ? ${n}) bFor;

      sameHost = hostA.name == hostB.name;

      segments = name: (factsOf name).spans == "many-hosts";

      # ── AND THE SEGMENT CLAUSE, WHICH IS NOW THE SECOND LINE AND NOT THE ONLY ONE ──
      #
      # Kept because this fold must answer exactly as `linksBetween` does, and
      # that is what `linksBetween` does. What changed is its standing: before
      # the giver fix above it was the ONLY thing keeping a loopback out of a
      # cross-host answer, which is why reading any earlier intermediate gave
      # the wrong wire. Now the data says no as well as the filter, and
      # `carriers-claim-no-address-for-an-unattached-peer` below is the check
      # that holds the carriers to it.
      usable =
        if sameHost then
          shared
        else
          # THE SAME THREE CLAUSES ../base/capability.nix's `usable` HAS, and it
          # has to be the same three: `carrier-roster-equals-resolution` holds
          # the two paths to one answer. `sameMachine` is equal names while no
          # entity declares a `parent`, so this is inert today and diverges the
          # moment it is not.
          lib.filterAttrs (
            n: _:
            (factsOf n).attachment == "provider"
            || segments n
            || ((factsOf n).spans == "one-machine" && machineOf hostA == machineOf hostB)
          ) shared;

      ranked = lib.sort (x: y: (factsOf x).preference < (factsOf y).preference) (lib.attrNames usable);

      # ── WHICH KEY THE CARRIER IS ANSWERED AT ──
      # An exact match wins; otherwise the alphabetically first furnished key
      # that narrows to what was asked. Same rule, same order, same reason
      # (../base/capability.nix's note on ../services/tempo.nix's gRPC port).
      matchOf =
        name:
        let
          cs = (factsOf name).caps;
        in
        if lib.elem cap cs then cap else lib.findFirst (g: lib.elem cap (capClosure g)) null cs;

      linkOf =
        name:
        let
          f = factsOf name;
          near = a.${name} or null;
          far = b.${name};
          m = matchOf name;
          extra = carriers.${name}.extra.${m} or { };
          assembled = {
            carrier = name;
          }
          //
            lib.optionalAttrs
              (lib.elem m [
                "tcp"
                "udp"
                "http"
                "address"
              ])
              {
                address = far.dialAt;
                interface = if near == null then null else near.hearsOn;
                inherit (f) mtu maxBodyBytes;
                inherit (f) namespaced;
              }
          // lib.optionalAttrs (m == "segment") { inherit (f) prefix; }
          // lib.optionalAttrs (m == "hostname") {
            hostname = far.hostnameAt;
            aliases = far.aliasesAt;
            inherit (f) domain;
          }
          // lib.optionalAttrs (m == "http") { inherit (f) domain; }
          // extra;
        in
        if m == null then { } else builtins.intersectAttrs (sideOptions "carrier" m) assembled;
    in
    map (name: {
      carrier = name;
      inherit sameHost;
      via = factsOf name;
      link = linkOf name;
    }) ranked;

  # ── THE COMPARISON, FIELD BY FIELD ──
  # Carrier, address, interface, mtu and maxBodyBytes, in preference order.
  # Stronger than "the carrier set and the addresses" on purpose: mtu and
  # maxBodyBytes were lifted out of each `gives` onto the carrier aspect by
  # this change, so the check is also what says that lift was inert.
  # ── READING A FIELD OFF A TYPED HALF, WHICH `or` CANNOT DO ──
  #
  # `linksBetween`'s link is a real submodule, not an attrset, and a DECLARED
  # option with no default THROWS when it is accessed unset rather than being
  # absent. `or` does not catch a throw, so `l.link.hostname or null` on a tcp
  # link died with
  #
  #   The option `capability.tcp.hostname' was accessed but has no value
  #   defined.
  #
  # for 127 of the 3630 triples -- every one that resolved over a loopback or
  # the tailnet -- and `checkLib.probe` reported the whole check as one throw.
  # `hostname` is declared on tcp's carrier side (through its surface) and
  # furnished only by a carrier that serves a namespace, which is exactly the
  # case this has to read as "no name" rather than as an error.
  #
  # Symmetric by construction: the fold's own links are plain attrsets, where
  # an absent field is absent, and `tryEval` succeeds and yields the same null.
  pick =
    set: f:
    let
      r = builtins.tryEval (set.${f} or null);
    in
    if r.success then r.value else null;

  brief =
    ls:
    map (l: {
      inherit (l) carrier;
      address = pick l.link "address";
      interface = pick l.link "interface";
      mtu = pick l.link "mtu";
      maxBodyBytes = pick l.link "maxBodyBytes";
      hostname = pick l.link "hostname";
      preference = l.via.preference;
    }) ls;

  show =
    xs:
    lib.concatMapStringsSep ", " (
      x:
      "${x.carrier}@${toString x.preference}(addr=${toString x.address}, if=${toString x.interface}, mtu=${toString x.mtu}, body=${toString x.maxBodyBytes}, name=${toString x.hostname})"
    ) xs;

  # ── THE FLEET, AND THE WHOLE CROSS PRODUCT ──
  hostEntities = checkLib.hostEntities;
  fleetRoster = poolFor hostEntities;
  capNames = lib.attrNames capabilities;

  # Every (hostA, hostB, capability) triple, with both answers. `linksBetween`
  # is asked here and nowhere else in this file.
  comparisons = lib.concatMap (
    A:
    lib.concatMap (
      B:
      map (c: {
        inherit A B c;
        mine = brief (linksFrom fleetRoster c A B);
        theirs = brief (lib.filter (l: lib.elem l.carrier carrierNames) (capability.linksBetween c A B));
        extras = lib.filter (l: !(lib.elem l.carrier carrierNames)) (capability.linksBetween c A B);
      }) capNames
    ) hostEntities
  ) hostEntities;
in
{
  # ── WHERE A CARRIER SAYS HOW ITS HALF IS READ ──
  #
  # One registration per carrier, in the carrier's OWN file, beside the `gives`
  # it shadows. Two consumers read it: the quirk emit (which turns it into
  # pooled data) and `poolFor` (which rebuilds the same pool from entities for
  # the check). One declaration, so they cannot disagree.
  #
  # TYPED, rather than checked: ../../CLAUDE.md -- a constraint belongs in the
  # option's type and a fallback in its default, so a carrier with no name to
  # furnish simply inherits `_: null` instead of every reader testing for one.
  options.den.carrierAttachments = mkOption {
    type = types.attrsOf (
      types.submodule (
        { name, config, ... }:
        {
          options = {
            # ── WHICH ENTITY KEYS ATTACH A HOST TO THIS SEGMENT ──
            #
            # `[ name ]`, DERIVED, so a carrier does not retype its own
            # identity to look itself up. `../network/segments.nix` read
            # `peer.attachments.lan-home.address` from inside
            # `den.aspects.lan-home`, which is the aspect's own name written a
            # second time -- and the two can disagree exactly once, silently,
            # because `attachments.<renamed>` is then MISSING and the `or null`
            # beside it swallows the hole. A literal is right for a foreign
            # name or for an instance name a take filters on; it is wrong for
            # one of our own identities when a reference already carries it.
            #
            # A LIST, because a segment is not a wire. A host attaches to the
            # house lan over whatever it has: auspc over `enp6s0` and over
            # `wlo1`, and ../network/house-wifi.nix adds its own key to THIS
            # list rather than presenting a second segment with the same
            # prefix. That is what makes `house-wifi` an attachment MECHANISM
            # -- it configures the association and yields an interface -- and
            # not a carrier of its own.
            attachesVia = mkOption {
              type = types.listOf types.str;
              default = [ name ];
              description = "The segment's OWN `attachments.<key>` entries on a host entity, in priority order. Defaults to the carrier's own name. A carrier with no entity attachment at all -- a loopback, a tunnel whose interface is derived -- sets `[ ]`.";
            };

            # ── AND THE KEYS AN ATTACHMENT MECHANISM CONTRIBUTES ──
            #
            # A SECOND option rather than more definitions of the one above,
            # and the reason is the module system rather than taste: a
            # definition SUPPRESSES a default, so `attachesVia = [
            # "house-wifi" ]` in ../network/house-wifi.nix would have replaced
            # `[ name ]` outright and lost the cable. Declaring `[ name ]`
            # there too would fix the merge and reintroduce exactly what
            # correction 2 removed -- the aspect's own identity, retyped.
            #
            # Appended, so ORDER IS BY CONSTRUCTION rather than by which file
            # the loader reached first: the segment's own key wins the address,
            # and a mechanism contributes an interface behind it. With a single
            # list and `lib.aspectFiles`' alphabetical order, `house-wifi.nix`
            # sorts before `segments.nix` and auspc's `hearsOn` would have come
            # back `wlo1` -- a different answer, chosen by a filename.
            alsoVia = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Further `attachments.<key>` entries contributed by attachment MECHANISMS -- an aspect that configures an association and yields an interface onto this segment without being a carrier of its own. Appended after `attachesVia`.";
            };

            keys = mkOption {
              type = types.listOf types.str;
              readOnly = true;
              default = config.attachesVia ++ config.alsoVia;
              description = "Every entity attachment key for this segment, segment-first.";
            };

            # ── WHERE OTHERS DIAL THIS HOST ──
            #
            # The FAR end's fact. Defaulted off `attachesVia` so a segment
            # declares nothing: the first attachment that carries an address
            # wins, which is the priority the list already states.
            #
            # `or null` IS THE CORRECT OPERATOR on the inner read and a `!=
            # null` test would NOT be, which is ../../CLAUDE.md's first trap in
            # its original form: `attachments.<k>` is a declared submodule, so
            # on a host that HAS the entry `address` exists -- possibly as null
            # -- while on a host with no entry at all the whole attribute is
            # missing. auspc is the live case: it declares
            # `attachments.house-wifi` with an interface and deliberately NO
            # address, because the radio takes a DHCP lease and a lease is not
            # a fixed address.
            dialAt = mkOption {
              type = types.functionTo (types.nullOr types.str);
              default =
                h: lib.findFirst (v: v != null) null (map (k: h.attachments.${k}.address or null) config.keys);
              defaultText = "the first `attachments.<key>.address` in `keys` that is not null";
              description = "Where OTHER hosts dial this one over this carrier. Null where the host is on the carrier but has no address of its own -- a published zone, or a host on a lease.";
            };

            # ── AND WHICH WIRES IT HEARS THEM ON ──
            #
            # The NEAR end's fact, and PLURAL: ../base/firewall.nix opens a
            # port on an interface, and a host on one segment over two wires
            # needs the port on both. Reading it off the peer would open one
            # host's ports on another's NIC name.
            interfacesOn = mkOption {
              type = types.functionTo (types.listOf types.str);
              default = h: lib.filter (v: v != null) (map (k: h.attachments.${k}.interface or null) config.keys);
              defaultText = "every non-null `attachments.<key>.interface` in `keys`";
              description = "Every kernel interface attaching this host to the carrier, segment-first.";
            };

            hearsOn = mkOption {
              type = types.functionTo (types.nullOr types.str);
              default =
                h:
                let
                  is = config.interfacesOn h;
                in
                if is == [ ] then null else lib.head is;
              defaultText = "the first of `interfacesOn`";
              description = "The one interface a single-valued link can hold, which is the first of `interfacesOn`. The plural is the honest answer; this is the one `capability.linksBetween` can carry.";
            };

            hostnameAt = mkOption {
              type = types.functionTo (types.nullOr types.str);
              default = _: null;
              description = "The name this carrier resolves for this host, where it serves a namespace. Null where it serves none -- which is most carriers, and is why a consumer wanting a name over the lan finds NO PROVIDER rather than a null.";
            };

            aliasesAt = mkOption {
              type = types.functionTo (types.listOf types.str);
              default = _: [ ];
              description = "Further names the carrier resolves for this host.";
            };

            extra = mkOption {
              type = types.attrsOf (types.attrsOf types.anything);
              default = { };
              description = "Per-capability constants the generic assembler has no slot for, keyed by capability name -- a loopback's `socketDir`, a zone's `protocol` and `port`. Facts, not logic.";
            };
          };
        }
      )
    );
    default = { };
    description = ''
      How each carrier's attachment half is read off a host entity, registered
      by the carrier aspect that owns the carrier.

      KEYED BY SEGMENT, not by wire. An attachment mechanism -- a wifi
      association, say -- adds its key to an existing segment's `attachesVia`
      instead of registering a carrier of its own, because two carriers with
      the same prefix are one network under two names and `preference` between
      them is a tiebreak between synonyms.
    '';
  };

  config = {
    # ── THE LIB SCOPE EACH CARRIER'S EMIT READS ──
    # Not an `import`: ../../CLAUDE.md -- a path import is re-evaluated per call
    # site and hard-codes the importer's position relative to this file. Same
    # route ../base/capability.nix takes for `capability` and
    # ../framework/checks.nix for `checkLib`.
    _module.args.carrierAttachment = {
      half = halfOf;
      inherit poolFor linksFrom factsOf;
    };

    # ── THE TRANSPORT FACTS, AS ASPECT FIELDS ──
    #
    # `mtu`, `maxBodyBytes` and `namespaced` were literals inside each
    # carrier's `gives`, which is a per-pair function -- so every pair on one
    # wire recomputed the same constant and no reader outside a resolved link
    # could ask for it. On the aspect they are declared once and read by both
    # paths, and the roster never pools them because they are not about a host.
    #
    # DECLARED, not freeform: den classifies an undeclared attrset-or-scalar
    # key on an aspect as a CHILD ASPECT, so `mtu = 1500` came back as a set
    # carrying a `__functor` rather than as the number. ../../CLAUDE.md's rule
    # -- a constraint belongs in the option's type -- and here it is also the
    # only way the value survives at all.
    #
    # Beside `prefix`, `domain`, `gatewayAddress` and `preference`, which
    # ../base/capability-types.nix already declares for the same reason. They
    # belong in that file; they are declared here only because it is being
    # edited elsewhere.
    den.schema.aspect.options = {
      mtu = mkOption {
        type = types.nullOr types.int;
        default = null;
        example = 1500;
        description = ''
          Largest packet this carrier passes unfragmented, where that is a
          fact about the carrier rather than about a pair on it. Read by the
          carrier's own `gives` and by ./carrier-attachment.nix's roster, so
          the two cannot drift.
        '';
      };

      maxBodyBytes = mkOption {
        type = types.nullOr types.int;
        default = null;
        example = 100000000;
        description = ''
          Largest request body this carrier passes; null is UNBOUNDED.
          Cloudflare caps a proxied body at 100MB and a WireGuard tunnel caps
          nothing, and a flow that sends bodies resolves or does not on this
          number.
        '';
      };

      namespaced = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether a process inside a network namespace on the host can reach
          the far end over this carrier. The fact that separates a container
          bridge from a loopback, and a fact about the carrier rather than a
          preference -- see ../network/container-bridge.nix.
        '';
      };
    };

    den.quirks.carrier-attachment.description = "one half of a carrier attachment: which carrier a host is on, where others dial it there, and which interface it hears it on";

    # ── THE POOL ──
    # `collectAll`, not `collect`: see the header -- four systems, and a host's
    # `collect` siblings are the hosts on its own system only. `withProvenance`
    # so the source host is the pipe's answer rather than a field in the record.
    den.policies.carrier-attachment-roster = _: [
      (pipe.from "carrier-attachment" [
        (pipe.collectAll ({ host, ... }: true))
        pipe.withProvenance
      ])
    ];

    # ── THE OBSERVATION POINT, AND IT IS INERT ──
    #
    # The roster is pooled data; nothing in this tree reads it yet, and a pipe
    # pool that nothing forces costs nothing. So this aspect exists only to
    # make the pooled value READABLE from outside, which is the only way to
    # check that the pipe carries what `poolFor` rebuilds:
    #
    #   nix eval .#nixosConfigurations.auspc.config.system.build.carrierRoster
    #
    # `system.build` because it changes no behaviour: nothing depends on an
    # attribute it does not name, so adding one alters no closure and no unit.
    #
    # NOT on `den.default.includes` -- a shadow that every host pays for is not
    # a shadow -- so it reaches a host only when somebody includes it. To turn
    # it on for the whole fleet for an afternoon, add
    #
    #   den.default.includes = [ den.aspects.carrier-roster ];
    #
    # here and remove it again. VERIFIED THAT WAY: with the line in,
    # `nixosConfigurations.auspc.config.system.build.carrierRoster` answered in
    # 10.2s with 20 entries covering all eleven hosts -- and
    # `source.host.name == value.host` held for every one of them, so
    # `withProvenance`'s tag and the record agree. It did NOT instantiate the
    # other ten hosts, which is the hazard this file's header is about: 10.2s
    # against the several GB and minutes a fleet-wide instantiation costs.
    #
    # The 20th entry is auspc's own loopback half, appearing TWICE. den
    # accumulates a quirk emit per delivery of its aspect (its own
    # `test-pipe-multi-def-same-aspect`), and `den.aspects.loopback` reaches
    # every host through `den.default.includes`. A consumer must therefore
    # dedupe -- the quirk recipes' own "merge in the consumer" -- and the fold
    # here is idempotent because `indexBy` keys by carrier.
    den.aspects.carrier-roster = {
      description = "Makes the pooled carrier attachment roster readable from a host, for the equivalence check's one-shot verification. Shadow only; nothing resolves through it.";

      includes = [ den.policies.carrier-attachment-roster ];

      nixos =
        { carrier-attachment, ... }:
        {
          system.build.carrierRoster = carrier-attachment;
        };
    };

    # ── THE REAL DELIVERABLE ──
    #
    # For every (hostA, hostB, capability) triple in the fleet, the roster fold
    # and `capability.linksBetween` must agree on the carrier set, the
    # addresses, the interfaces, the body caps and the order. Without this, a
    # swap is a guess.
    #
    # ENTITY-LEVEL, so it answers in seconds and never instantiates a host
    # (../framework/checks.nix's reason for computing findings at flake level).
    # `probe` because one broken aspect would otherwise take the whole
    # fleet-wide comparison down as an opaque trace.
    den.checks.carrier-roster-equals-resolution = {
      description = "The pooled attachment roster's local fold answers every (hostA, hostB, capability) triple exactly as `capability.linksBetween` does.";

      findings =
        checkLib.probe "carrier-roster-equals-resolution"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.carrier-roster-equals-resolution.findings'"
          (
            lib.concatMap (
              x:
              lib.optional (x.mine != x.theirs)
                "${x.A.name} -> ${x.B.name} over `${x.c}`: the roster fold says [${show x.mine}] and capability.linksBetween says [${show x.theirs}]."
            ) comparisons
          );

      fix = "The fold is in `linksFrom` in ./carrier-attachment.nix and the halves are the `den.carrierAttachments.<carrier>` registrations in each carrier's own file. A disagreement is one of three things: a half read off the wrong end (address is the FAR host's, interface the NEAR host's), a carrier fact still written inside a `gives` instead of on the aspect, or a clause in `linksFrom` that has drifted from the one it mirrors in ../base/capability.nix.";
    };

    # ── WHAT RESOLUTION RETURNS THAT NO CARRIER EMITS A HALF FOR ──
    #
    # `linksBetween cap A B` asks `providersFor A (carrierCapFor cap)`, and on
    # the SAME-HOST path there is no segment filter -- so every SERVICE on the
    # box that gives `http` or `tcp` is returned as a candidate beside the
    # carriers. Measured: `linksBetween "tcp" auspc auspc` returns `loopback`,
    # `lan-home`, `house-wifi`, `tailscale` and `openrgb`.
    #
    # That is resolution working as written rather than a fault, and the roster
    # deliberately does not reproduce it: a service is not a carrier and has no
    # attachment half to emit. Reported, deduped by name, so the set is visible
    # and so that a swap knows exactly what it would have to keep answering.
    #
    # `report`, not `error`: the finding is about the shape of the existing
    # resolver and nothing here can make it go away.
    den.checks.carrier-roster-covers-resolution = {
      description = "Which aspects `capability.linksBetween` returns as candidates that no carrier emits an attachment half for.";
      severity = "report";

      findings =
        checkLib.probe "carrier-roster-covers-resolution"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.carrier-roster-covers-resolution.findings'"
          (
            let
              seen = lib.concatMap (
                x:
                map (l: {
                  name = l.carrier;
                  where = "${x.A.name} -> ${x.B.name} over `${x.c}`";
                }) x.extras
              ) comparisons;
            in
            lib.mapAttrsToList (
              n: ws:
              "`${n}` is returned as a link candidate (${toString (lib.length ws)} triple(s), e.g. ${(lib.head ws).where}) and emits no `carrier-attachment` half."
            ) (lib.groupBy (e: e.name) seen)
          );

      fix = "Nothing, if it is a service: `linksBetween` admits any giver of the asked capability on the same host, and a service is not a carrier. If it is a genuine carrier, register it with `den.carrierAttachments.<name>` in its own aspect file.";
    };

    # ── A CARRIER THAT IGNORES WHO IS ASKING ──
    #
    # THE CHECK THAT TURNS THIS CLASS OF BUG FROM AN INVISIBLE WRONG ANSWER
    # INTO A BUILD FAILURE, and the most valuable thing in this file.
    #
    # A CARRIER's half is pair-dependent by definition: an address is a
    # property of the attachment between a host and a network, not a fact about
    # either end (../network/segments.nix's header). So a carrier furnishing an
    # ADDRESS or a NAME must look at `peer` -- and a giver written `_:` cannot.
    # It then claims the same address for every peer in the fleet, which is
    # true for nobody but the host itself.
    #
    # An ENDPOINT's half is pair-INDEPENDENT and `_:` is right there: a port
    # does not vary by who asks, which is why ../services/openrgb.nix's
    # `ports.sdk` is furnished with `_:` entirely legitimately. So the rule is
    # scoped to carriers -- the aspects registered in
    # `den.carrierAttachments` -- and not to every giver of a carrier-side
    # capability.
    #
    # ── MEASURED, ON THE TWO THAT HAD IT WRONG ──
    #
    #   den.aspects.loopback         tcp, udp, hostname, unix   all `_:`
    #   den.aspects.container-bridge tcp, udp                   both `_:`
    #
    # and the loopback's `tcp` unconditionally claiming `127.0.0.1` is what
    # made a derived firewall exposure open port 6742 on `lo` for a REMOTE
    # peer. Both are fixed; this is what keeps them fixed.
    #
    # ── WHICH CAPABILITIES THE RULE APPLIES TO ──
    #
    # Those whose carrier-side option set contains `address` or `hostname` --
    # the two fields that are a CLAIM ABOUT THE PEER. `segment` is exempt by
    # that rule rather than by an exception: a prefix is a fact about the wire
    # and identical for every pair on it. So is `unix`, whose own type says it
    # "cannot leave the host, because it is not a narrowing of `address`".
    #
    # ── AND `builtins.isFunction`, NOT `lib.isFunction` ──
    #
    # `lib.isFunction` is TRUE for a functor (an attrset carrying `__functor`),
    # and `builtins.functionArgs` throws on one -- measured: "error:
    # 'functionArgs' requires a function". den builds functor-carrying
    # attrsets for its synthetic aspect children, so the lib version turns this
    # check into an opaque trace rather than a finding.
    den.checks.carrier-givers-bind-the-peer = {
      description = "Every carrier furnishing an address or a name binds `peer`, so it cannot claim the same address for every host in the fleet.";

      findings =
        checkLib.probe "carrier-givers-bind-the-peer"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.carrier-givers-bind-the-peer.findings'"
          (
            lib.concatMap (
              name:
              let
                aspect = den.aspects.${name};
                given =
                  let
                    args = {
                      host = { };
                      self = aspect;
                      resolve = _: _: null;
                    };
                    g = aspect.gives or null;
                  in
                  if g != null && den.lib.canTake.atLeast args g then g args else { };

                # A claim about the peer is an address or a name. Read off the
                # capability's own carrier-side options, so adding a field that
                # is a claim brings it under the rule without editing this list.
                claimsPeer =
                  c:
                  let
                    o = sideOptions "carrier" c;
                  in
                  o ? address || o ? hostname;

                # A `provider` carrier publishes one route everyone reaches the
                # same way -- the cloudflare zone -- so furnishing one address
                # for the fleet is correct there, not a defect.
                oneSided = (aspect.attachment or "both") == "provider";
              in
              lib.concatMap (
                c:
                let
                  v = given.${c};
                  # `builtins.isFunction`: see the note above.
                  isFn = builtins.isFunction v;
                  binds = isFn && (builtins.functionArgs v) ? peer;
                in
                lib.optional (claimsPeer c && !binds && !oneSided) (
                  if isFn then
                    "den.aspects.${name}: `gives.${c}` takes { ${lib.concatStringsSep ", " (lib.attrNames (builtins.functionArgs v))} } and does not bind `peer`, so it furnishes the same ${
                      if (sideOptions "carrier" c) ? address then "address" else "name"
                    } for every host in the fleet. Written `_:`, it discards its arguments entirely."
                  else
                    "den.aspects.${name}: `gives.${c}` is not a function of the peer at all (it is ${builtins.typeOf v}), so it furnishes one ${
                      if (sideOptions "carrier" c) ? address then "address" else "name"
                    } for every host in the fleet."
                )
              ) (lib.attrNames given)
            ) carrierNames
          );

      fix = "Bind the peer and answer for it: `{ peer, ... }: if peer.name == host.name then <the half> else { address = null; interface = null; ... }`. A NULL ADDRESS is this layer's established word for \"does not connect\" -- ../network/house-wifi.nix has always used it, returning null for a peer with no attachment -- so a carrier that answers honestly self-filters and no resolver predicate has to compensate. Set `interface` to null too: a consumer reading it off a link with a null address would otherwise get the near end's wire for a peer that is not on it.";
    };

    # ── A HOST-LOCAL CARRIER MUST NEVER BE A CROSS-HOST CANDIDATE ──
    #
    # THE INVARIANT, STATED WHERE IT CAN FAIL LOUDLY. `127.0.0.1` on auspc is
    # not the same wire as `127.0.0.1` on secondpc, and neither is
    # `172.17.0.1`: ../network/container-bridge.nix exists precisely because
    # "`127.0.0.1` inside a container is the container, so a containerised
    # service pointed at the host's loopback reaches itself". Each host's
    # loopback and each host's bridge is its own wire that merely shares a name
    # and an address with every other host's.
    #
    # ── WHY THIS IS A CHECK AND NOT A COMMENT ──
    #
    # It has already cost a wrong answer downstream. Deriving firewall exposure
    # by taking the lowest-preference carrier that furnishes a non-null address
    # for the peer picked the LOOPBACK for a REMOTE peer and opened the port on
    # `lo` -- `{"ifaces":{"lo":[6742]}}`, where the answer was the LAN
    # interface. The final answer from `linksBetween` is right; the
    # INTERMEDIATE is not, and `shared` is a perfectly reasonable thing for a
    # consumer to reconstruct. Measured, for (auspc, secondpc) over `tcp`:
    #
    #   shared = lan-home, loopback, publish-ivymect-in, tailscale
    #   usable = lan-home, publish-ivymect-in, tailscale
    #
    # So the check is over the ANSWER of both paths, which is what a consumer
    # is entitled to trust, and the fold's structure (./carrier-attachment.nix's
    # `identityOf`) is what makes it true there rather than a predicate at the
    # end.
    den.checks.host-local-carriers-stay-on-one-host = {
      description = "No host-local carrier -- a loopback, a container bridge -- is ever a link candidate beyond its reach: a loopback between two entities, a bridge between two machines, on either resolution path.";

      findings =
        checkLib.probe "host-local-carriers-stay-on-one-host"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.host-local-carriers-stay-on-one-host.findings'"
          (
            lib.concatMap (
              x:
              lib.optionals (x.A.name != x.B.name) (
                let
                  # ── HOW FAR "ONE" GOES DEPENDS ON THE CARRIER ──
                  # A `one-host` carrier (a loopback) is one entity's own, so
                  # any two different entities over it is wrong -- a container's
                  # 127.0.0.1 is not its host's. A `one-machine` carrier (a
                  # bridge) joins every entity on one box, so it is wrong only
                  # between two DIFFERENT machines. Comparing names alone would
                  # report every container talking to its own host.
                  crosses = n: if (factsOf n).spans == "one-machine" then machineOf x.A != machineOf x.B else true;

                  bad = lib.unique (
                    lib.filter (n: lib.elem n carrierNames && hostLocal n && crosses n) (
                      map (l: l.carrier) x.mine ++ map (l: l.carrier) x.theirs
                    )
                  );
                in
                lib.optional (bad != [ ])
                  "${x.A.name} -> ${x.B.name} over `${x.c}`: ${
                    lib.concatMapStringsSep ", " (n: "`${n}`") bad
                  } is host-local (no `segment`, not a provider) and is offered as a link between two different hosts. Its address means a different machine at each end."
              )
            ) comparisons
          );

      fix = "In the roster, host-local carriers carry the host in their IDENTITY (`identityOf` in ./carrier-attachment.nix), so two hosts' loopbacks never meet and there is no filter to forget. In ../base/capability.nix the equivalent is `satisfies ... \"segment\"` on the off-host path -- correct, and a predicate at the end rather than a structure, so an intermediate like `shared` is still wrong.";
    };

    # ── AND THE SAME QUESTION ASKED OF THE DATA, NOT OF THE SIGNATURE ──
    #
    # `carrier-givers-bind-the-peer` above checks the SHAPE: does the giver
    # look at who is asking. This checks the ANSWER: having looked, does it say
    # no. A giver can bind `peer`, ignore it, and be just as wrong.
    #
    # Asked of `capability.carrierHalf` -- the LIVE giver, through the real
    # resolver, not through the roster -- for every cross-host pair and every
    # capability a host-local carrier furnishes. A host-local carrier is one
    # attached to by exactly one host (no `segment`, not a provider), so there
    # is no pair of DIFFERENT hosts it can hand an address to.
    #
    # This is the check that would have caught the live bug on its own: before
    # ../network/segments.nix's loopback bound `peer`, it furnished
    # `127.0.0.1` for all 110 ordered cross-host pairs, and
    # ../network/container-bridge.nix furnished `172.17.0.1` the same way.
    # `linksBetween`'s final answer was right throughout -- the segment clause
    # removed them -- so nothing anywhere said so.
    den.checks.carriers-claim-no-address-for-an-unattached-peer = {
      description = "No host-local carrier furnishes a non-null address for a peer that is not its own host, which is an address meaning a different machine at each end.";

      findings =
        checkLib.probe "carriers-claim-no-address-for-an-unattached-peer"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.carriers-claim-no-address-for-an-unattached-peer.findings'"
          (
            let
              locals = lib.filter hostLocal carrierNames;
              rows = lib.concatMap (
                name:
                lib.concatMap (
                  A:
                  lib.concatMap (
                    B:
                    lib.optionals (A.name != B.name && A.hasAspect den.aspects.${name}) (
                      lib.concatMap (
                        c:
                        let
                          half = capability.carrierHalf den.aspects.${name} c {
                            host = A;
                            peer = B;
                          };
                          addr = if half == null then null else pick half "address";
                        in
                        lib.optional (addr != null) {
                          inherit name c addr;
                          where = "${A.name} -> ${B.name}";
                        }
                      ) (factsOf name).caps
                    )
                  ) hostEntities
                ) hostEntities
              ) locals;
            in
            lib.mapAttrsToList (
              k: rs:
              "${k} furnishes the address ${(lib.head rs).addr} for a peer that is not its host, in ${toString (lib.length rs)} cross-host case(s), e.g. ${(lib.head rs).where}. That address means a different machine at each end."
            ) (lib.groupBy (r: "den.aspects.${r.name}: `gives.${r.c}`") rows)
          );

      fix = "The carrier answers it, not the resolver: bind `peer` and furnish `address = null` (and `interface = null`) when the peer is not the host. See the fix on `carrier-givers-bind-the-peer`.";
    };

    # ── TWO NAMES FOR ONE NETWORK ──
    #
    # THE CHECK THAT WOULD HAVE CAUGHT THE MODELLING ERROR THIS FILE FIXES.
    # ../network/house-wifi.nix furnished a `segment` whose `prefix` was READ
    # OFF `lan-home` -- so the two were the same wire by construction, and the
    # resolver had two carriers for it, ordered against each other by
    # `preference` 25 against 20. ../base/capability-types.nix defines a
    # segment as "Attachment to a shared wire. Several hosts on it are
    # siblings", so equal prefixes are not a coincidence to be ranked; they are
    # the same segment, and a host on it over two wires has two ATTACHMENTS and
    # not two carriers.
    #
    # The cost was not theoretical. ../services/openrgb.nix names both carriers
    # in one `expose` so a firewall rule lands on `enp6s0` AND `wlo1`;
    # ../services/corsair-psu.nix carries the same note; ../hosts/auspc.nix's
    # own ⚠ says "`lan-home` belongs on the radio, which is what the house-wifi
    # carrier providing a lan link was meant to express"; and
    # ../hosts/fleet.nix's interface check exists to catch the disagreement
    # between the two hand-written halves. One segment with
    # `attachesVia = [ "lan-home" "house-wifi" ]` answers all four.
    #
    # `prefix` is COMPARED HERE and nowhere else: ../base/capability.nix's
    # `linksBetween` matches carriers by NAME and never compares prefixes, and
    # its own comment says lifting that is two changes. This check is the other
    # half of that note -- it does not lift the restriction, it reports the case
    # where the restriction is hiding a duplicate.
    # ── A CARRIER SAYS HOW FAR IT GOES, AND THE PAIRING IS CHECKED ──
    #
    # `spans` replaced a fact stored as a missing attribute, so the new way to
    # get this wrong is to leave the declared one out: a carrier with no
    # `spans` is neither host-local nor a shared wire, which would drop it from
    # the cross-host filter AND from `hostLocal` without a word. That is the
    # same silence the old shape had, so it is an error here.
    #
    # AND THE PREFIX GOES WITH IT. A wire several hosts share has a CIDR,
    # identical for everything on it; a loopback spans one box and has none.
    # The two facts have to agree, and `linksBetween` rests on the prefix being
    # right (../network/segments.nix measures what `/24` would have cost).
    den.checks.carriers-declare-how-far-they-span = {
      description = "Every carrier declares `spans`, and a wire several hosts share carries a prefix.";
      fix = "Set `spans = \"many-hosts\"` on a wire hosts share (with its CIDR in `prefix`) or `spans = \"one-host\"` on a loopback or a bridge, which is a whole network that stops at one box.";
      findings =
        checkLib.probe "carriers-declare-how-far-they-span"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.carriers-declare-how-far-they-span.findings'"
          (
            lib.concatMap (
              n:
              let
                f = factsOf n;
              in
              # ── A PROVIDER IS NOT A WIRE, SO IT HAS NO SPAN ──
              # A published zone reaches whoever can resolve the name, which is
              # why `attachment = "provider"` bypasses the segment test in
              # ../base/capability.nix rather than claiming to span hosts. It is
              # exempt here for the same reason, and this was MEASURED rather
              # than assumed: the first version of this check reported
              # `publish-ivymect-in`, which is a zone.
              if f.attachment == "provider" then
                [ ]
              else if f.spans == null then
                [
                  "den.aspects.${n} is a carrier and declares no `spans`, so it counts as neither a host-local wire nor one several hosts share -- it is silently dropped from every cross-host link search and from `hostLocal` alike."
                ]
              else
                lib.optional (f.spans == "many-hosts" && f.prefix == null)
                  "den.aspects.${n} spans many hosts and carries no `prefix`, so nothing says which addresses are on it -- resolution matches carriers by name and this is the fact that says two hosts are siblings at all."
            ) carrierNames
          );
    };

    den.checks.segments-have-one-name = {
      description = "No two carriers furnish a `segment` with the same prefix, which would be one network under two names ordered against itself by `preference`.";

      findings =
        checkLib.probe "segments-have-one-name"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.segments-have-one-name.findings'"
          (
            let
              segmentCarriers = lib.filter (
                n: lib.elem "segment" (capsOf n) && (den.aspects.${n}.prefix or null) != null
              ) (lib.attrNames den.aspects);
            in
            lib.concatLists (
              lib.mapAttrsToList (
                p: ns:
                lib.optional (lib.length ns > 1)
                  "${
                    lib.concatMapStringsSep " and " (n: "den.aspects.${n}") ns
                  } each furnish a `segment` with prefix ${p}, so they are one wire under ${toString (lib.length ns)} names -- and `preference` (${
                    lib.concatMapStringsSep ", " (n: "${n}=${toString den.aspects.${n}.preference}") ns
                  }) ranks them against each other instead of ranking routes."
              ) (lib.groupBy (n: den.aspects.${n}.prefix) segmentCarriers)
            )
          );

      fix = "Keep ONE segment and make the other an attachment mechanism: drop its `segment`, its `preference` and its `prefix`, keep whatever it configures, and add its entity attachment key to the segment's `den.carrierAttachments.<segment>.alsoVia` (./carrier-attachment.nix). A host then states one attachment per wire and the segment has one name.";
    };

    # ── WHERE ONE SEGMENT REACHES A HOST OVER MORE THAN ONE WIRE ──
    #
    # The evidence for the paragraph above, as data rather than as prose, and
    # the thing `capability.linksBetween`'s link CANNOT carry: its `interface`
    # is one string, so a host on the house lan over a cable and a radio gets
    # whichever comes first and the other wire is invisible to
    # ../base/firewall.nix. That is why `expose` names two carriers.
    #
    # `report`: every line here is correct and currently unexploited. It
    # becomes actionable when a consumer reads `interfaces` off the roster
    # instead of naming carriers, at which point `expose = [ "lan-home" ]`
    # covers both wires on its own.
    den.checks.carrier-roster-segment-interfaces = {
      description = "Which hosts attach to one segment over more than one wire, where a single-valued `interface` on a resolved link can only name the first.";
      severity = "report";

      findings =
        checkLib.probe "carrier-roster-segment-interfaces"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.carrier-roster-segment-interfaces.findings'"
          (
            lib.concatMap (
              e:
              # `unique` FIRST: two carriers onto one segment may name the SAME
              # wire, and that is not a finding -- a host whose cable is out and
              # whose address is on its radio has every attachment pointing at
              # the radio, and reporting "over wlo1 and wlo1" is noise. What
              # matters is more than one DISTINCT interface, because then a
              # single-valued `interface` on a resolved link can only name one.
              let
                wires = lib.unique e.interfaces;
              in
              lib.optional (lib.length wires > 1)
                "${e.host} attaches to `${e.carrier}` over ${lib.concatStringsSep " and " wires} (dialled at ${toString e.dialAt}). A resolved link carries only `${toString e.hearsOn}`, so a port exposed on that carrier is opened on one of them."
            ) fleetRoster
          );

      fix = "Nothing to fix in the roster -- it already holds both. The consumer side is what changes: read `interfaces` off the host's `carrier-attachment` pool rather than naming a second carrier in `expose`.";
    };

    # ── A CARRIER'S KEY SET IS A CARRIER FACT, AND THIS IS WHY ──
    #
    # `factsOf` reads `caps` once per carrier, with a NEUTRAL host, on the
    # grounds that the outer level of a `gives` returns a fixed set of keys and
    # only the values inside depend on the peer. If that stopped being true --
    # a carrier furnishing `hostname` only on hosts that run the resolver, say
    # -- the fold would answer for the wrong set and `linksFrom` would be
    # wrong in a way the equivalence check could easily pass over, because both
    # sides would be reading the same stale list.
    #
    # So it is checked rather than assumed, against every host in the fleet.
    den.checks.carrier-attachment-caps-are-host-independent = {
      description = "Every carrier furnishes the same set of capability keys whatever host asks, which is what lets the roster treat it as a carrier fact rather than a pooled one.";

      findings =
        checkLib.probe "carrier-attachment-caps-are-host-independent"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.carrier-attachment-caps-are-host-independent.findings'"
          (
            lib.concatMap (
              n:
              let
                neutral = capsOf n;
              in
              lib.concatMap (
                h:
                let
                  here = capsOfWith h n;
                in
                lib.optional (h.hasAspect den.aspects.${n} && here != neutral)
                  "den.aspects.${n}: `gives` furnishes [${lib.concatStringsSep " " here}] on ${h.name} and [${lib.concatStringsSep " " neutral}] with no host, so which capabilities this carrier answers is not a carrier fact."
              ) hostEntities
            ) carrierNames
          );

      fix = "Either make the key set unconditional (the values inside may still depend on the peer -- that is what the halves are for), or move `caps` out of `factsOf` and into the pooled half in ./carrier-attachment.nix.";
    };

    # ── AND THAT A HALF IS ENTITY-ONLY, STRUCTURALLY ──
    #
    # ../base/capability.nix already checks that no cross-host EMISSION binds
    # `config`. This checks the other side of the same hazard for this pipe:
    # a `den.carrierAttachments` reader is applied to a host ENTITY, so a
    # reader that wanted `config` could not be written -- but one could still
    # be handed a thunk, and `types.functionTo (nullOr str)` is what refuses
    # it. The remaining hole is a reader that READS something only a
    # config-bearing entity has, which shows up as a throw rather than a type
    # error, so every half in the fleet pool is forced here.
    den.checks.carrier-attachment-halves-are-entity-only = {
      description = "Every carrier attachment half in the fleet pool is readable from a host entity alone, with no host configuration instantiated.";

      findings =
        checkLib.probe "carrier-attachment-halves-are-entity-only"
          "nix eval --impure .#debug.config --apply 'cfg: cfg.den.checks.carrier-attachment-halves-are-entity-only.findings'"
          (
            lib.concatMap (
              e:
              let
                r = builtins.tryEval (builtins.deepSeq e true);
              in
              lib.optional (!r.success)
                "den.carrierAttachments.${e.carrier}: the half for ${e.host} threw when forced, so it is not readable from the entity alone -- which is what would turn the fleet-wide gather into a fleet-wide host instantiation."
            ) fleetRoster
          );

      fix = "A half may read `host.name`, `host.attachments.<carrier>` and the carrier aspect's own declared facts. It may not reach into a host's configuration: `den.carrierAttachments.<n>.dialAt` is applied to an ENTITY, and anything it cannot answer from there belongs in the carrier's `gives` and not in the roster.";
    };

    # The pool, as data, without a host: the loop to develop the fold in.
    flake.carrierRoster = {
      halves = fleetRoster;
      carriers = lib.genAttrs carrierNames factsOf;
    };
  };
}
