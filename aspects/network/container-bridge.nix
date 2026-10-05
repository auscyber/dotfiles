{
  den,
  lib,
  carrierAttachment,
  ...
}:
# ── THE WIRE BETWEEN A CONTAINER AND THE BOX IT RUNS ON ──
#
# A carrier like any other: it has an address, an interface and an MTU, and a
# consumer reaches it by resolution rather than by knowing a number. What
# makes it worth modelling is that the obvious alternative is a lie --
# `127.0.0.1` inside a container is the container, so a containerised service
# pointed at the host's loopback reaches itself.
#
# The tree's answer until now was `--network=host`, which makes `localhost`
# true again by deleting the namespace. ../services/splitpro.nix says so in as
# many words: "Host networking so it reaches postgres on loopback and serves
# on 127.0.0.1:3000 behind nginx". That works and it costs the thing a
# namespace is for: splitpro and grafana both bound 127.0.0.1:3000 on
# secondpc, nginx proxied two vhosts to one socket, and whichever service lost
# the race was simply not running. Nobody noticed until both declared their
# ports where ../base/capability.nix could compare them.
#
# ── WHY IT IS A CARRIER AND NOT A CONSTANT ──
#
# The gateway address is a deployment fact, like `lanAddress`: it is whatever
# the daemon's bridge was configured with, it differs between docker and
# podman, and a second container on another host will have a different one.
# As a carrier it is declared once per host and every consumer resolves it;
# as a constant it would be `172.17.0.1` copied into each container's
# connection string, which is the shape this tree keeps deleting.
#
# ── AND WHY THE LOOPBACK DOES NOT SIMPLY LOSE ──
#
# `namespaced` (../base/capability-types.nix) is what distinguishes them, and
# it is a fact about the carrier rather than a preference: the loopback is
# perfectly correct for everything NOT in a namespace, and that is most
# things. A containerised consumer filters on it; everyone else never sees
# this carrier because `preference` puts it after the loopback and the clause
# never matches for them.
{
  den.aspects.container-bridge = {
    # ── `{ peer, ... }`, FOR ./segments.nix's REASON EXACTLY ──
    #
    # This giver discarded its arguments too, so it claimed `172.17.0.1` for
    # any peer -- and this carrier is the one whose header already says why
    # that is wrong: "`127.0.0.1` inside a container is the container". Each
    # host's bridge is its own wire, sharing a name and an address with every
    # other host's, so a half furnished for a remote peer is the same class of
    # wrong answer that opened a port on `lo`.
    #
    # `reach = "host"` below already said both ends are the same machine. That
    # is a statement about who may RESOLVE this carrier; it is not a null
    # address, and nothing downstream reads it as one.
    gives =
      {
        self,
        host ? { },
        ...
      }:
      let
        # ── ONE MACHINE, NOT ONE ENTITY ──
        # This answered only `peer.name == host.name`, which was "the same box"
        # while every entity was its own machine. A container is an entity on
        # its parent's machine, and it reaches that machine over this bridge.
        machineOf = h: lib.defaultTo (h.name or null) (h.parent or null);
        sameMachine = a: b: machineOf a != null && machineOf a == machineOf b;
        isMachine = h: (h.parent or null) == null;

        # ── EACH ENTITY'S OWN HALF OF THE ATTACHMENT ──
        # A container and its host are separate entities, so they attach to
        # this bridge differently: the machine is the NAT side (the gateway,
        # heard on `docker0`), a container is its own side (`eth0`, an address
        # docker hands out at runtime). Read off the entity, like `lan-home` --
        # with the MACHINE's half defaulting to the daemon's declared facts, so
        # no docker host has to restate `172.17.0.1`.
        attachedOf =
          h:
          let
            stated = h.attachments.${self.name} or { };
            default =
              if isMachine h then
                {
                  address = self.gatewayAddress;
                  interface = self.bridgeInterface;
                }
              else
                {
                  address = null;
                  interface = null;
                };
          in
          {
            address = lib.defaultTo default.address (stated.address or null);
            interface = lib.defaultTo default.interface (stated.interface or null);
          };

        link =
          { peer, ... }:
          {
            # Where the asker dials the peer, and where this side hears it --
            # each off its own entity, and only within one machine.
            # An OCI container has no address of its own until docker runs
            # it; the machine reaches it where the runtime publishes its
            # ports, its own loopback (../base/container.nix).
            address =
              let
                stated = (attachedOf peer).address;
              in
              if !(sameMachine host peer) then
                null
              else if stated == null && isMachine host then
                "127.0.0.1"
              else
                stated;
            interface = if sameMachine host peer then (attachedOf host).interface else null;

            inherit (self) namespaced;
            inherit (self) mtu maxBodyBytes;
          };
      in
      {
        tcp = link;
        udp = link;
        # A segment like nspawn's: what a routed taker still reaches directly.
        segment = _: { inherit (self) prefix spans; };
      };

    # AFTER THE LOOPBACK (0) AND BEFORE A WIRE (20): this is still one box,
    # so it is shallower than anything that leaves it -- but it is a bridge
    # and a NAT hop rather than a direct socket, so it is not free either.
    preference = 10;

    # ── EVERY ENTITY ON THIS MACHINE, NOT JUST THIS ONE ──
    # It has a prefix and is still not shared between machines, which is why
    # carrying a prefix was never the right test for a segment. And it is not
    # `one-host` either: a container on this bridge is an ENTITY
    # (../base/capability-types.nix's `spans`), so the wire between it and this
    # host joins two of them without leaving the box.
    #
    # NOTE this carrier still answers with the GATEWAY only (`dialAt` below):
    # the docker daemon assigns container addresses at runtime, so there is no
    # eval-time per-entity address here. An nspawn bridge, whose `localAddress`
    # is declared, is the one that can carry per-entity addresses.
    spans = "one-machine";

    # Both ends are the same machine, so there is nothing to attach to.
    reach = "host";

    # The daemon's own facts, declared where the carrier is rather than in
    # each container. `prefix` is what a consumer's source address falls in,
    # which is what ../services/postgres.nix needs for its `pg_hba` line --
    # routing to the host is only half of reaching it.
    gatewayAddress = "172.17.0.1";
    bridgeInterface = "docker0";
    prefix = "172.17.0.0/16";

    # The three transport facts, off the per-pair link. `namespaced` is the one
    # that distinguishes this carrier from the loopback, and it is a fact about
    # the carrier rather than a preference -- see the header.
    namespaced = true;
    mtu = 1500;
    maxBodyBytes = null;
  };

  # ── THE BRIDGE'S HALF OF AN ATTACHMENT ──
  #
  # See ./carrier-attachment.nix. BOTH HALVES ARE CARRIER CONSTANTS here, and
  # that is the honest shape rather than a degenerate one: both ends are the
  # same machine (`reach = "host"`), so there is nothing per-pair to say. What
  # the pooled roster still carries is the fact that matters -- WHICH hosts run
  # a bridge at all -- and a host whose daemon is configured differently says
  # so by setting `gatewayAddress` on the aspect, which both halves and both
  # paths then read.
  den.carrierAttachments.container-bridge = {
    # NO `attachesVia`: there is nothing per-host to attach. Both ends are the
    # same machine, and the daemon's own facts are on the aspect.
    attachesVia = [ ];
    dialAt = _: den.aspects.container-bridge.gatewayAddress;
    interfacesOn = _: [ den.aspects.container-bridge.bridgeInterface ];
  };

  den.aspects.container-bridge.carrier-attachment =
    { host, ... }:
    [ (carrierAttachment.half "container-bridge" host) ];
}
