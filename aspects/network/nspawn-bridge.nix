{
  den,
  lib,
  carrierAttachment,
  ...
}:
# ── THE BRIDGE NIXOS CONTAINERS ATTACH TO ──
#
# ./container-bridge.nix is docker's `docker0`, and it can only ever answer
# with its gateway: the docker daemon assigns container addresses at runtime,
# so there is no eval-time address to dial a container at. That is correct for
# OCI and it is why an OCI container cannot be a dialable entity.
#
# A systemd-nspawn container is the opposite. Its `localAddress` is DECLARED,
# so its address is an eval-time fact, and it can sit on its own entity as
# `attachments.nspawn-bridge.address` like any host on the lan. So this carrier
# reads the address off the PEER's attachment, exactly as `lan-home` does, and
# a container entity becomes something `dial` can reach and dial from.
#
# `spans = "one-machine"`: the wire joins a container to the machine it runs on
# (its entity's `parent`) and never leaves the box.
#
# Its own range: `10.42.0.0/24` is clear of the lan (192.168.0.0/16), docker
# (172.17.0.0/16), wireguard (10.100.0.0/24) and the tailnet (100.64.0.0/10).
{
  den.aspects.nspawn-bridge = {
    gives =
      {
        self,
        host ? { },
        ...
      }:
      let
        machineOf = h: lib.defaultTo (h.name or null) (h.parent or null);
        sameMachine = a: b: machineOf a != null && machineOf a == machineOf b;
        # The machine's side is the bridge itself, so no host restates it.
        attachedOf =
          h:
          let
            stated = h.attachments.${self.name} or { };
            isMachine = (h.parent or null) == null;
          in
          {
            address = lib.defaultTo (if isMachine then self.gatewayAddress else null) (stated.address or null);
            interface = lib.defaultTo (if isMachine then self.bridgeInterface else null) (
              stated.interface or null
            );
          };
        link =
          { peer, ... }:
          {
            address = if sameMachine host peer then (attachedOf peer).address else null;
            interface = if sameMachine host peer then (attachedOf host).interface else null;
            inherit (self) namespaced mtu maxBodyBytes;
          };
      in
      {
        tcp = link;
        udp = link;
        segment = _: { inherit (self) prefix spans; };
      };

    # Still one box, so shallower than any wire that leaves it -- and AHEAD of
    # `container-bridge` (10) rather than tied with it. Both join entities on
    # one machine, but only this one can address a particular entity; docker's
    # answers with its gateway, so a tie left the choice to sort order.
    preference = 9;

    spans = "one-machine";
    prefix = "10.42.0.0/24";
    gatewayAddress = "10.42.0.1";
    bridgeInterface = "nspawn0";
    namespaced = true;
    mtu = 1500;
    maxBodyBytes = null;

    # Reaching the peer's address on the bridge, which is what "is that
    # container up" means at this level.
    probe = { link, ... }: "ping -c1 -W2 ${link.address}";

    # The bridge itself is created by the host that runs an nspawn container
    # (../base/container.nix), never by the container.
  };

  # ── THIS BRIDGE'S HALF OF AN ATTACHMENT ──
  # The machine's side defaults to the bridge itself, as the giver's does.
  den.carrierAttachments.nspawn-bridge =
    let
      me = den.aspects.nspawn-bridge;
      isMachine = h: (h.parent or null) == null;
    in
    {
      dialAt =
        h:
        lib.defaultTo (if isMachine h then me.gatewayAddress else null) (
          h.attachments.nspawn-bridge.address or null
        );
      interfacesOn =
        h:
        let
          i = lib.defaultTo (if isMachine h then me.bridgeInterface else null) (
            h.attachments.nspawn-bridge.interface or null
          );
        in
        lib.optional (i != null) i;
    };

  den.aspects.nspawn-bridge.carrier-attachment = { host, ... }: [
    (carrierAttachment.half "nspawn-bridge" host)
  ];
}
