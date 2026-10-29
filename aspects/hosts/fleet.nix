# The fleet as one attrset, for the aspects that need every machine at once
# (ssh known_hosts and aliases, the wireguard peer list, remote builders,
# tailscale enrolment). Built from `den.hosts` -- each host declares its own
# `hostPublicKey`, `builder` and `device` -- plus `fleet.external`, for machines
# we dial but do not build. Handed to modules as the `fleet` argument.
#
#   <name> = {
#     system;          den host only
#     hostPublicKey;   only when declared
#     user;            the login account peers SSH into: the host's user
#     uid;             that account's uid (501 on darwin, 1000 elsewhere)
#     builder;         `nix.buildMachines` record, or null
#     device;          tailscale enrolment record, or null
#     lanAddress;      fixed home-LAN address, or null
#   }
{
  den,
  lib,
  config,
  checkLib,
  ...
}:
let
  inherit (lib) mkOption types;
  record = types.nullOr (types.attrsOf types.anything);

  fromHost =
    _: h:
    {
      inherit (h)
        system
        builder
        device
        ;

      # DERIVED from the attachment, so a host states its address once. Falls
      # back to the flat field for anything not migrated. (`||` is boolean or
      # in Nix, not a null coalesce -- it would compare two strings.)
      lanAddress =
        let
          attached = h.attachments.lan-home.address or null;
        in
        if attached != null then attached else h.lanAddress;
      user = if h.users == { } then null else lib.head (builtins.attrNames h.users);
      uid = if lib.hasSuffix "darwin" h.system then 501 else 1000;
    }
    // lib.optionalAttrs h.hasHostPublicKey { inherit (h) hostPublicKey; };
in
{
  options.fleet.external = mkOption {
    type = types.attrsOf (types.attrsOf types.anything);
    default = { };
    description = "Machines in the fleet that are not den hosts, in the same shape as the rest of `fleet`.";
  };

  config = {
    # ── IS THE ATTACHMENT'S INTERFACE THE ONE CARRYING ITS ADDRESS ──
    #
    # `attachments.<carrier>` is two facts that have to agree: where other hosts
    # dial this one, and which wire this one hears them on. ../base/firewall.nix
    # writes every lan-exposed rule against the INTERFACE, so an attachment
    # naming the wrong one produces a resolvable, correctly-configured,
    # unreachable service -- the rules land on a wire that is not carrying the
    # traffic, and nothing anywhere says so.
    #
    # ── WHY THIS IS AN ASSERTION'S SHAPE AND NOT A FLAKE CHECK ──
    #
    # The attachment is declared data and so is cheap to read; the answer it has
    # to be compared against is not. "Which interface does this host give that
    # address to" lives in `networking.interfaces` / `systemd.network`, which
    # is rendered config -- several GB and one host at a time (../../CLAUDE.md).
    # A host already pays for its own evaluation when it is built, so the
    # comparison is free exactly there and nowhere else. The option's own
    # description says the same thing from the other side: reading those back is
    # why the attachment exists on the entity at all.
    #
    # ── A WARNING, NOT AN ASSERTION ──
    #
    # Two reasons, and the second is the honest one. A static address is only
    # one of the ways an interface gets one, so an address this cannot find is
    # not evidence of a mistake -- a DHCP lease or an unmanaged NIC is silent
    # here and must stay buildable. And the disagreement it DOES find is a
    # one-line edit on a machine that is otherwise working, which does not
    # deserve to block a rebuild.
    #
    # ── AND WHAT IT STILL CANNOT SEE ──
    #
    # ../hosts/auspc.nix is the case this check was written for and the case it
    # does NOT catch: `attachments.lan-home.interface = "enp6s0"` and
    # `networking.interfaces.enp6s0.ipv4.addresses = [ 192.168.1.11 ]` AGREE,
    # and the box disagrees with both -- `ip -br addr` puts that address on
    # `wlo1`, because no cable is plugged in. Declared-against-declared cannot
    # reach that; only the running machine can. ../framework/requires.nix is the
    # surface for it (`ip -4 -o addr show dev enp6s0 | grep -q 192.168.1.11`)
    # and it is deliberately NOT wired up here: a failing `requires` makes
    # ../tooling/nh-checked.nix select a specialisation that excludes the
    # failing aspect, and nothing excludes a host's own attachment, so auspc
    # would refuse to build instead of warning.
    den.aspects.attachment-interface-check = {
      description = "Warns when an attachment names an interface this host's own config does not give that attachment's address to.";

      nixos =
        {
          config,
          host ? { },
          ...
        }:
        {
          warnings =
            let
              me = checkLib.entityFor host;

              # ── BOTH WAYS THIS TREE ASSIGNS AN ADDRESS ──
              # `networking.interfaces.<n>.ipv4.addresses` is the scripted/NM
              # route; `systemd.network.networks.<n>.address` is networkd's, and
              # its interface is a MATCH rather than the attribute name -- so
              # the name is read off `matchConfig.Name`, which is what the hosts
              # that use networkd actually set.
              fromScripted = lib.concatLists (
                lib.mapAttrsToList (
                  iface: i:
                  map (a: {
                    inherit iface;
                    inherit (a) address;
                  }) i.ipv4.addresses
                ) config.networking.interfaces
              );

              fromNetworkd = lib.concatLists (
                lib.mapAttrsToList (
                  _: n:
                  let
                    iface = n.matchConfig.Name or null;
                  in
                  if iface == null then
                    [ ]
                  else
                    map (a: {
                      inherit iface;
                      # `address` carries a prefix (`192.168.0.26/16`); the
                      # attachment does not.
                      address = lib.head (lib.splitString "/" a);
                    }) n.address
                ) config.systemd.network.networks
              );

              assigned = fromScripted ++ fromNetworkd;

              carries = addr: lib.unique (map (a: a.iface) (lib.filter (a: a.address == addr) assigned));
            in
            lib.concatLists (
              lib.mapAttrsToList (
                carrier: attach:
                let
                  where = if attach.address == null then [ ] else carries attach.address;
                in
                # NO FINDING WHEN NOTHING CLAIMS THE ADDRESS. A lease, an
                # unmanaged NIC or an address set outside these two options is
                # not a mistake, and ../../CLAUDE.md's rule about `or` on a null
                # applies to the address itself: it is a declared option with a
                # null default, so it EXISTS and has to be tested.
                if attach.address == null || where == [ ] then
                  [ ]
                else if attach.interface == null then
                  [
                    "attachments.${carrier} on ${me.name or "this host"} has address ${attach.address} and no `interface`, but this host's config puts that address on ${lib.concatStringsSep ", " where}. ../base/firewall.nix has no interface to open this carrier's exposed ports on, so they are reported as unresolved rather than opened. Set `attachments.${carrier}.interface = \"${lib.head where}\"`."
                  ]
                else if !(lib.elem attach.interface where) then
                  [
                    "attachments.${carrier} on ${me.name or "this host"} names interface `${attach.interface}`, but this host's own config assigns ${attach.address} to ${lib.concatStringsSep ", " where}. ../base/firewall.nix writes this carrier's lan-exposed rules against `${attach.interface}`, so they land on a wire that is not carrying that address -- a resolvable, correctly-configured service that cannot be reached. Either move the address or set `attachments.${carrier}.interface = \"${lib.head where}\"`."
                  ]
                else
                  [ ]
              ) (me.attachments or { })
            );
        };
    };

    # ── AND WHETHER TWO HOSTS CLAIM THE SAME ADDRESS ──
    #
    # Fleet-wide and therefore not answerable from any one host, which is what
    # makes it a flake check rather than another assertion: an attachment is
    # where other hosts DIAL this one, so two entries with one address send two
    # services' traffic to whichever box answered ARP first. Pure entity data,
    # so it costs nothing.
    den.checks.attachment-addresses = {
      description = "No two hosts claim one address on a carrier, and no host puts two carriers' addresses on one interface.";
      findings =
        let
          claims = lib.concatLists (
            lib.mapAttrsToList (
              _: h:
              lib.concatLists (
                lib.mapAttrsToList (
                  carrier: a:
                  lib.optional (a.address != null) {
                    host = h.name or "?";
                    inherit carrier;
                    inherit (a) address interface;
                  }
                ) (h.attachments or { })
              )
            ) checkLib.hostsOf
          );

          duplicateAddress = lib.concatLists (
            lib.mapAttrsToList (
              key: cs:
              lib.optional (lib.length cs > 1) (
                "${key} is claimed by ${
                  lib.concatMapStringsSep " and " (c: "${c.host}'s attachments.${c.carrier}") cs
                } -- an attachment is where other hosts dial that one, so both resolve to whichever box answers first."
              )
            ) (lib.groupBy (c: "${c.carrier} address ${c.address}") claims)
          );

          # One interface holding two carriers' addresses is NOT wrong by itself
          # -- a bridge legitimately carries both -- so this reports the pair
          # and leaves the judgement, rather than claiming a bug.
          sharedInterface = lib.concatLists (
            lib.mapAttrsToList
              (
                key: cs:
                lib.optional (lib.length (lib.unique (map (c: c.address) cs)) > 1) (
                  "${key} carries ${
                    lib.concatMapStringsSep " and " (c: "${c.address} (attachments.${c.carrier})") cs
                  } -- ../base/firewall.nix opens both carriers' exposed ports on that one interface, so a port meant for one wire is open on the other."
                )
              )
              (
                lib.groupBy (c: "${c.host} interface ${toString c.interface}") (
                  lib.filter (c: c.interface != null) claims
                )
              )
          );
        in
        duplicateAddress ++ sharedInterface;
      fix = "Give each attachment its own address, or drop the one that is not real -- ./fleet.nix's `attachments` description says an entry is declared only where the host is genuinely addressable over that carrier.";
    };

    den.schema.host = { options, ... }: {
      # ON THE HOST SCHEMA, not `den.default.includes`: ../base/firewall.nix
      # records the measurement -- `den.default` lands in every scope, host and
      # user alike, and an assertion or warning emitted from a user scope is not
      # the host's. Declared from inside this module rather than as a second
      # `den.schema.host` definition because this file already has one and two
      # in one attrset is "already defined"; `optionalAttrs` because den also
      # introspects this schema standalone, where `includes` is not declared --
      # the same guard ../security/age-scope.nix uses, for the same reason.
      config = lib.optionalAttrs (options ? includes) {
        includes = [ den.aspects.attachment-interface-check ];
      };

      options = {
        # ── THE MACHINE THIS ENTITY RUNS ON, WHERE IT IS NOT ITS OWN ──
        #
        # A container is an entity (celler2 is one: class `nixos`, its own
        # attachment), so "the same entity" and "the same box" have come apart.
        # `parent` names the host whose machine this one shares. Null -- every
        # host today -- means it is its own machine.
        #
        # Read by ../base/capability.nix's `machineOf`, which is what lets a
        # `spans = "one-machine"` carrier join a container to the host it runs
        # on without letting it claim to span machines.
        parent = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "secondpc";
          description = "The host whose machine this entity runs on, for a container. Null for a machine of its own.";
        };

        builder = mkOption {
          type = record;
          default = null;
          description = ''
            Present iff the host is a remote build machine: `hostName` or
            `ipAddress`, `publicHostKey` (base64), `systems`, `maxJobs`,
            `speedFactor`, `features`, `sshUser`. ../base/builders.nix gives
            every other host a `nix.buildMachines` entry for it.
          '';
        };
        device = mkOption {
          type = record;
          default = null;
          description = "Present iff the host is a tailscale client with nothing built for it (`description`); ../network/tailscale.nix enrols these.";
        };
        # NO `publicZone` HERE. The zone briefly lived on this entity, beside
        # `lanAddress`, on the reasoning that both are facts nothing can derive
        # and a fleet-wide reader must reach. That is true of an ADDRESS and
        # false of a zone: a zone is a carrier, so a host publishes under it by
        # INCLUDING the aspect that carries it -- ../network/cloudflare.nix's
        # `publish-ivymect-in` -- and membership is then inclusion rather than a
        # field that can disagree with it. Same reasoning as `lan-home`.
        lanAddress = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            The host's fixed address on the home LAN, for the traffic that has
            no business going through the tunnel -- `builder.ipAddress` is a
            wireguard address, so it is the wrong thing to reach a box on the
            same switch with. Null for anything that only has a tunnel
            address or is dialled by name.
          '';
        };
        # ── HOW THIS HOST IS ATTACHED TO A CARRIER ──
        #
        # "we say that secondpc is connected to home-lan with the ip address
        # 192.168.0.26 using interface br0, not on the host itself".
        #
        # Keyed by CARRIER name, because both fields are facts about the
        # ATTACHMENT and not about either end alone: the address is where
        # other hosts dial this one over that carrier, the interface is where
        # this one hears them, and a host on two segments has two of each.
        # `lanAddress` above is this fact with the carrier left implicit --
        # ../network/segments.nix's header already calls it "awkward on the
        # host record" -- so it is derived from here now rather than declared
        # beside it.
        #
        # ── WHY IT IS ON THE ENTITY AND NOT IN THE HOST'S CONFIG ──
        #
        # `networking.interfaces.<n>` and `networking.bridges` say the same
        # thing, and reading them back would mean EVALUATING THE HOST to find
        # out which wire a port should be opened on -- several GB for a fact
        # that is one string. On the entity, ../base/firewall.nix and every
        # other consumer answer it from declared data in seconds.
        attachments = mkOption {
          type = types.attrsOf (
            types.submodule {
              options = {
                address = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  description = "Where other hosts dial this one over that carrier.";
                };

                interface = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  example = "br0";
                  description = ''
                    The kernel interface this host hears that carrier on.
                    `br0` and not the NIC where a host bridges several --
                    secondpc bridges its ethernet and its access point, so
                    that is the wire anything on the LAN arrives over.
                  '';
                };

              };
            }
          );
          default = { };
          example = lib.literalExpression ''{ lan-home = { address = "192.168.0.26"; interface = "br0"; }; }'';
          description = ''
            This host's attachment to each carrier, keyed by the carrier's
            aspect name.

            DECLARED ONLY WHERE THE HOST IS ADDRESSABLE. secondpc is on the
            house lan at a fixed address over a bridge, always, and that is a
            fact about its configuration. A LAPTOP HAS NO ENTRY HERE, and
            should not get one: it has no address anything can dial, and
            whether it is on that wire at all is a fact about where somebody
            carried it. Declaring an attachment for it would be claiming a
            link that is sometimes a lie -- and `dial` bakes one answer into
            config (a substituter, an upstream, a VBAN destination), so the
            lie would be written into the closure.

            A roaming host reaching the lan is therefore not modelled here at
            all. It is the run-time half: a consumer reads the PEER's declared
            address, probes it, and confirms the box answering is really that
            peer. Eval supplies the table; the probe supplies the truth.
          '';
        };

        # Read instead of `hostPublicKey` itself, whose `apply` warns when it
        # is unset: walking the fleet would warn once per keyless host.
        #
        # `definitions`, not `isDefined`: the option carries a `null` default
        # and den's host submodule reports `isDefined` for every host whether
        # or not a key was written, so the old form was constantly true and
        # put `hostPublicKey = null` on all twelve fleet entries. Consumers
        # testing `meta ? hostPublicKey` then saw a key where there was none
        # -- which is how seven keyless members reached nixpkgs'
        # `programs.ssh.knownHosts` assertion. A raw definition has not been
        # through `apply`, so reading them stays warning-free.
        #
        # `or [ ]`, because `definitions` is only there on an EVALUATED
        # option. den also introspects this schema standalone -- the same
        # thing ../security/age-scope.nix guards against with
        # `options ? includes` -- and there `options.hostPublicKey` is the
        # raw `mkOption` declaration, which has neither. Nothing real reads
        # the fleet during that pass, so `false` is the right answer for it.
        hasHostPublicKey = mkOption {
          type = types.bool;
          readOnly = true;
          internal = true;
          default = lib.any (d: d != null) (options.hostPublicKey.definitions or [ ]);
        };
      };
    };

    _module.args.fleet =
      lib.mapAttrs fromHost (lib.concatMapAttrs (_: hosts: hosts) den.hosts) // config.fleet.external;

    # Not one of ours: an external NixOS box we only ever dial. It is here
    # because `builder` is what ../base/builders.nix walks, and a build machine
    # every host can reach over plain DNS (rather than only from inside the
    # wireguard tunnel) is exactly what that list is for. No `system`/`uid`,
    # since nothing builds or forwards an agent to it -- and no wireguard
    # keypair, which keeps it out of the tunnel peer list and the ssh aliases.
    fleet.external."faggot.sh" = {
      # An account with no home here, linked to its person like any other.
      users.ivy.person = den.persons.ivy;
      aspects = [
        den.aspects.openssh
        den.aspects.tailscale
      ];
      # ed25519, from `ssh-keyscan faggot.sh`. The box also still offers the
      # ssh-rsa key that is already in ~/.ssh/known_hosts; this is the modern one.
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMWQGIJW7xP4ayNXSF0FOMdvpoPb1abvBxftF6L8ajn3";
      device = null;
      # ── IT IS ON THE TAILNET, SAID AS AN ATTACHMENT ──
      #
      # An external carries `attachments` like any other fleet member: the
      # option is `attrsOf anything`, so this is the same key a den host
      # declares, and ../base/builders.nix reads it. Saying it is attached is
      # what makes the tailnet route a derived fact rather than a second
      # hostname pasted into the builder record.
      #
      # THE ADDRESS IS STATED HERE and the name is not, which is the opposite
      # of a den host: the tailnet addresses its members by MagicDNS name
      # derived from the ENTITY name (../network/tailscale.nix), and this
      # entity is keyed `faggot.sh` while its tailnet name is `myhost`. So the
      # name cannot be composed from the key, and the one fact that is stable
      # and knowable is the address. Measured: `myhost.tailef3db9.ts.net` and
      # 100.82.210.67 both resolve and answer.
      attachments.tailscale.address = "100.82.210.67";

      builder = {
        # ── STILL THE PUBLIC NAME FOR SSH, AND THAT IS NOT A PREFERENCE ──
        #
        # The tailnet route reaches this box but CANNOT ssh to it: the tailnet
        # policy refuses it ("tailnet policy does not permit you to ssh to this
        # node"), which is Tailscale SSH intercepting port 22 and declining.
        # So `nc -z 100.82.210.67 22` answers OPEN while ssh does not work --
        # the connect is to tailscaled, not to sshd.
        #
        # Until that policy grants ssh, builds go over the public name. The
        # hazard there is this box's fail2ban: one failed publickey from this
        # address lost port 22 for a while, and it reads as `No route to host`
        # on 22 while 80 and 443 stay open and ICMP answers 2/2.
        hostName = "faggot.sh";
        # NO `publicHostKey`: ../base/builders.nix derives it from
        # `hostPublicKey` above, in the base64 `nix.buildMachines` wants. It was
        # the same key a second time in a second encoding.
        systems = [ "x86_64-linux" ];
        # 12 cores, but over a tunnel across the internet rather than a LAN, so
        # it is deliberately rated below auspc (20) and above secondpc (5).
        maxJobs = 6;
        speedFactor = 8;
        # No /dev/kvm on the box, so no `kvm` and no `nixos-test`.
        features = [ "big-parallel" ];
        sshUser = "ivy";

        # ── ITS CREDENTIAL HAS A SOURCE, BECAUSE WE CANNOT MINT IT ──
        # Every other builder's key is minted per edge (../base/builders.nix).
        # This box's `authorized_keys` is hand-filled on a machine this tree
        # does not configure, so the private half has to be a value that
        # already matches it -- a `rekeyFile` and no generator, which is what
        # `den.capabilities.credential.rekeyFile` is for.
        rekeyFile = "secrets/static/fargonekey.age";
      };
    };
  };
}
