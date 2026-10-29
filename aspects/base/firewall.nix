{
  den,
  lib,
  checkLib,
  routeLib,
  ...
}:
# ── WHY THIS IS NOT A SECOND PLACE PORTS LIVE ──
# FIREWALL RULES DERIVED FROM WHAT SERVICES OFFER.
# Every service in this tree binds loopback-only on purpose, so an open port is
let
  # THE REAL ENTITY, NOT THE ONE THE CLASS BODY IS HANDED.
  #
  # This is why every `expose` in the tree was inert. A class body's `host`
  # carries den's PROJECTED `hasAspect`, which answers "is this delivered into
  # THIS scope" rather than "does that host have it" -- so
  # `den.lib.capability.providersFor host "tcp"` came back EMPTY here and the derived
  # firewall opened nothing, for as long as it has existed. Every port that
  # looked derived was in fact a literal somewhere else, which is exactly why
  # nobody noticed.
  #
  # ../services/shares.nix hit the same trap and recorded it ("`takersOn host
  # "share"` comes back EMPTY and every share silently vanishes"); it answered
  # by asking each account instead. The question here is about a HOST, so the
  # fix is to ask the host entity rather than its projection.
  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;
  entityFor = host: hostsOf.${host.name or ""} or host;
in
{
  den.default.includes = [ den.aspects.derived-firewall ];

  den.aspects.derived-firewall = {
    description = "Opens exactly the ports services declare as exposed, on exactly the wires they name.";

    nixos =
      {
        config,
        pkgs,
        host ? { },
        ...
      }:
      let
        me = entityFor host;

        # ── WHICH WIRE A CARRIER IS, FROM THE CARRIER, ONCE ──
        #
        # Read off the LINK, never off the host: the interface sits beside the
        # address on what the carrier furnishes (./capability-types.nix),
        # because "secondpc is on the home lan at 192.168.0.26 over br0" is one
        # fact about one attachment. `tailscale0` is derived the same way --
        # the aspect furnishes it, since tailscaled always creates it.
        #
        # `carrierHalf` and NOT `linkFor`: the whole link is both halves typed
        # together, and asking a carrier for that demands endpoint fields it
        # has no business holding -- measured, as `The option
        # `capability.tcp.hostname' was accessed but has no value defined`.
        # The interface is a carrier-side fact, so the carrier side is what to
        # ask for.
        #
        # ONCE PER CARRIER, not once per port: resolving a link walks the
        # giver and types the result, and a host with five exposed ports on
        # one wire would otherwise do that five times for one answer.
        interfaces =
          cap:
          lib.listToAttrs (
            map (
              carrier:
              let
                aspect = den.aspects.${carrier} or null;
                half =
                  if aspect == null || !(den.lib.capability.has me aspect) then
                    null
                  else
                    den.lib.capability.carrierHalf aspect cap {
                      host = me;
                      peer = me;
                    };
              in
              lib.nameValuePair carrier (if half == null then null else half.interface or null)
            ) (exposedCarriers cap)
          );

        # The endpoint halves of everything on this host that gives `cap`,
        # resolved ONCE and read by both of the passes below.
        # ── THE HALF AND THE ASPECT THAT FURNISHED IT ──
        # Paired, because the exposure of a port is no longer a field ON the
        # port: `den.lib.capability.exposeFor` needs the giver to ask who resolves it,
        # and reading the half alone threw that away.
        givenHalves =
          cap:
          lib.filter (x: x.half != null) (
            # THE ENDPOINT HALF, not a whole link: a service furnishes a port
            # and no address, and typing it against the full set -- which
            lib.mapAttrsToList (_: aspect: {
              inherit aspect;
              half = den.lib.capability.endpointHalf aspect cap {
                host = me;
                peer = me;
              };
            }) (den.lib.capability.providersFor me cap)
          );

        halves = cap: map (x: x.half) (givenHalves cap);

        # EVERY (aspect, port name, port, exposure) this host gives for a
        # capability, with the exposure RESOLVED -- the stated list unioned with
        # the wires consumers actually reach it over. `den.lib.capability.exposeFor` is
        # the one answer, shared with the bind address, so the two cannot
        # disagree about which wires are in play.
        exposures =
          cap:
          lib.concatMap (
            x:
            lib.mapAttrsToList (pn: p: {
              inherit (x) aspect;
              inherit (p) port;
              carriers = den.lib.capability.exposeFor me x.aspect cap pn;
            }) x.half.ports
          ) (givenHalves cap);

        # ── DERIVED EXPOSURE ──
        #
        # An aspect that contributes config to a consumer says where the
        # consumer dials it (`listensOn` on the take, ../base/capability-types.nix)
        # and the hole follows: find the consumer's host, resolve the carrier
        # between here and there, open that port on that wire. One declaration
        # produces the integration and the hole, so removing the aspect removes
        # both.
        #
        # `expose` stays for consumers the graph cannot see -- the ESPHome nodes
        # on mosquitto's 1883 are not den hosts and never ask the resolver.
        listeners = lib.concatLists (
          lib.mapAttrsToList (
            k: _:
            let
              got = (den.lib.capability.contributions me k).listensOn or { };
            in
            lib.concatLists (
              # The ASPECT comes from `takersOn`, which returns references. `got`
              # is keyed by name only, so rebuilding it with `den.aspects.<name>`
              # would be a string lookup with an `or null` ready to swallow a miss.
              lib.mapAttrsToList (
                taker: aspect:
                let
                  spec = got.${taker} or null;
                in
                lib.optional (spec != null) {
                  inherit
                    k
                    taker
                    aspect
                    spec
                    ;
                }
              ) (den.lib.capability.takersOn me k)
            )
          ) den.capabilities
        );

        # Hosts that give the contribution capability, i.e. where the consumer is.
        consumersOf =
          k:
          lib.filter (h: (h.name or "") != (me.name or "") && (den.lib.capability.providersFor h k) != { }) (
            lib.attrValues hostsOf
          );

        # The carrier this host reaches `peer` over for `cap`, lowest preference first.
        # Ask the resolver how the CONSUMER reaches this endpoint rather than
        # re-deriving candidates: `linksBetween` admits any carrier both hosts
        # have by name, loopback included, and leans on `judge` downstream -- so
        # a hand-rolled version opened `lo` for a remote peer. Measured.
        carrierTo =
          cap: aspect: peer:
          let
            cands =
              if aspect == null then
                [ ]
              else
                den.lib.capability.reachEndpoint cap { } peer {
                  inherit aspect;
                  host = me;
                };
          in
          if cands == [ ] then null else (lib.head cands).link.carrier or null;

        # My interface for a carrier, asked the way `interfaces` asks.
        ifaceFor =
          cap: carrier:
          let
            aspect = den.aspects.${carrier} or null;
            half =
              if aspect == null || !(den.lib.capability.has me aspect) then
                null
              else
                den.lib.capability.carrierHalf aspect cap {
                  host = me;
                  peer = me;
                };
          in
          if half == null then null else half.interface or null;

        derived = lib.concatMap (
          l:
          let
            cap = l.spec.capability;
            hs = halves cap;
            ports = lib.concatMap (h: lib.attrValues (h.ports or { })) hs;
            named =
              if l.spec.port != null then
                lib.filter (p: p != null) (map (h: h.ports.${l.spec.port} or null) hs)
              else
                ports;
          in
          lib.concatMap (
            peer:
            let
              carrier = carrierTo cap l.aspect peer;
              iface = if carrier == null then null else ifaceFor cap carrier;
            in
            if iface == null then
              [ ]
            else
              map (p: {
                inherit (p) port;
                inherit iface;
                proto = cap;
              }) named
          ) (consumersOf l.k)
        ) listeners;

        # Every carrier any exposed port of this capability names, deduped --
        # the set `interfaces` has to answer for, and nothing wider.
        exposedCarriers = cap: lib.unique (lib.concatMap (e: e.carriers) (exposures cap));

        # Every (interface, port) this host should open for one capability.
        # PER PORT, not per link: each named port carries its own `expose`, so
        # Home Assistant's 8123 can sit behind nginx while its HomeKit bridge
        # is reachable from the LAN. A link-level flag could not say that.
        opened =
          cap:
          let
            ifaces = interfaces cap;
          in
          lib.concatMap (
            e:
            map (carrier: {
              inherit (e) port;
              inherit carrier;
              iface = ifaces.${carrier} or null;
            }) e.carriers
          ) (exposures cap);

        # `{ <iface> = [ ports ]; }`, dropping the ones that did not resolve --
        # those are reported by the assertion below rather than silently
        # opened somewhere wrong.
        byInterface =
          cap:
          lib.mapAttrs (_: es: lib.unique (map (e: e.port) es)) (
            lib.groupBy (e: e.iface) (lib.filter (e: e.iface != null) (opened cap))
          );

        # Declared plus derived, per protocol.
        withDerived =
          cap:
          let
            extra = lib.filter (e: e.proto == cap) derived;
          in
          lib.foldl' (
            acc: e: acc // { ${e.iface} = lib.unique ((acc.${e.iface} or [ ]) ++ [ e.port ]); }
          ) (byInterface cap) extra;

        tcp = withDerived "tcp";
        udp = withDerived "udp";

        # A port naming a carrier this host has no attachment to. LOUD,
        # because the alternative is the failure this whole file just came
        # back from: a rule that was never written and a service that looks
        # exposed.
        unresolved = lib.concatMap (cap: lib.filter (e: e.iface == null) (opened cap)) [
          "tcp"
          "udp"
        ];
      in
      {
        # PER INTERFACE, not the global list. nixpkgs folds
        # `allowedTCPPorts` into a pseudo-interface called `default` and emits
        # its rules with no `-i` at all, so the global list is "every wire this
        # host has" -- which is what made `expose` unable to distinguish the
        # tailnet from the LAN.
        #
        # LIST OPTIONS, so these merge with every hand-written opening rather
        # than replacing them.
        networking.firewall.interfaces = lib.mkMerge [
          (lib.mapAttrs (_: ports: { allowedTCPPorts = ports; }) tcp)
          (lib.mapAttrs (_: ports: { allowedUDPPorts = ports; }) udp)
        ];

        # ── AND WHERE TRAFFIC LEAVES, FROM THE SAME RESOLUTION ──
        # The resolved route takes (../network/routes.nix): marks, rules,
        # source NAT and the guard. In the firewall's own hook, so a reload
        # re-applies them with the holes above.
        networking.firewall.extraCommands = (routeLib.planFor { inherit config pkgs host; }).netfilter;

        assertions = map (e: {
          assertion = false;
          message = "firewall: a service on ${me.name or "this host"} exposes port ${toString e.port} over a carrier it has no attachment to, so there is no interface to open it on. Either attach the host to that carrier or drop it from the port's `expose`.";
        }) unresolved;

        # ── AND A PORT OPENED ON EVERY WIRE AT ONCE ──
        #
        # `networking.firewall.allowedTCPPorts` is nixpkgs' pseudo-interface
        # `default`: its rules are emitted with no `-i` at all, so a literal
        # there opens the port on the LAN, the tailnet, the access point and
        # anything else this box is on. That is the failure Matter's UDP 5540
        # was -- a global literal for a port that belonged on `br0` alone.
        #
        # A WARNING AND NOT AN ASSERTION, because the global list is also where
        # every genuinely-undeclared opening still lives (this host's own
        # `allowedTCPPorts`, nixpkgs modules' `openFirewall`), and turning
        # those into build failures would break every machine before anything
        # could be migrated. It names the ports so the list shrinks.
        #
        # ONE WARNING PER HOST rather than per port: the action is the same for
        # all of them, and fourteen lines of it on every build is noise that
        # gets ignored.
        warnings =
          let
            globalTcp = config.networking.firewall.allowedTCPPorts;
            globalUdp = config.networking.firewall.allowedUDPPorts;
            shown = proto: ps: lib.optional (ps != [ ]) "${proto} ${lib.concatMapStringsSep "," toString ps}";
            both = shown "tcp" globalTcp ++ shown "udp" globalUdp;
          in
          lib.optional (both != [ ]) ''
            firewall: ${me.name or "this host"} opens ${lib.concatStringsSep " and " both} on EVERY wire.
              `networking.firewall.allowedTCPPorts`/`allowedUDPPorts` is nixpkgs'
              pseudo-interface `default` -- no `-i`, so these are reachable from the
              lan, the tunnel and the access point alike, whatever the service meant.
              Declare them instead as `gives.<cap>.ports.<n>.expose = [ "<carrier>" ]`
              on the aspect that serves them; this file then opens each one on exactly
              that carrier's interface.
          '';
      };
  };

  # ── THE SAME SOCKET WALK, FOR THE WHOLE FLEET, WITHOUT BUILDING ONE HOST ──
  #
  # `derived-port-exclusion` below is per host and fires when that host is
  # built, which is the latest moment it still helps. This asks the same
  # question of every machine at once off entities and givers, so `nix flake
  # check` reports a clash on secondpc without evaluating secondpc -- and a
  # FLEET- or HOUSEHOLD-scoped rule (a published name per zone) is only
  # answerable when every claimant is in view anyway.
  #
  # `lib.unique` because a fleet-scoped rule finds the same pair from both ends:
  # the message names both claimants, so asking eleven hosts yields eleven
  # copies of one line.
  den.checks.socket-exclusions = {
    description = "No two givers claim one exclusive resource -- a socket per host, a published name per zone.";
    findings = lib.unique (
      lib.concatMap (
        h:
        checkLib.probe (h.name or "?") "den.lib.capability.exclusionsIn ${h.name or "?"}" (
          den.lib.capability.exclusionsIn h
        )
      ) checkLib.hostEntities
    );
    fix = "Two givers furnish the same port on one host, or the same subdomain in one zone. Move one of them, or give the second its own instance -- ./capability-types.nix declares what each capability holds exclusively and how far.";
  };

  # ── AND WHETHER TWO OF THEM ARE THE SAME SOCKET ──
  # The rules are NOT here: ../base/capability-types.nix declares what each
  # capability holds exclusively and how far (a socket per host, a published
  # name per zone), and this is only the walk that enforces them. So a new
  # exclusive resource needs no change to this file.
  #
  # ── ON `den.schema.host.includes`, NOT `den.default.includes` ──
  # Measured, after an assertion from a `den.default` aspect silently failed to
  # reach `config.assertions` while `networking.firewall` from the same aspect
  # arrived: `den.default` lands in EVERY scope, host and user alike, and an
  # assertion emitted from a user scope is not the host's.
  #
  # Which is the right home anyway. "Is this socket claimed twice on this
  # machine" is a question about a host, and `den.schema.host.includes` is what
  # lands on hosts only -- the same reasoning ./caches.nix records for moving
  # the public substituters off `den.default`.
  den.schema.host.includes = [ den.aspects.derived-port-exclusion ];

  den.aspects.derived-port-exclusion = {
    description = "Refuses two givers claiming one exclusive resource.";

    # AN ASSERTION, NOT A THROW, so every clash on a host is reported together
    # with everything else wrong with it rather than the first one hiding the
    # rest. The alternative is EADDRINUSE at boot, on the machine, which is the
    # latest and least useful place to learn this.
    nixos =
      {
        host ? { },
        ...
      }:
      {
        assertions = map (m: {
          assertion = false;
          message = m;
        }) (den.lib.capability.exclusionsIn host);
      };
  };
}
