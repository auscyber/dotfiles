{
  den,
  lib,
  ...
}:
# ── THE ROAMING LOCAL RESOLVER ──
#
# "the laptop will run an unbound resolver locally so that it can resolve those
# in priority so that services use that to find things" -- and the sentence that
# makes it work:
#
#   "for tls, on the laptop if we want sso.ivymect.in and we are connected
#    locally we should have sso.ivymect.in resolve to secondpc-house which then
#    resolves to 192.168.0.26 -- as the name sso.ivymect.in matches what the
#    certificate provides, its fine, its just using a different route to get
#    there"
#
# So the PUBLISHED NAME IS KEPT and only the ADDRESS IT RESOLVES TO changes with
# where this host is. Nothing downstream is reconfigured, no certificate moves,
# no vhost learns a second name. ../../docs/roaming-resolver.md carries the whole
# argument and what was ruled out; what follows is what a reader of this file
# needs.
#
# ── WHY THIS IS A CONSUMER AND NOT A CARRIER ──
#
# The obvious shape -- give the laptop `lan-home` and let ../base/capability.nix
# resolve the wire -- is WRONG, and measurably so. `linksBetween` ranks
# `lan-home` at preference 20 against the tailnet's 40, so including it would
# move every EVAL-TIME answer onto the lan: substituters, netrc, the push hook,
# ssh config, the time-machine destination. Those are written into the built
# system, and a roaming host is off that wire for part of the week. The route is
# not knowable at evaluation, which is precisely why it must not be ranked at
# evaluation.
#
# So this aspect never joins a segment. It asks a DIFFERENT question -- "if I
# were on that wire, where would that peer be" -- which `den.lib.capability.linkFor`
# answers for one NAMED carrier without any membership rule applying. Measured:
#
#   den.lib.capability.linkFor den.aspects.lan-home "tcp"
#     { host = <laptop>; peer = <secondpc>; }
#   => { address = "192.168.0.26"; interface = null; mtu = 1500; }
#
# while `linksBetween "tcp" <laptop> <secondpc>` stays
# `[ tailscale, publish-ivymect-in ]`. The consumer-side view already exists and
# costs the resolution layer nothing -- which is why no `attachment =
# "consumer"` is added here; see the doc for that argument in full.
#
# ── AND WHY THERE IS NO `intermittent` FLAG ──
#
# "intermittent makes less sense, its more of intermittent *contains* a lan-home
# link, as it would require checking if false for users of it". Right: nothing
# here is a boolean any consumer has to test. `roams` names the wires this host
# MIGHT be on; the eval-time product is the whole SHAPE of the answer (which
# names, which address, grouped per peer) and a run-time probe picks one. That is
# ../base/capability.nix's own split -- "eval time knows the SHAPE of the
# decision and run time knows what is up" -- with the list of possibilities being
# exactly the ordered candidate set `linksBetween` already returns.
let
  inherit (lib) mkOption types;

  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;

  # A walk over every host's takers reaches aspects whose `gives`/`takes` can
  # throw for reasons that have nothing to do with this question, and one bad
  # clause must not stop the resolver answering for the rest of the fleet.
  # ../base/service-reach.nix carries the same helper for the same reason.
  try =
    v:
    let
      r = builtins.tryEval v;
    in
    if r.success then r.value else null;

  aspectsOn = host: den.lib.capability.heldBy host;

  # ── THE WIRES THIS HOST MIGHT BE ON ──
  # Unioned across every aspect it has, the way `includes` is, so a role or a
  # household could say "machines like this roam onto the house lan" without
  # each host repeating it.
  roamsOn = host: lib.unique (lib.concatMap (a: a.roams or [ ]) (lib.attrValues (aspectsOn host)));

  # ── WHERE A PEER PUBLISHES, which is the zone carrier's fact ──
  # The same filter ../base/capability.nix's `reachEndpoint` uses to find a
  # published name: a zone attaches provider-side and carries `domain`.
  zoneOn =
    peer:
    let
      zs = lib.attrValues (
        lib.filterAttrs (_: a: (a.domain or null) != null && a.attachment == "provider") (aspectsOn peer)
      );
    in
    if zs == [ ] then null else (lib.head zs).domain;

  # ── THE NAMES A PEER SERVES, DERIVED ──
  #
  # `contributions peer "proxy"` is the taker side of ../base/capability-types
  # .nix's `proxy`: every service that asked to be published, with the subdomain
  # it asked for. Measured on secondpc: 25 names, which is the vhost set bar the
  # two a raw server block serves.
  #
  # `defaultTo` AND NOT `or`: the taker option has `default = null`, so the
  # attribute EXISTS and `or` never fires -- ../../CLAUDE.md's seven-bug trap. A
  # null subdomain means the service did not override it, and the gateway
  # publishes it under the client's own name.
  publishedOn =
    peer:
    let
      zone = zoneOn peer;
      subs = try ((den.lib.capability.contributions peer "proxy").subdomain or { });

      # ── A SERVICE UNDER A SUBPATH HAS NO NAME OF ITS OWN ──
      # `subpath` is `proxy`'s own vocabulary for "several services under one
      # name" -- `arr.<zone>/tv` and friends -- so a taker that set one is
      # published under SOMEBODY ELSE'S subdomain and contributes nothing here.
      #
      # Measured, and it is exactly the error this filter exists to stop: the
      # fallback below produced `sonarr.ivymect.in`, `radarr`, `bazarr`,
      # `lidarr` and `prowlarr`, none of which is a vhost on secondpc, while
      # `contributions secondpc "proxy"` gives those five -- and only those
      # five -- a non-"/" subpath.
      #
      # `!= "/"` and not `or`: `contributions` is the TYPED fold, so the
      # option's `default = "/"` has materialised and the attribute exists.
      paths =
        if subs == null then { } else (den.lib.capability.contributions peer "proxy").subpath or { };

      fromProxy =
        if subs == null then
          [ ]
        else
          lib.mapAttrsToList (taker: sub: "${lib.defaultTo taker sub}.${zone}") (
            lib.filterAttrs (taker: _: (paths.${taker} or "/") == "/") subs
          );

      # A GIVER MAY FURNISH ITS OWN SUBDOMAIN instead of taking one -- celler
      # does, which is how `cache.ivymect.in` is derived rather than written.
      fromGives = lib.concatMap (
        a:
        let
          h = try (
            den.lib.capability.endpointHalf a "http" {
              host = peer;
              peer = peer;
            }
          );
          sub = if h == null then null else try (h.subdomain or null);
        in
        lib.optional (sub != null) "${sub}.${zone}"
      ) (lib.attrValues (lib.filterAttrs (_: a: (a.gives or null) != null) (aspectsOn peer)));

      # AND THE THIRD SOURCE, which is not a hand-written zone: a service that
      # renders its own nginx server block records no `proxy` take at all
      # (../services/proxy-nginx.nix's `emits` is false without a port or an
      # upstream), so the name it publishes under cannot be folded out of
      # `contributions`. `alsoPublishes` is a SUBDOMAIN declared beside the vhost
      # that serves it, with the zone still coming from the carrier.
      raw = lib.concatMap (a: map (s: "${s}.${zone}") (a.alsoPublishes or [ ])) (
        lib.attrValues (aspectsOn peer)
      );
    in
    if zone == null then [ ] else lib.sort (x: y: x < y) (lib.unique (fromProxy ++ fromGives ++ raw));

  # ── ONE GROUP PER (WIRE, PEER), WHICH IS THE HINGE ──
  #
  # "we give as an address `secondpc-house` or something like that, which may be
  # tsnet or may be 192.168.0.26". The hinge is real and it is this group: ONE
  # address, N published names, one probe, one flip.
  #
  # It is deliberately NOT a CNAME to a switchable target, and that is a finding
  # rather than a simplification. A CNAME whose target the resolver holds no
  # data for returns the alias and no address, so the client never falls through
  # to the public answer -- which is the case that has to keep working. The group
  # has to be removable WHOLE for the public route to come back, so the flip is a
  # transaction over N names rather than one record N names point at.
  #
  # `<peer>.roam.internal` is served alongside, because the name asked for is
  # worth having for its own sake: `dig @127.0.0.1 -p 5335 secondpc.roam
  # .internal` says in one line which wire this box thinks it is on. `.internal`
  # because ICANN reserved it for exactly this, so it cannot collide with a real
  # delegation.
  groupsFor =
    host:
    lib.concatMap (
      carrier:
      lib.concatMap (
        peer:
        let
          l = try (
            den.lib.capability.linkFor carrier "tcp" {
              inherit host peer;
            }
          );
          address = if l == null then null else l.address or null;
          names = publishedOn peer;
        in
        # NO ADDRESS, NO GROUP. A peer on this wire with no address on it is the
        # roaming host's own situation seen from the other side (measured:
        # `linkFor lan-home "tcp" { host = secondpc; peer = laptop; }` has a null
        # address), and NO NAMES is a peer that publishes nothing -- auspc is on
        # the house lan and serves no zone. Both are honest absences rather than
        # entries that would resolve to nothing.
        lib.optional (address != null && names != [ ]) {
          carrier = carrier.name;
          peer = peer.name;
          inherit address names;
          hinge = "${lib.toLower peer.name}.roam.internal";

          # ── THE CARRIER'S OWN NAMES FOR THE PEER ──
          # The one lever that moves an EVAL-TIME answer without rebuilding: the
          # laptop's substituter is `http://secondpc.tailef3db9.ts.net:8069/main`
          # (measured), not the published `cache.ivymect.in`, so pinning the
          # PUBLISHED name cannot move nix onto the lan and pinning the TAILNET
          # name can. Off unless the host includes
          # `den.aspects.roaming-resolver-carrier-names`; see that aspect.
          #
          # ── A LOOPBACK IS NOT A SEGMENT, AND HERE THAT IS A LOADED GUN ──
          # `providersFor host "hostname"` includes `loopback`, which furnishes
          # `localhost` for any peer -- so the first version of this put
          # `localhost` in the pin set, and turning the aspect on would have
          # pointed `localhost` at 192.168.0.26. Measured, in the plan output.
          #
          # The filter is ../base/capability.nix's own: a name only counts when
          # the giver is a wire the peer SHARES with this host, which is what
          # `spans = "many-hosts"` declares (../base/capability-types.nix). A
          # loopback is a segment too and spans one box, so it is filtered out
          # here by its span rather than by giving nothing -- the same pair of
          # conditions that make the ADDRESS this group pins meaningful.
          carrierNames =
            let
              segs = lib.filterAttrs (_: a: a.spans == "many-hosts") (
                den.lib.capability.providersFor peer "segment"
              );
            in
            lib.unique (
              lib.concatMap
                (
                  a:
                  let
                    n = try (
                      den.lib.capability.linkFor a "hostname" {
                        inherit host peer;
                      }
                    );
                  in
                  if n == null then [ ] else [ n.hostname ] ++ (n.aliases or [ ])
                )
                (
                  lib.attrValues (
                    lib.filterAttrs (n: _: segs ? ${n}) (den.lib.capability.providersFor host "hostname")
                  )
                )
            );
        }
      ) (lib.filter (p: p.name != host.name && den.lib.capability.has p carrier) (lib.attrValues hostsOf))
    ) (roamsOn host);

  # ── RESOLVED OFF THE REAL ENTITY, NEVER THE PROJECTED ONE ──
  # A class body is handed a PROJECTED host whose `hasAspect` comes back empty,
  # which is the trap ../base/firewall.nix and ../services/shares.nix were both
  # bitten by. `name` survives projection, so the body looks the entity up here
  # and every `hasAspect` above runs against the real one.
  planFor = name: if hostsOf ? ${name} then groupsFor hostsOf.${name} else [ ];
  entityFor = name: hostsOf.${name} or null;
in
{
  # den's `fx/key-classification.nix` dispatches an unrecognised key as class
  # content. Both of these are declared on the schema below, which is what makes
  # a misspelling an error rather than an aspect that silently says nothing --
  # the reason ../base/capability-types.nix reserves `gives` and `takes`.
  config.den.reservedKeys = [
    "roams"
    "alsoPublishes"
  ];

  config.den.schema.aspect.options = {
    roams = mkOption {
      type = types.listOf types.raw;
      default = [ ];
      description = ''
        Carriers this host may be attached to SOMETIMES, as aspects -- the same
        vocabulary `includes` uses, and deliberately not `includes` itself.

        Including a segment makes ../base/capability.nix rank it at evaluation,
        which is wrong for a wire a laptop is on three days a week: every
        substituter, netrc and ssh entry in the built system would name an
        address that is unreachable from a cafe.

        `roams` says the weaker thing -- the route EXISTS when this host happens
        to be there -- and only ./roaming-resolver.nix reads it.
      '';
    };

    alsoPublishes = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [ "auth" ];
      description = ''
        Subdomains this aspect publishes through a server block it renders
        itself, rather than through a `takes.proxy`. SUBDOMAINS, not names: the
        zone comes from the host's zone carrier, so a service that moves hosts
        is not edited.

        It exists because ../services/proxy-nginx.nix's `emits` is false for a
        taker with no port and no upstream, so a raw vhost's name cannot be
        folded out of `den.lib.capability.contributions`. Declared beside the vhost it
        describes, which is the only place it cannot go stale.
      '';
    };
  };

  # ── MEMBERSHIP, NOT A FLAG ──
  # Including this is how a host opts into pinning the carrier's OWN names for a
  # peer as well as the published ones -- `secondpc.tailef3db9.ts.net` beside
  # `cache.ivymect.in`.
  #
  # It is the only thing here that can move an answer the capability layer
  # already resolved, and that is both why it is wanted and why it is separate:
  # the laptop's substituters, its ssh config and its time-machine destination
  # all name the tailnet, so this pin takes all three onto the lan at home with
  # no rebuild -- and takes all three down if the premise is wrong.
  #
  # NOT DEFAULT, because one link in the premise is unverified from here:
  # `/etc/resolver/tailef3db9.ts.net` is written by tailscaled (measured on this
  # laptop), and a more specific file beating it is darwin's documented
  # longest-suffix rule but has not been observed against a running MagicDNS.
  # Turn it on once that can be checked on the box.
  config.den.aspects.roaming-resolver-carrier-names.includes = [ den.aspects.roaming-resolver ];

  config.den.aspects.roaming-resolver = {
    # ── DARWIN ONLY, AND SAID OUT LOUD ──
    #
    # nix-darwin has no `services.unbound` -- its service set is `dnsmasq`,
    # `dnscrypt-proxy` and `nextdns` -- so the daemon below is written by hand,
    # which is fine: `pkgs.unbound` builds for aarch64-darwin (measured,
    # unbound-1.26.1, `meta.platforms` includes it, not broken).
    #
    # What darwin has and NixOS does not is the half that matters more.
    # `/etc/resolver/<name>` routes ONE NAME to a chosen resolver on a chosen
    # port, which is what keeps this a scalpel instead of taking a whole zone
    # hostage. Measured on this laptop: /etc/resolver holds 70 files, all written
    # by tailscaled, `tailef3db9.ts.net` among them -- so the mechanism is
    # confirmed live rather than assumed from a man page.
    #
    # systemd-resolved's split DNS is per-LINK and per-DOMAIN, never per-name, so
    # the Linux equivalent routes the entire zone and has to be argued
    # separately (the doc says what that costs). Until it is, a NixOS host
    # including this would run a resolver nothing queries, so it says so where
    # the module system prints it rather than coming up quietly inert.
    os = { pkgs, ... }: {
      assertions = [
        {
          assertion = pkgs.stdenv.hostPlatform.isDarwin;
          message = ''
            den.aspects.roaming-resolver is darwin-only: it needs per-NAME
            resolver scoping (`/etc/resolver/<name>`), and systemd-resolved
            routes per domain and per link instead. See
            docs/roaming-resolver.md for what the Linux half would take.
          '';
        }
      ];
    };

    darwin =
      {
        pkgs,
        lib,
        host ? { },
        ...
      }:
      let
        groups = planFor (host.name or "");
        me = entityFor (host.name or "");

        withCarrierNames =
          me != null && den.lib.capability.has me den.aspects.roaming-resolver-carrier-names;

        namesOf = g: g.names ++ lib.optionals withCarrierNames g.carrierNames;

        # ── WHAT PART OF THE ANSWER IS WRITTEN AT BUILD TIME ──
        # The port, and which names MAY be pinned. Nothing about which one wins.
        #
        # 5335 rather than 53 so nothing contends with mDNSResponder or with
        # MagicDNS's 100.100.100.100:53, and because the resolver files below
        # carry a `port` line regardless -- the same shape nix-darwin's own
        # dnsmasq module writes, which is where that line is known to work.
        port = 5335;
        ctl = "/var/run/roaming-resolver.ctl";

        allNames = lib.unique (lib.concatMap (g: namesOf g ++ [ g.hinge ]) groups);
        hinges = lib.unique (map (g: g.hinge) groups);

        unbound = pkgs.unbound;

        # ── THE HINGES NXDOMAIN WHEN UNPINNED; THE REST FALL THROUGH ──
        # A published name with no local data must reach the PUBLIC answer, so it
        # gets no `local-zone` at all until the probe adds one. `.roam.internal`
        # has no public answer and never will, so "not on that wire" is NXDOMAIN
        # rather than a lookup that leaves the box to fail.
        conf = pkgs.writeText "roaming-resolver.conf" ''
          server:
            verbosity: 1
            interface: 127.0.0.1
            port: ${toString port}
            do-ip4: yes
            do-ip6: no
            do-udp: yes
            do-tcp: yes
            access-control: 0.0.0.0/0 refuse
            access-control: 127.0.0.0/8 allow
            # A ROAMING ANSWER MUST NOT OUTLIVE THE WALK TO THE CAR.
            cache-min-ttl: 0
            cache-max-ttl: 30
            prefetch: no
            # No `unbound` user exists on darwin and there is no chroot to enter.
            username: ""
            chroot: ""
            directory: "/"
            pidfile: ""
            # launchd captures stderr; see StandardErrorPath below.
            logfile: ""
            use-syslog: no
            hide-identity: yes
            hide-version: yes
            # No validator: every name this answers is either local data or a
            # forward to a resolver that validates, and a trust anchor is state
            # this daemon has no business maintaining.
            module-config: "iterator"
          ${lib.concatMapStrings (h: ''
            local-zone: "${h}." static
          '') hinges}
          remote-control:
            control-enable: yes
            control-interface: ${ctl}
            control-use-cert: no

          forward-zone:
            name: "."
            forward-addr: 1.1.1.1
            forward-addr: 1.0.0.1
            forward-addr: 9.9.9.9
        '';

        # ── THE PROBE: THE ONLY THING THAT DECIDES ──
        #
        # Per group: does that address actually serve that name, right now. The
        # test is a TLS handshake against the PUBLISHED NAME with `--resolve`
        # forcing the candidate address, so it proves the three things a pin
        # asserts at once -- the address answers, the proxy matches the vhost,
        # and the certificate is the public one -- without asking the resolver it
        # is about to configure.
        #
        # That is also what stops the obvious false positive: a cafe lan where
        # 192.168.0.26 is somebody's printer answers TCP on 443 and fails the
        # handshake, so a connect test alone would pin the wrong box.
        #
        # NO `curl -f`: a gated vhost redirecting to the identity provider IS the
        # service answering, and `-f` would turn a login page into an unpinned
        # route. ../base/service-reach.nix states the same reasoning.
        #
        # FAIL-CLOSED. Anything other than a clean handshake removes the pin, so
        # the worst case of a broken probe is the behaviour of a machine without
        # this aspect at all.
        probe = pkgs.writeShellApplication {
          name = "roaming-resolver-probe";
          runtimeInputs = [
            unbound
            pkgs.curl
            pkgs.coreutils
          ];
          text = ''
            ctl() { unbound-control -c ${conf} "$@"; }

            # The control socket appears a moment after the daemon does, and
            # launchd starts both at load.
            for _ in $(seq 1 30); do
              if ctl status >/dev/null 2>&1; then break; fi
              sleep 1
            done
            if ! ctl status >/dev/null 2>&1; then
              echo "roaming-resolver: no control socket at ${ctl}; is com.ivy.roaming-resolver running?" >&2
              exit 1
            fi

            ${lib.concatMapStrings (
              g:
              let
                # The probe name is the first of the PUBLISHED names, in sorted
                # order, because only a published name has a certificate to
                # check. A carrier name has none and the hinge is not public.
                probeName = lib.head g.names;
                ns = namesOf g;
                # `static` and not `redirect`: an AAAA or HTTPS query for a
                # pinned name must come back NODATA rather than leaving the box,
                # or a client with a public v6 answer routes around the pin.
                zones = map (n: "${n}. static") ns;
                datas = map (n: "${n}. 30 IN A ${g.address}") ns ++ [
                  "${g.hinge}. 30 IN A ${g.address}"
                ];
              in
              ''
                if curl -sS --max-time 3 -o /dev/null \
                     --resolve ${lib.escapeShellArg "${probeName}:443:${g.address}"} \
                     ${lib.escapeShellArg "https://${probeName}/"}
                then
                  printf '%s\n' ${lib.escapeShellArgs zones} | ctl local_zones >/dev/null
                  printf '%s\n' ${lib.escapeShellArgs datas} | ctl local_datas >/dev/null
                  echo "roaming-resolver: ${g.peer} pinned to ${g.address} over ${g.carrier} (${toString (lib.length ns)} names)"
                else
                  # `|| true` ON THE UNPIN PATH ONLY. `writeShellApplication`
                  # sets `errexit`, and removing a zone that was never added is
                  # the NORMAL case -- every run off the wire does it. Letting
                  # that abort the script would stop the groups after this one
                  # from being unpinned, which is the half that has to work.
                  printf '%s\n' ${
                    lib.escapeShellArgs (map (n: "${n}.") ns)
                  } | ctl local_zones_remove >/dev/null || true
                  ctl local_data_remove ${lib.escapeShellArg "${g.hinge}."} >/dev/null || true
                  echo "roaming-resolver: ${g.peer} not reachable at ${g.address} over ${g.carrier}; public route in use"
                fi
              ''
            ) groups}
          '';
        };
      in
      lib.mkIf (groups != [ ]) {
        environment.systemPackages = [
          probe
          unbound
        ];

        # ── ONE FILE PER NAME, NOT ONE PER ZONE ──
        #
        # `/etc/resolver/ivymect.in` would route the WHOLE zone here, and the
        # zone is not only vhosts: `den.lib.capability.contributions secondpc "dns"`
        # holds the apex A and MX, `_dmarc`, `_imaps._tcp` and
        # `_submissions._tcp` SRVs, and a second set of the same under
        # `bitwarden.ivymect.in` (measured). A local view answering NODATA for
        # those would break mail on this laptop to fix a web route.
        #
        # Per name, the blast radius is the name: if this daemon is down, the
        # routed names fail and the rest of the zone -- and the rest of DNS --
        # never touches it.
        environment.etc = lib.listToAttrs (
          map (
            n:
            lib.nameValuePair "resolver/${n}" {
              enable = true;
              text = ''
                # aspects/network/roaming-resolver.nix
                port ${toString port}
                nameserver 127.0.0.1
              '';
            }
          ) allNames
        );

        launchd.daemons.roaming-resolver = {
          command = "${unbound}/bin/unbound -d -c ${conf}";
          serviceConfig = {
            Label = "com.ivy.roaming-resolver";
            RunAtLoad = true;
            KeepAlive = true;
            StandardErrorPath = "/var/log/roaming-resolver.log";
            StandardOutPath = "/var/log/roaming-resolver.log";
          };
        };

        # ── WHEN TO ASK AGAIN ──
        #
        # `/etc/resolv.conf` is a symlink to `/var/run/resolv.conf` (measured on
        # this laptop) and configd rewrites it on every network change, so
        # watching it is the closest thing darwin has to a link-up event -- and
        # it fires at the moment the answer is most likely to be wrong.
        #
        # The interval is the backstop for the changes configd does not write:
        # the same SSID on a different AP, a cable in or out, the peer rebooting.
        launchd.daemons.roaming-resolver-probe = {
          command = lib.getExe probe;
          serviceConfig = {
            Label = "com.ivy.roaming-resolver-probe";
            RunAtLoad = true;
            StartInterval = 120;
            WatchPaths = [ "/var/run/resolv.conf" ];
            ThrottleInterval = 10;
            StandardErrorPath = "/var/log/roaming-resolver.log";
            StandardOutPath = "/var/log/roaming-resolver.log";
          };
        };
      };
  };

  # ── THE PLAN, WITHOUT A HOST EVAL ──
  #
  #   nix eval --impure --json .#debug.config \
  #     --apply 'cfg: cfg.den.roamingPlan.Ivys-MacBook-Pro'
  #
  # Which names this host would pin, to what, over which wire. Everything above
  # reads entities and givers only, so this answers in seconds -- ../../CLAUDE.md's
  # capability loop, and the same shape ../base/service-reach.nix exports as
  # `den.householdReach`.
  options.den.roamingPlan = mkOption {
    type = types.raw;
    readOnly = true;
    default = lib.filterAttrs (_: v: v != [ ]) (lib.mapAttrs (_: groupsFor) hostsOf);
    description = "Per host, the (wire, peer, address, names) groups its resolver may pin.";
  };
}
