{
  den,
  lib,
  caps,
  ...
}:
# ── ROUTES ARE WHAT A CARRIER GIVES A SEGMENT ──
#
# A segment is a destination with a prefix (./segments.nix, ./tailscale.nix,
# the bridges); `internet` is one more, `0.0.0.0/0`. A carrier that reaches a
# segment gives a `route` to it: which table, which device, whether it is
# private. A service that needs a particular way out TAKES a route, with
# clauses, and resolution picks the carrier exactly as it does for `tcp` -- so
# "qbittorrent leaves through PIA, and the house uplink lost because it is not
# private" is an edge in the capability graph rather than iptables in a file.
#
# What a host's kernel needs follows from the resolved takes: a rule per
# routed table, a mark per (table, fallback), the guard for takers that must
# not fall back, and the house's own segments exempted -- read off the
# segments this host has, not a list kept in step with them.
let
  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;
  entityOf = host: hostsOf.${host.name or ""} or host;

  # A RAW take, so `or`: a clause is absent unless stated.
  takeOf = aspect: aspect.takes.route or { };

  # Every route take on this host, resolved. The resolver is the whole of the
  # choice; nothing here ranks carriers.
  resolve =
    host: take:
    let
      clauses = lib.filterAttrs (_: v: v != null) (
        removeAttrs take [
          "fallback"
          "units"
          "user"
        ]
      );
      cands = den.lib.capability.resolveFrom host "route" (
        {
          name = null;
          carrier = null;
          within = null;
          sameHost = null;
          where = null;
        }
        // clauses
      );
    in
    lib.mapNullable (c: c.link // { carrier = c.via.name; }) (lib.findFirst (_: true) null cands);

  # ── WHERE A TAKER GOES WHEN ITS ROUTE IS DOWN ──
  # Each choice is its own mark and its own chain, so rendering looks it up
  # rather than branching on it.
  fallbacks = {
    # Nowhere: an `unreachable` rule behind the table, and the guard.
    none = {
      offset = 0;
      guarded = true;
    };
    # The host's own `main` table, which the rule falls through to.
    main = {
      offset = 1;
      guarded = false;
    };
  };
  fallbackOption = lib.mkOption {
    type = lib.types.enum (lib.attrNames fallbacks);
    default = "none";
    description = "Where traffic goes when the chosen route is down: `none`, or `main`, the host's own default.";
  };

  # ── OR OVER THE TAILNET, THROUGH ANOTHER HOST ──
  # When nothing here answers a take, the first tailnet peer (by name, so both
  # ends derive the same one) whose OWN answer is an advertised route does.
  # An advertised `0.0.0.0/0` is an exit node; anything else a subnet route.
  advertised = r: (lib.defaultTo { } r).advertise or false;
  byName = lib.sort (a: b: a.name < b.name) (lib.attrValues hostsOf);
  viaTailnet =
    host: take:
    let
      h = entityOf host;
      onTailnet = b: den.lib.capability.has b den.aspects.tailscale;
      answers = b: onTailnet h && onTailnet b && b.name != h.name && advertised (resolve b take);
    in
    lib.mapNullable (
      b:
      resolve b take
      // {
        carrier = "tailscale";
        via = b.name;
        table = null;
        mark = null;
        nameservers = [ ];
      }
    ) (lib.findFirst answers null byName);

  resolveAnywhere = host: take: lib.defaultTo (viaTailnet host take) (resolve host take);

  # ── WHAT THIS HOST DOES ON THE TAILNET, FROM EVERYONE'S TAKES ──
  # It advertises a prefix only because another host's take resolved to it
  # through the tailnet, and uses an exit node only because its own did.
  overTailnet =
    a:
    lib.filter (r: (lib.defaultTo { } r).carrier or null == "tailscale") (
      lib.mapAttrsToList (_: aspect: resolveAnywhere a (takeOf aspect)) (
        den.lib.capability.takersOn a "route"
      )
    );
  isExit = r: r.prefix == "0.0.0.0/0";
  tailnetPlan =
    host:
    let
      h = entityOf host;
      toMe = lib.filter (r: r.via == h.name) (lib.concatMap overTailnet byName);
      offered = lib.partition isExit (lib.unique (map (r: r.prefix) toMe));
    in
    {
      advertiseRoutes = offered.wrong;
      advertiseExit = offered.right != [ ];
      exitNode = lib.mapNullable (r: r.via) (lib.findFirst isExit null (overTailnet h));
    };

  # The take's clauses choose the route; its typed options (`fallback`,
  # `units`, `user`, with their defaults) are read through `contributions`.
  routedOn =
    host:
    let
      h = entityOf host;
      c = den.lib.capability.contributions h "route";
    in
    lib.mapAttrs (name: aspect: {
      route = resolveAnywhere h (takeOf aspect);
      fallback = c.fallback.${name};
      units = lib.defaultTo [ name ] c.units.${name};
      user = c.user.${name};
    }) (den.lib.capability.takersOn h "route");

  # The house's own wires on this host, which a routed taker still reaches
  # directly: every segment it is attached to that has a prefix.
  segmentsOn =
    host:
    let
      h = entityOf host;
    in
    lib.unique (
      lib.filter (p: p != null) (
        lib.mapAttrsToList (_: aspect: lib.defaultTo null (aspect.prefix or null)) (
          den.lib.capability.providersFor h "segment"
        )
      )
    );

  # ── THE PLAN: WHAT THE RESOLVED TAKES ASK OF THE KERNEL ──
  # Computed here, rendered by ../base/firewall.nix beside the ports it opens,
  # so every netfilter rule on a host comes from one derivation.
  planFor =
    {
      config,
      pkgs,
      host,
    }:
    let
      cfg = config.routing;
      ip = "${pkgs.iproute2}/bin/ip";
      ipt = "${pkgs.iptables}/bin/iptables";
      ipt6 = "${pkgs.iptables}/bin/ip6tables";

      takes = routedOn host;
      # Only routes this layer steers into: a table AND a mark. tailscale's
      # table is tailscaled's to steer.
      steered = r: r != null && r.table != null && r.mark != null;
      routed = lib.filterAttrs (_: t: steered t.route) takes;

      # A unit with no `User` runs as root, and routing root would move sshd,
      # nix and the tunnel itself -- so that is an assertion, not a skip.
      userOf =
        unit: t:
        lib.defaultTo "" (
          lib.defaultTo (config.systemd.services.${unit}.serviceConfig.User or null) t.user
        );
      usersOf = t: lib.unique (lib.filter (u: u != "") (map (u: userOf u t) t.units));
      rootUnits = lib.concatLists (
        lib.mapAttrsToList (_: t: lib.filter (u: userOf u t == "") t.units) routed
      );

      # Takers and forwarded interfaces grouped by the route they won.
      byRoute = lib.groupBy (x: x.route.carrier) (
        lib.mapAttrsToList (name: t: t // { inherit name; }) routed
        ++ lib.mapAttrsToList (iface: f: f // { inherit iface; }) forwarded
      );
      forwarded = lib.filterAttrs (_: f: steered f.route) (
        lib.mapAttrs (_: f: {
          route = resolve (entityOf host) (removeAttrs f [ "fallback" ]);
          inherit (f) fallback;
        }) cfg.forwarded
      );

      exempt = segmentsOn host ++ cfg.exemptPrefixes;
      each = f: xs: lib.concatMapStringsSep "\n" f xs;

      rulesFor =
        carrier: xs:
        let
          r = (lib.head xs).route;
          table = toString r.table;
          markOf = f: toString (r.mark + fallbacks.${f}.offset);
          strict = markOf "none";
          c = "rt-${carrier}";
          guarded = x: fallbacks.${x.fallback}.guarded;
          strictUsers = lib.concatMap usersOf (lib.filter (x: x ? units && guarded x) xs);
          ifaces = lib.filter (x: x ? iface) xs;
        in
        ''
          # ── ${carrier}: table ${table} ──
          ${each (f: ''
            ${ip} rule del fwmark ${markOf f}/0xffff table ${table} 2>/dev/null || true
            ${ip} rule add fwmark ${markOf f}/0xffff table ${table} priority ${table}
          '') (lib.attrNames fallbacks)}
          # `none` has nowhere to go when the table is empty; `main` falls
          # through to the host's own table.
          ${ip} rule del fwmark ${strict}/0xffff unreachable 2>/dev/null || true
          ${ip} rule add fwmark ${strict}/0xffff unreachable priority ${toString (r.table + 1)}

          # Local and house destinations stay unmarked; the rest gets the mark
          # of its fallback.
          ${each (f: ''
            ${ipt} -t mangle -N ${c}-${f} 2>/dev/null || ${ipt} -t mangle -F ${c}-${f}
            ${ipt} -t mangle -A ${c}-${f} -m addrtype --dst-type LOCAL -j RETURN
            ${each (p: "${ipt} -t mangle -A ${c}-${f} -d ${p} -j RETURN") exempt}
            ${ipt} -t mangle -A ${c}-${f} -j MARK --set-xmark ${markOf f}/0xffff
          '') (lib.attrNames fallbacks)}

          ${ipt} -t mangle -N ${c}-out 2>/dev/null || ${ipt} -t mangle -F ${c}-out
          ${each (
            x:
            each (u: "${ipt} -t mangle -A ${c}-out -m owner --uid-owner ${u} -j ${c}-${x.fallback}") (usersOf x)
          ) (lib.filter (x: x ? units) xs)}
          while ${ipt} -t mangle -D OUTPUT -j ${c}-out 2>/dev/null; do :; done
          ${ipt} -t mangle -A OUTPUT -j ${c}-out

          ${ipt} -t mangle -N ${c}-fwd 2>/dev/null || ${ipt} -t mangle -F ${c}-fwd
          ${each (x: "${ipt} -t mangle -A ${c}-fwd -i ${x.iface} -j ${c}-${x.fallback}") ifaces}
          while ${ipt} -t mangle -D PREROUTING -j ${c}-fwd 2>/dev/null; do :; done
          ${ipt} -t mangle -A PREROUTING -j ${c}-fwd

          # The source address follows the route, not the first lookup.
          ${ipt} -t nat -N ${c}-snat 2>/dev/null || ${ipt} -t nat -F ${c}-snat
          ${each (f: "${ipt} -t nat -A ${c}-snat -m mark --mark ${markOf f}/0xffff -j MASQUERADE") (
            lib.attrNames fallbacks
          )}
          while ${ipt} -t nat -D POSTROUTING -j ${c}-snat 2>/dev/null; do :; done
          ${ipt} -t nat -A POSTROUTING -j ${c}-snat

          # ── THE GUARD, for takers that may not fall back ──
          # Independent of the mark and the route, so a rule that failed to load
          # is still not a leak.
          ${ipt} -N ${c}-guard 2>/dev/null || ${ipt} -F ${c}-guard
          ${ipt} -A ${c}-guard -o lo -j RETURN
          ${lib.optionalString (r.dev != null) "${ipt} -A ${c}-guard -o ${r.dev} -j RETURN"}
          ${ipt} -A ${c}-guard -m addrtype --dst-type LOCAL -j RETURN
          ${each (p: "${ipt} -A ${c}-guard -d ${p} -j RETURN") exempt}
          ${ipt} -A ${c}-guard -j REJECT --reject-with icmp-host-prohibited
          ${ipt} -N ${c}-hook 2>/dev/null || ${ipt} -F ${c}-hook
          ${each (u: "${ipt} -A ${c}-hook -m owner --uid-owner ${u} -j ${c}-guard") strictUsers}
          while ${ipt} -D OUTPUT -j ${c}-hook 2>/dev/null; do :; done
          ${ipt} -I OUTPUT 1 -j ${c}-hook

          ${lib.optionalString (!r.ipv6) ''
            # No v6 on this route, so a strict taker's v6 is refused rather
            # than leaving on the host's own v6 default.
            ${ipt6} -N ${c}-no6 2>/dev/null || ${ipt6} -F ${c}-no6
            ${ipt6} -A ${c}-no6 -o lo -j RETURN
            ${ipt6} -A ${c}-no6 -d fe80::/10 -j RETURN
            ${ipt6} -A ${c}-no6 -d fd00::/8 -j RETURN
            ${ipt6} -A ${c}-no6 -m addrtype --dst-type LOCAL -j RETURN
            ${ipt6} -A ${c}-no6 -j REJECT --reject-with adm-prohibited
            ${ipt6} -N ${c}-hook6 2>/dev/null || ${ipt6} -F ${c}-hook6
            ${each (u: "${ipt6} -A ${c}-hook6 -m owner --uid-owner ${u} -j ${c}-no6") strictUsers}
            while ${ipt6} -D OUTPUT -j ${c}-hook6 2>/dev/null; do :; done
            ${ipt6} -I OUTPUT 1 -j ${c}-hook6
            ${ipt6} -N ${c}-fwd6 2>/dev/null || ${ipt6} -F ${c}-fwd6
            ${each (x: "${ipt6} -A ${c}-fwd6 -i ${x.iface} -j ${c}-no6") (lib.filter guarded ifaces)}
            while ${ipt6} -D FORWARD -j ${c}-fwd6 2>/dev/null; do :; done
            ${ipt6} -I FORWARD 1 -j ${c}-fwd6
          ''}
        '';

      dnsFor =
        t:
        lib.optionalAttrs (t.route.nameservers != [ ]) (
          lib.genAttrs t.units (_: {
            serviceConfig.BindReadOnlyPaths = [
              "${
                pkgs.writeText "route-resolv.conf" (
                  lib.concatMapStrings (n: "nameserver ${n}\n") t.route.nameservers
                )
              }:/etc/resolv.conf"
            ];
          })
        );
      pins = lib.concatMapAttrs (
        _: t:
        lib.optionalAttrs (t.user != null) (
          lib.genAttrs t.units (_: {
            serviceConfig = {
              DynamicUser = lib.mkForce false;
              User = t.user;
              Group = t.user;
            };
          })
        )
      ) routed;
      pinnedUsers = lib.unique (lib.filter (u: u != null) (lib.mapAttrsToList (_: t: t.user) routed));
    in
    {
      inherit routed rootUnits pinnedUsers;
      active = byRoute != { };
      netfilter = lib.concatStrings (lib.mapAttrsToList rulesFor byRoute);
      services = lib.mkMerge (lib.mapAttrsToList (_: dnsFor) routed ++ [ pins ]);
    };
in
{
  _module.args.routeLib = {
    inherit
      routedOn
      segmentsOn
      planFor
      tailnetPlan
      ;
  };

  den.capabilities.route = {
    meta.description = "A way to a segment, given by the carrier that reaches it.";
    includes = [ caps.carried ];
    options = {
      segment = lib.mkOption {
        type = lib.types.str;
        description = "The destination segment this route reaches.";
      };
      prefix = lib.mkOption {
        type = lib.types.str;
        description = "That segment's prefix.";
      };
      table = lib.mkOption {
        type = lib.types.nullOr (lib.types.ints.between 1 252);
        default = null;
        description = "The routing table holding it. Null is `main`, which the kernel already has.";
      };
      dev = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "The interface it leaves on, as an iptables `-o` match (`tun+`).";
      };
      mark = lib.mkOption {
        type = lib.types.nullOr (lib.types.ints.between 1 65534);
        default = null;
        description = "fwmark selecting the table; `mark + 1` selects it for takers that may fall back.";
      };
      private = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether traffic on it leaves from somewhere other than this house.";
      };
      ipv6 = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether it carries v6. A taker that may not fall back has its v6 rejected when it does not.";
      };
      nameservers = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Resolvers a taker of this route uses instead of the host's, reached over it.";
      };
      advertise = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Offer it to tailnet peers that have no route of their own: a subnet route, or for `0.0.0.0/0` an exit node.";
      };
    };
    taker.options = {
      fallback = fallbackOption;
      units = lib.mkOption {
        type = lib.types.nullOr (lib.types.listOf lib.types.str);
        default = null;
        description = "The systemd units whose sockets this take routes. Defaults to the aspect's name.";
      };
      user = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "A static user to pin the units to, for a unit nixpkgs runs with `DynamicUser`.";
      };
    };
  };

  # ── EVERY HOST'S OWN WAY OUT ──
  # The default route the kernel already holds. Ranked like any carrier, so a
  # take that states nothing gets it, and one asking for `private` does not.
  den.aspects.uplink = {
    preference = 20;
    spans = "many-hosts";
    gives = _: {
      route = _: {
        segment = "internet";
        prefix = "0.0.0.0/0";
      };
    };
  };
  den.schema.host.includes = [
    den.aspects.uplink
    den.aspects.routing
  ];

  den.aspects.routing.nixos =
    {
      config,
      pkgs,
      host ? { },
      ...
    }:
    let
      plan = planFor { inherit config pkgs host; };
    in
    {
      options.routing = {
        forwarded = lib.mkOption {
          type = lib.types.attrsOf (
            lib.types.submodule {
              options = {
                segment = lib.mkOption {
                  type = lib.types.str;
                  description = "The destination segment the forwarded traffic is routed to.";
                };
                private = lib.mkOption {
                  type = lib.types.nullOr lib.types.bool;
                  default = null;
                  description = "Demand a private route, or a non-private one. Null states nothing.";
                };
                fallback = fallbackOption;
              };
            }
          );
          default = { };
          example = {
            tailscale0 = {
              segment = "internet";
              private = true;
            };
          };
          description = "Route takes for traffic this host FORWARDS, keyed by the interface it arrives on.";
        };
        exemptPrefixes = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Further house prefixes a routed taker reaches directly, for a wire that is not yet a segment.";
        };
      };

      config = lib.mkIf plan.active {
        assertions = [
          {
            assertion = plan.rootUnits == [ ];
            message = "routing: ${lib.concatStringsSep ", " plan.rootUnits} take a route but run as root, and routes are matched by socket uid. Give the unit a `User`, or `takes.route.user`.";
          }
        ];
        systemd.services = plan.services;
        users.users = lib.genAttrs plan.pinnedUsers (u: {
          isSystemUser = true;
          group = u;
        });
        users.groups = lib.genAttrs plan.pinnedUsers (_: { });
      };
    };
}
