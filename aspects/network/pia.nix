{
  den,
  lib,
  inputs,
  rootPath,
  routeLib,
  ...
}:
# ── PRIVATE INTERNET ACCESS: A CARRIER TO THE `internet` SEGMENT ──
#
# The tunnel, and the `route` it gives: its own table, `tun+`, private, no v6.
# Who leaves through it is not decided here -- a service TAKES a route with
# `private = true` and ./routes.nix resolves it and derives the rules, so the
# confined set is whoever asked, and the capability graph shows it.
#
# A dead tunnel empties the table; ./routes.nix then gives a taker that may not
# fall back `unreachable`, and one that may the house uplink.
#
# It is ADVERTISED, so a tailnet peer whose own private `internet` take finds
# nothing local resolves to this box as its exit node (./routes.nix), and the
# traffic this box then forwards takes the same route.
#
# ── WHAT THIS DOES AND DOES NOT TAKE FROM pia.nix ──
#
# The `pia` CLI comes from the flake. Its NixOS module does NOT, and that is
# measured rather than a preference: see `bundleHash` below.
let
  # PIA's legacy OpenVPN profile bundle -- the same artifact upstream pia.nix
  # fetches, pinned here instead of read out of its `yae.json`.
  #
  # ⚠ UPSTREAM'S PIN IS STALE, so `services.pia.enable = true` cannot be used.
  # Its module does `builtins.readDir` on the fetchzip, which makes the hash an
  # EVAL-time failure and not a build-time one -- enabling it takes the whole
  # host evaluation down. Measured 2026-10-27 against this URL:
  #
  #   specified  sha256-ZA8RS6eIjMVQfBt+9hYyhaq8LByy5oJaO9Ed+x8KtW8=   (yae.json)
  #   got        sha256-mhXV2CF8G7dAkXY9KI7g2qSOlxUzyqgJlidL1At46GU=
  #
  # The URL carries no version, so this hash will rot the same way. When it
  # does, the failure is a fixed-output build mismatch naming both hashes.
  bundleUrl = "https://www.privateinternetaccess.com/openvpn/openvpn.zip";
  bundleHash = "sha256-mhXV2CF8G7dAkXY9KI7g2qSOlxUzyqgJlidL1At46GU=";

  # The real entity, not the projection a class body is handed -- ../base/
  # firewall.nix's trap. `serves` is plain data either way, but `hasAspect` on
  # the projection answers a different question and this file reads the same
  # record that one does.
  routeTable = 198;
in
{
  den.aspects.pia = {
    description = "A PIA OpenVPN tunnel giving a private route to the internet, and this box offered as a tailscale exit node over it.";

    preference = 45;
    spans = "one-host";
    gives = _: {
      route = _: {
        segment = "internet";
        prefix = "0.0.0.0/0";
        table = routeTable;
        dev = "tun+";
        # 1198 is PIA's OpenVPN port; masked with 0xffff so tailscale's own
        # marks survive.
        mark = 1198;
        private = true;
        # PIA's legacy OpenVPN profiles are v4-only.
        ipv6 = false;
        # Public resolvers reached THROUGH the tunnel, so a query comes from
        # PIA's exit and keeps working if PIA renumbers its own.
        nameservers = [
          "9.9.9.9"
          "149.112.112.112"
        ];
        advertise = true;
      };
    };

    # `inputs.nixpkgs.follows`: the module and the CLI both close over pia.nix's
    # own `pkgs`, which is nixos-24.05 unlocked. Only the CLI is used here and
    # it is a shell script, but a second nixpkgs in the closure for that is not
    # worth it.
    inputs.pia.url = "github:Fuwn/pia.nix";
    inputs.pia.inputs.nixpkgs.follows = "nixpkgs";

    includes = [ den.aspects.agenix-rekey ];

    # ── THE CREDENTIAL, STATIC: PIA MINTS IT, WE DO NOT ──
    #
    # No `generator`, unlike ../base/builders.nix's keys -- there is nothing to
    # generate. Two lines, exactly what OpenVPN's `auth-user-pass <file>` reads
    # and what upstream pia.nix documents for `authUserPassFile`:
    #
    #   nix run .#secret-edit -- secrets/static/pia_auth.age
    #     p1234567
    #     <password>
    #
    # The username is PIA's own `pNNNNNNN` login, NOT an email. Until the file
    # holds a real one the tunnel never authenticates, which fails CLOSED: the
    # confined set has no route at all rather than a leaking one.
    secrets.pia_auth.rekeyFile = rootPath + "/secrets/static/pia_auth.age";

    nixos =
      {
        config,
        pkgs,
        scoped,
        host ? { },
        ...
      }:
      let
        cfg = config.pia;

        ip = "${pkgs.iproute2}/bin/ip";
        table = toString routeTable;
        bundle = pkgs.fetchzip {
          url = bundleUrl;
          name = "pia-openvpn-profiles";
          hash = bundleHash;
          stripRoot = false;
        };

        # Upstream names its units after the profile with `_` turned into `-`
        # (`au_sydney.ovpn` -> `openvpn-au-sydney`), and its CLI lists units to
        # build `pia start <x>`. Same mapping here so `pia start singapore`
        # means what the README says it means.
        unitOf = region: builtins.replaceStrings [ "_" ] [ "-" ] region;

        regions = lib.unique ([ cfg.region ] ++ cfg.alsoAvailable);

        # ── THE TUNNEL'S DEFAULT ROUTE, IN ITS TABLE AND ONLY THERE ──
        # `route-noexec` in the profile stops OpenVPN acting on the pushed
        # `redirect-gateway`, which would otherwise replace the BOX's default
        # route and put the lan, the wireguard hub and tailscale behind PIA.
        routeUp = pkgs.writeShellScript "pia-route-up" ''
          set -eu
          if [ -n "''${route_vpn_gateway:-}" ]; then
            ${ip} route replace default via "$route_vpn_gateway" dev "$dev" table ${table} metric 50
          else
            ${ip} route replace default dev "$dev" table ${table} metric 50
          fi
          ${ip} route flush cache
        '';

        # An empty table is what ./routes.nix reads as "down".
        routeDown = pkgs.writeShellScript "pia-route-down" ''
          set -eu
          ${ip} route del default dev "$dev" table ${table} 2>/dev/null || true
          ${ip} route flush cache
        '';

        # Whether a peer chose this box as its exit node.
        exitNode = (routeLib.tailnetPlan host).advertiseExit;
      in
      {
        options.pia = {
          region = lib.mkOption {
            # The profile basename in PIA's bundle (`singapore`, `au_sydney`).
            # Not an enum: the list lives in the fetched zip, and reading it is
            # import-from-derivation on every evaluation of this host.
            type = lib.types.strMatching "[a-z0-9_]+";
            default = "singapore";
            description = ''
              PIA region the always-on tunnel connects to. There is no
              automatic failover to another country; switch by hand with
              `pia stop <this>` / `pia start <other>`, which needs the other
              region in `alsoAvailable`.
            '';
          };

          alsoAvailable = lib.mkOption {
            type = lib.types.listOf (lib.types.strMatching "[a-z0-9_]+");
            default = [ ];
            description = "Further regions rendered as STOPPED `openvpn-<region>` units, for `pia start <region>`.";
          };
        };

        config = {
          # The upstream CLI, which is the whole of what this takes from the
          # flake: `pia list` / `pia start <region>` / `pia stop <region>`.
          environment.systemPackages = [ inputs.pia.packages.${pkgs.stdenv.hostPlatform.system}.pia ];

          services.openvpn.servers = lib.listToAttrs (
            map (
              region:
              lib.nameValuePair (unitOf region) {
                autoStart = region == cfg.region;
                # The resolvers are the route's, handed to its takers per unit
                # by ./routes.nix, not to the host.
                updateResolvConf = false;
                config = ''
                  config ${bundle}/${region}.ovpn
                  auth-nocache
                  auth-user-pass ${scoped.pia.secrets.pia_auth.path}

                  route-noexec
                  script-security 2
                  route-up ${routeUp}
                  route-pre-down ${routeDown}

                  pull-filter ignore "dhcp-option DNS"
                  pull-filter ignore "block-outside-dns"
                '';
              }
            ) regions
          );

          # ── WHAT IT FORWARDS AS AN EXIT NODE TAKES THE SAME ROUTE ──
          routing.forwarded = lib.mkIf exitNode {
            ${config.services.tailscale.interfaceName} = {
              segment = "internet";
              private = true;
            };
          };
          # ./vpn.nix's wireguard hub declares no segment prefix yet.
          routing.exemptPrefixes = [ "10.100.0.0/24" ];
        };
      };
  };
}
