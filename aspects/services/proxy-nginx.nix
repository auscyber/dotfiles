{
  den,
  lib,
  capability,
  ...
}:
# ── IT TRANSLATES, IT DOES NOT REIMPLEMENT ──
# NGINX AS ONE PROVIDER OF `proxy`, AND NOT THE DEFINITION OF IT.
# A service does not publish itself. It declares that it NEEDS proxying
let
  # ── WHO ASKED, AND WITH WHAT ──
  # The fold is ../base/capability.nix's, by the merge discipline each option
  # declares -- `groups` unions across contributors (which is how
  entriesFor =
    host:
    let
      asked = capability.takersOn host "proxy";
      p = capability.contributions host "proxy";
      g = capability.contributions host "gated";
      n = capability.contributions host "nginx";

      # The zone this host publishes under, which is a CARRIER it includes
      # (../network/cloudflare.nix) rather than a field anywhere -- so a
      zone =
        let
          zones = lib.filterAttrs (
            _: a: a.domain != null && a.attachment == "provider" && host.hasAspect a
          ) den.aspects;
        in
        if zones == { } then null else (lib.head (lib.attrValues zones)).domain;

      # Did this service ask for the NARROWING? A bare `takes.proxy` is
      # published and not gated; `takes.gated` is both.
      isGated = name: capability.takeKeyFor "proxy" asked.${name} == "gated";

      # ── THE UPSTREAM, RESOLVED RATHER THAN ASSUMED ──
      #
      # Was `http://127.0.0.1:${port}`, justified as "loopback is the proxy's
      # to assume" -- true as far as it went, and it meant the proxy held an
      # address for a service that never told it one. A service that gives its
      # endpoint can be ASKED instead: `dial` composes that endpoint with the
      # nearest carrier the two ends share, which for a service on this box is
      # the loopback -- the same 127.0.0.1, now derived from the carrier that
      # furnishes it rather than written here.
      #
      # `sameHost = true` because that is the actual constraint: nginx
      # forwards to something on its own box, and a service that has moved
      # should fail to resolve rather than be proxied to the wrong address.
      #
      # FALLS BACK to the take's `port` for a service that has not been
      # migrated yet -- `takes.gated.port` is a number with no endpoint behind
      # it, so there is nothing to resolve. The fallback is what lets the
      # services move one at a time instead of in one commit.
      resolved =
        name:
        let
          d = capability.dial "tcp" {
            name = name;
            sameHost = true;
          } host;
        in
        if d == null || d.port == null then null else "http://${d.address}:${toString d.port}";

      entry =
        name:
        let
          port = p.port.${name} or null;
          upstream = p.upstream.${name} or null;
          dialled = resolved name;
        in
        {
          upstream =
            if upstream != null then
              upstream
            else if dialled != null then
              dialled
            else if port != null then
              "http://127.0.0.1:${toString port}"
            else
              throw "proxy-nginx: `${name}` takes `gated`/`proxy` but neither gives a `tcp` endpoint this host can reach nor states a `port`, so there is nothing to forward to. Give it `gives.tcp.ports.http` (which is also what makes its public https address derivable) or set `upstream` explicitly.";

          # `sso` only means anything for a gated service. A bare proxy take is
          # published without a gate, so it is false rather than defaulted.
          oauthProxy = if isGated name then (g.oauthProxy.${name} or true) else false;

          # Only a GATED service has an authenticated person to forward: a bare
          # proxy take has no `auth_request` in front of it, so there is no
          # variable to read and the answer is false rather than defaulted --
          # the same reasoning `oauthProxy` above uses.
          forwardUser = if isGated name then (g.forwardUser.${name} or false) else false;
        }
        // lib.optionalAttrs ((p.subdomain.${name} or null) != null) {
          # ── `domain`, COMPOSED, NOT `subdomain` ──
          # `gateway.services.<n>` has no `subdomain` -- that is
          # `gateway.oidcClients.<n>`'s. The gated entry takes a whole `domain`,
          domain = "${p.subdomain.${name}}.${zone}";
        }
        // lib.optionalAttrs ((p.subpath.${name} or "/") != "/") {
          subpath = p.subpath.${name};
        }
        // lib.optionalAttrs (p.websockets.${name} or false) {
          websockets = true;
        }
        // lib.optionalAttrs ((p.maxBodyBytes.${name} or null) != null) {
          # The SERVICE's limit. The CARRIER's cap is a separate fact, and
          # ../network/cloudflare.nix's `excluded` reconciles the two: a vhost
          maxBodyBytes = p.maxBodyBytes.${name};
        }
        # ── NO `extraGroups` FROM THE `gated` CONTRIBUTION: IT WAS DEAD ──
        #
        # This read `g.groups.${name} or [ ]`. `gated.groups` was a `merge =
        # "union"` takerOption, and the union fold is `unique (concatLists
        # (attrValues per))` -- a FLAT LIST with the per-taker key discarded. So
        # `.${name}` selected an attribute off a list, which with `or` yields
        # the default SILENTLY (`[ "a" "b" ].grocy or [ "D" ]` is `[ "D" ]`),
        # the guard was never true, and this branch never ran. Measured:
        # `contributions secondpc "gated"` gave `groups = [ ]` with 20 takers.
        #
        # Nothing wrote it either, so it was dead at both ends. A service's
        # gate groups come from its own `gated.<n>.extraGroups` now: declared
        # where they are consumed and typed by the option that reads them,
        # which is the difference between failing loudly and defaulting
        # quietly.
        // (
          # ── THE TWO HALVES OF `api`, MERGED ──
          # `clients` is the SERVICE's fact (who may call it, which any gate can
          # enforce) and comes from the `gated` take. The rest -- which header,
          let
            fromNginx = n.api.${name} or { };
            fromGated = lib.optionalAttrs (isGated name && (g.clients.${name} or [ ]) != [ ]) {
              clients = g.clients.${name};
            };
            api = fromNginx // fromGated;
          in
          lib.optionalAttrs (api != { }) { inherit api; }
        )
        // lib.optionalAttrs ((n.banner.${name} or null) != null) {
          banner = n.banner.${name};
        }
        // lib.optionalAttrs ((n.landing.${name} or null) != null) {
          landing = n.landing.${name};
        };

      # ── A TAKER MAY RECORD PUBLICATION FACTS WITHOUT WANTING A VHOST ──
      # A service that renders its OWN server block still needs the rest of the
      # tree to know where it publishes and how large a body it accepts:
      #
      # A GIVEN ENDPOINT COUNTS TOO, and it has to: a migrated service states
      # no `port` on its take any more, so a predicate that only looked at the
      # take decided those services wanted no vhost at all -- measured, as
      # `The option `gateway.services.bazarr.upstream' was accessed but has no
      # value defined`.
      emits =
        name:
        (p.port.${name} or null) != null || (p.upstream.${name} or null) != null || resolved name != null;
    in
    # A HOST WITH NO ZONE PUBLISHES NOTHING. Entries whose `domain` interpolated
    # a null would be names like `music.`, so a host including no zone carrier
    # simply has none -- the honest answer for a machine that publishes nothing.
    if zone == null then { } else lib.genAttrs (lib.filter emits (lib.attrNames asked)) entry;
in
{
  # ── THE GATE, A NARROWING OF WHAT NGINX GIVES ──
  # Giving `gated` satisfies a bare `takes.proxy` as well -- a gate-capable
  # proxy can also just proxy -- while a plain `proxy` giver cannot answer a
  den.aspects.gated-oauth2-proxy = {
    includes = [
      den.aspects.proxy-nginx
      den.aspects.sso
    ];

    gives =
      { self, ... }:
      {
        gated = _: {
          carrier = self.name;
        };

        # THE EXTRAS ONLY THIS PROXY CAN SERVE -- the API key rewrite, the
        # banner, the landing page. A service needing them takes `nginx`
        nginx = _: {
          carrier = self.name;
        };
      };

    reach = "host";
  };

  den.aspects.proxy-nginx = {
    includes = [ den.aspects.gateway ];

    gives =
      { self, ... }:
      {
        proxy = _: {
          carrier = self.name;
        };

        user = _: {
          name = "nginx";
          group = "nginx";
          carrier = self.name;
        };
      };

    reach = "host";

    # ── THE TRANSLATION ──
    # Hands `gateway.services` the same shape it already consumes, so there is
    # ONE implementation of the gate and the capability is how a service asks
    nixos =
      {
        host ? { },
        ...
      }:
      {
        gateway.services = entriesFor host;
      };
  };
}
