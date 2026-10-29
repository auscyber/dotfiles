{
  den,
  ...
}:
# How Home Assistant is reached: home.<domain>, on its own vhost outside the
# gated machinery.
let
  # The one literal zone left in this directory, and it cannot be derived where
  # it is used: a `vhosts` entry's KEY is the vhost name, so it has to be known
  # while the class content is written rather than while the host's config is
  # evaluated.
  #
  # ── A FORWARDED CLASS BODY DOES NOT GET `host`, MEASURED ──
  #
  # This file used to claim the opposite: that a `vhosts` body could be a
  # function of the host's module args because ../nginx.nix forwards with
  # `adaptArgs = lib.id`, and that it was "sound by inspection" but could not
  # be confirmed by evaluation in the session it was written in. It is
  # confirmed now, the other way. `vhosts` content is put through its own
  # `lib.evalModules`, and den's per-class arguments -- `host`, `scoped`,
  # `user` -- are injected by den around a CLASS body, not by nixpkgs through
  # `_module.args`. So `{ host, ... }` there is "attribute 'host' missing", and
  # it was: every full evaluation of secondpc failed on it, surfacing as
  # `home-manager.users.auscyber.home.username` because that is merely where
  # the fixpoint happened to touch `users.users` first.
  #
  # So the resolution moved to the `nixos` body below, which is a class body
  # and does get `host`, and the vhost is written from both -- the module
  # system merges the two halves, which is the whole reason this is cheap.
  #
  # The literal zone stays for the reason above it: a vhost's KEY has to be
  # known while the content is written. The assertion below is what stops it
  # going stale.
  domain = "ivymect.in";
in
{
  # Direct TLS, not behind the shared oauth2-proxy SSO (../../hosts/secondpc/sso.nix)
  # that gates grafana/homepage/etc: HA has its own login -- kanidm-backed via
  # ./auth.nix's auth_oidc -- and the companion app plus any voice-assistant
  # integration cannot follow an interactive SSO redirect.
  #
  # Written as a `vhosts` entry rather than as `gated.home-assistant` with
  # `oauthProxy = false` (which is what ../audiobookshelf.nix does for the same
  # reason): `gateway.services.<n>` has no way to put a directive at the SERVER
  # level, and `proxy_buffering off` below has to be there rather than on the
  # location. Converting this would mean growing an `extraConfig` option on
  # that submodule first.
  den.aspects.homeassistant = {
    # `home.<domain>`, the `vhosts` entry below. A SUBDOMAIN here, although the
    # key above needs the whole name: the zone belongs to the host's carrier,
    # and ../../network/roaming-resolver.nix composes the two.
    alsoPublishes = [ "home" ];

    vhosts."home.${domain}" = {
      useACMEHost = domain;
      forceSSL = true;
      # The wiki's one nginx-side requirement. HA streams (camera, logbook
      # subscriptions) through this vhost and buffering them breaks playback.
      extraConfig = ''
        proxy_buffering off;
      '';

      # NO `proxyPass` HERE. It is resolved in the `nixos` body below, because
      # that is the body `host` reaches -- see the note at the top of this file.
      locations."/".proxyWebsockets = true;
    };

    nixos =
      {
        config,
        host ? { },
        ...
      }:
      {
        # ── THE UPSTREAM, RESOLVED RATHER THAN WRITTEN ──
        #
        # `dial` answers both halves: WHICH giver (by the name HA furnishes on
        # its `tcp` endpoint) and HOW to reach it from here (the loopback,
        # because `sameHost` says so and nothing is nearer). So neither the
        # address nor the port appears in this file -- the port is declared
        # once in ./core.nix's `gives` and the address belongs to the carrier.
        #
        # `ports.http` and not `dial`'s own `port`: HA furnishes two ports, the
        # frontend and the HomeKit bridge, so there is no single one to
        # collapse to -- which is exactly why they are named.
        services.nginx.virtualHosts."home.${domain}".locations."/".proxyPass =
          let
            ha = den.lib.capability.dial "tcp" {
              name = "home-assistant";
              sameHost = true;
            } host;
          in
          if ha != null then
            "http://${ha.address}:${toString ha.ports.http.port}"
          else
            throw "aspects/services/homeassistant/vhost.nix: nothing on ${host.name or "?"} furnishes a `tcp` endpoint named home-assistant, so there is no upstream for home.${domain}. ./core.nix's `gives.tcp` is what this resolves.";

        assertions = [
          {
            assertion = config.gateway.domain == domain;
            message = ''
              aspects/services/homeassistant/vhost.nix publishes Home Assistant at
              home.${domain}, but this host's gateway.domain is
              "${toString config.gateway.domain}". The vhost name is a literal there
              because a `vhosts` key cannot be derived from `config` (see that file);
              update it, or move this aspect to a host under ${domain}.
            '';
          }
        ];
      };
  };
}
