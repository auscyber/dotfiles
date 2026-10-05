{ den, accessLib, ... }: {
  den.aspects.bazarr = {
    # `arr.<zone>`, the shared vhost this and the four other *arr apps sit
    # under by `subpath`. It is set from `gateway.services.bazarr.domain` in the
    # `nixos` body below -- host config, so no flake-level reader can see it.
    # ../../network/roaming-resolver.nix is the one that needs to.
    alsoPublishes = [ "arr" ];

    # The dashboard tile is a capability this aspect gives
    # (`gives.homepage-widget` below), so it no longer includes the dashboard.
    includes = [ den.aspects.gateway ];

    # `/subs` on the shared arr vhost: bazarr manages subtitles, and the
    # siblings are named after what they manage rather than after the program
    # (/tv, /movies, /music, /indexers).
    #
    # bazarr DOES take a base path -- `general.base_url` in its config.yaml,
    # which it mounts both its UI blueprint and its `/api` blueprint under -- so
    # the gateway's `subpath` + derived `api.subpath` line up with it exactly.
    # The seeding unit below is what sets it, since bazarr has no environment
    # configuration at all.
    #
    # ── WHAT IT LISTENS ON, so the proxy can ASK rather than assume ──
    #
    # The port used to be a number inside `takes.gated`, which meant the only
    # thing that could reach this service was the gate, and only because it
    # hardcoded `http://127.0.0.1:<that number>`. Given here it is an endpoint:
    # ../../base/capability.nix composes it with whichever carrier the two ends
    # share -- the loopback for nginx on this box -- and the proxy resolves the
    # upstream instead of assuming one.
    #
    # NOT EXPOSED: loopback only, because the gate is the way in. The firewall
    # is derived from this (../../base/firewall.nix), so saying nothing here
    # means the port stays shut on every wire.
    gives = { self, ... }: {
      # WHAT THE HOUSE OFFERS: tagged `arr` so one grant covers it with the rest of the stack.
      service = _: {
        group = "arr";
      };

      # The dashboard tile. No `href` and no widget `url` -- both are the
      # gateway's published address for this service -- and no `key`, because
      # `takes.gated.clients` below admits homepage and ../homepage.nix
      # composes the placeholder from the credential it holds.
      homepage-widget = _: {
        group = "Media";
        icon = "bazarr.svg";
        widget.type = "bazarr";
      };

      tcp = _: {
        name = self.name;
        ports.http = {
          port = 6767;
          meta.description = "Bazarr's web UI and REST API on loopback; nginx is the only dialer and it substitutes bazarr's own key for an api caller's.";
        };
      };
    };

    # `internalEnv = null` with `internalKey = true`: bazarr reads no environment
    # configuration at all, so the gateway mints it a key but cannot hand it over
    # -- the unit below writes it into bazarr's own config instead.
    # ── WHAT A GRANT ON THE `arr` TAG DOES ──
    #
    # Keyed on the TAG, not on this app: one grant on the household's `arr`
    # group covers all six of these apps, and the gate is how it is honoured.
    # ../access.nix used to write it centrally for every service in the tree --
    # the gate is these apps' own mechanism, so it is declared beside the take
    # that creates it.
    gated.bazarr.extraGroups = accessLib.groupsFor "arr";

    # Out through a private route (../../network/routes.nix), and not at all
    # when it is down.
    takes.route = {
      segment = "internet";
      private = true;
    };

    takes.gated = {
      subpath = "/subs";
      # WHO MAY CALL IT -- the service's own fact, which any gate can enforce.
      clients = [ "homepage" ];
    };

    # ── NGINX-SPECIFIC, SO DECLARED AS SUCH ──
    #
    # The key-rewrite machinery is a fact about the PROXY, not about this
    # service: which header carries the key, whether it is swapped for an
    # internal one, which env var the service reads it from. Taking `nginx`
    # says this service depends on that particular proxy -- a host running
    # traefik does not give `nginx`, so this fails to resolve rather than
    # publishing with the rewrite silently missing.
    #
    # `clients` stays on `takes.gated`: who may call this is the service's own
    # fact and any gate can enforce it.
    takes.nginx.api = {
      enable = true;
      internalKey = true;
      internalEnv = null;
    };

    nixos =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        # Set here rather than in `gated` above: it needs the host's zone, and
        # inside the entry submodule `config` is the entry's own.
        gateway.services.bazarr.domain = "arr.${config.gateway.domain}";

        services.bazarr = {
          enable = true;
          group = "media";
        };
        systemd.services.bazarr.serviceConfig.UMask = "0002";

        # bazarr keeps its settings in a YAML file it rewrites at runtime, so
        # this seeds them BEFORE it starts rather than trying to own the file.
        # `auth.type: null` turns off bazarr's own login -- reaching it at all
        # means oauth2-proxy let you through -- which is bazarr's equivalent of
        # the servarr apps' `auth.method = "External"`.
        systemd.services.bazarr-config = {
          description = "Seed bazarr's API key and disable its own login";
          before = [ "bazarr.service" ];
          requiredBy = [ "bazarr.service" ];
          serviceConfig.Type = "oneshot";
          path = [ pkgs.yq-go ];
          script =
            let
              dir = config.services.bazarr.dataDir;
            in
            ''
              cfg=${lib.escapeShellArg dir}/config/config.yaml
              install -d -o bazarr -g media ${lib.escapeShellArg dir}/config
              [ -f "$cfg" ] || echo '{}' > "$cfg"

              # Runs as root because the key is root-owned; hand the file back
              # afterwards or bazarr cannot rewrite its own settings.
              # `base_url` re-asserted on every start, not just seeded once:
              # bazarr rewrites config.yaml whenever anything is saved in its
              # own settings page, and a base_url that drifts from the location
              # nginx routes on is a UI whose every asset 404s.
              key=$(cat ${config.age.secrets."bazarr/internal".path})
              yq -i ".auth.apikey = \"$key\" | .auth.type = null | .general.base_url = \"${lib.removeSuffix "/" config.gateway.services.bazarr.subpath}\"" "$cfg"
              chown bazarr:media "$cfg"
            '';
        };
      };
  };
}
