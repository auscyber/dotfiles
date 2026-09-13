{ den, lib }:
# The shape every servarr app shares: a gateway entry, a dashboard entry,
# `External` auth so the app shows no login form of its own behind oauth2-proxy,
# its own API key handed in through `environmentFiles`, and a reconcile unit for
# the things that live in the app's database rather than in its config.
#
# Underscore-prefixed so the dendritic auto-import skips it: this is a function
# to import, not an aspect. sonarr/radarr/lidarr come from ./servarr.nix,
# prowlarr from ../prowlarr.nix which adds the flaresolverr it drives on top.
#
# `bindaddress` is load bearing: `External` trusts whatever reaches it, so the
# app has to sit on loopback where nginx is the only way in.
{
  name,
  port,
  apiVersion,
  category ? null,
  downloadClient ? true,
  domain ? null,
  subpath ? "/",
  urlbase ? null,
  clients ? [ ],
  group ? null,
  user ? null,
  apiOwner ? null,
}:
{
  ${name} = {
    includes = [
      den.aspects.gateway
      den.aspects.homepage
    ];

    # Dashboard entry, declared by the service it describes. The widget talks to
    # the PUBLIC url with homepage's own per-caller key, so nginx does the swap
    # and the dashboard shows up in the api log as `homepage->${name}` like any
    # other caller.
    homepage =
      { config, ... }:
      {
        ${name} = {
          group = "Media";
          href = config.gateway.services.${name}.url;
          icon = "${name}.svg";
          widget = {
            type = name;
            url = config.gateway.services.${name}.url;
            key = "{{HOMEPAGE_VAR_${lib.toUpper name}_KEY}}";
          };
        };
      };

    gated.${name} = {
      inherit subpath;
      upstream = "http://127.0.0.1:${toString port}";
      websockets = true; # SignalR live updates
      api = {
        enable = true;
        inherit clients;
        internalEnv = "${lib.toUpper name}__AUTH__APIKEY";
      }
      // lib.optionalAttrs (apiOwner != null) { owner = apiOwner; };
    };

    nixos =
      {
        config,
        pkgs,
        ...
      }:
      {
        # Set here rather than in `gated` above: it needs the host's zone, and
        # inside the entry submodule `config` is the entry's own.
        gateway.services.${name} = lib.mkIf (domain != null) {
          domain = "${domain}.${config.gateway.domain}";
        };

        services.${name} = {
          enable = true;
          settings = {
            auth.method = "External";
            auth.required = "Enabled";
            server.bindaddress = "127.0.0.1";
          }
          // lib.optionalAttrs (urlbase != null) { server.urlbase = urlbase; };
          environmentFiles = [ config.age.templates."gateway/${name}.env".path ];
        }
        // lib.optionalAttrs (group != null) { inherit group; }
        // lib.optionalAttrs (user != null) { inherit user; };

        # Imports land in the shared library, so they have to be writable by
        # the group that owns it rather than by the importer alone.
        systemd.services.${name}.serviceConfig.UMask = "0002";

        # `environmentFiles` above covers this app's OWN key, because that is a
        # config.xml field. A download client is not -- it lives in the app's
        # SQLite database, with no config.xml or env equivalent -- so the only
        # declarative route is to reconcile it over the app's own REST API.
      }
      // lib.optionalAttrs downloadClient {
        #
        # No credential goes in it: qbittorrent.nix turns off LocalHostAuth, so
        # a connection from loopback is trusted already.
        systemd.services."${name}-connect" = {
          description = "Reconcile ${name}'s qbittorrent download client";
          after = [
            "${name}.service"
            "qbittorrent.service"
          ];
          requires = [ "${name}.service" ];
          wantedBy = [ "multi-user.target" ];
          path = [
            pkgs.curl
            pkgs.jq
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            EnvironmentFile = config.age.templates."gateway/${name}.env".path;
          };
          script = ''
            key="$(printenv ${lib.toUpper name}__AUTH__APIKEY)"
            base="http://127.0.0.1:${toString port}${
              lib.optionalString (urlbase != null) urlbase
            }/api/${apiVersion}"

            # The app rebuilds its database on first start, so the API can lag
            # well behind the unit being "started".
            for _ in $(seq 60); do
              if curl -sfS -H "X-Api-Key: $key" "$base/system/status" >/dev/null 2>&1; then
                break
              fi
              sleep 2
            done

            desired=$(jq -n --arg cat "${category}" '{
              enable: true,
              protocol: "torrent",
              priority: 1,
              name: "qbittorrent",
              implementation: "QBittorrent",
              configContract: "QBittorrentSettings",
              fields: [
                { name: "host",     value: "127.0.0.1" },
                { name: "port",     value: 9090 },
                { name: "useSsl",   value: false },
                { name: "username", value: "" },
                { name: "password", value: "" },
                { name: "category", value: $cat }
              ]
            }')

            id=$(curl -sfS -H "X-Api-Key: $key" "$base/downloadclient" \
              | jq -r '.[] | select(.name == "qbittorrent") | .id' | head -n1)

            if [ -n "$id" ]; then
              curl -sfS -X PUT -H "X-Api-Key: $key" -H 'Content-Type: application/json' \
                -d "$(printf '%s' "$desired" | jq --argjson id "$id" '. + {id: $id}')" \
                "$base/downloadclient/$id" >/dev/null
            else
              curl -sfS -X POST -H "X-Api-Key: $key" -H 'Content-Type: application/json' \
                -d "$desired" "$base/downloadclient" >/dev/null
            fi
          '';
        };
      };
  };
}
