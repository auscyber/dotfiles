{
  den,
  lib,
  accessLib,
}:
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
  clients ? [ ],
  group ? null,
  # THE ACCESS TAG, and deliberately not `group` above -- that one is the UNIX
  # group these apps run as (`media`). One grant covering all six is a
  # different question from which account owns the files, and reusing the word
  # would make `gives.service.group = group` read as correct while meaning the
  # wrong thing.
  serviceGroup ? null,
  user ? null,
  apiOwner ? null,
  # A `takes.route` (../../network/routes.nix): how this app reaches the
  # internet, or null for the host's own default.
  route ? null,
}:
# Derived, never passed in: `server.urlbase` is what makes the app emit its own
# links at the prefix nginx routes on, so a value that disagrees with `subpath`
# is a UI that half works -- the page loads and every asset and redirect on it
# 404s. One source for both.
let
  urlbase = if subpath == "/" then null else lib.removeSuffix "/" subpath;
in
{
  ${name} =
    lib.recursiveUpdate
      {
        # NO `den.aspects.homepage` ANY MORE. The dashboard tile is a capability
        # this aspect GIVES (`gives.homepage-widget` below), so the dependency runs
        # the other way: ../homepage.nix reads whichever givers are on the host.
        # Including it here said that running sonarr requires a dashboard, which
        # dragged the whole of homepage onto any host that ran one *arr app.
        includes = [ den.aspects.gateway ];

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
        gives =
          { self, ... }:
          {
            # WHAT THE HOUSE OFFERS, and what a grant in it covers. The LEVELS a
            # grant carries stay on the household's `serviceGroups` -- what a
            # grant means to people is the house's fact -- while the tag that says
            # which services one grant reaches is the service's own.
            service = _: {
              group = serviceGroup;
            };

            # ── THE DASHBOARD TILE, DECLARED BY THE SERVICE IT DESCRIBES ──
            #
            # Everything that can be derived is: no `href`, because the gateway
            # already publishes this app at `gateway.services.<n>.url` and that is
            # what ../homepage.nix reads; no widget `url`, which is the same value;
            # and no `key`, because `clients` below already admits homepage as an
            # api caller and the dashboard composes its own placeholder from the
            # credential it holds. So the widget talks to the PUBLIC url and shows
            # up in the api log as `homepage->${name}` like any other caller.
            homepage-widget = _: {
              group = "Media";
              icon = "${name}.svg";
              widget.type = name;
            };

            tcp = _: {
              name = self.name;
              ports.http = {
                inherit port;
                meta.description = "${name}'s web UI and REST API; nginx is the only dialer and it swaps an api caller's own key for ${name}'s on the way through.";
              };
            };
          };

        # ONE TAKE PER *ARR APP, from the one generator. `takes.gated` is keyed by
        # the TAKING ASPECT rather than by a name written here, so each app's
        # contribution arrives under its own aspect and nothing has to agree on the
        # key -- which is what `gated.${name}` was doing by hand.
        # ── WHAT A GRANT ON THE `arr` TAG DOES ──
        #
        # One grant on the household's `arr` group covers all six of these
        # apps, and the gate is how it is honoured: oauth2-proxy fronts every
        # one of them, so the realisation is the tag's group added to each
        # vhost's allowed set. `accessLib.groupsFor serviceGroup` asks about
        # the TAG rather than about this app -- `app-arr-admin`, not
        # `app-sonarr-admin` -- which is what makes one grant open six doors.
        #
        # ../access.nix used to write this centrally for every service in the
        # tree; it is these apps' own mechanism and is declared beside the take
        # that creates it.
        gated.${name}.extraGroups = lib.optionals (serviceGroup != null) (accessLib.groupsFor serviceGroup);

        takes.${if route == null then null else "route"} = route;

        takes.gated = {
          inherit subpath;
          websockets = true; # SignalR live updates
          # WHO MAY CALL IT -- the service's own fact, which any gate can enforce.
          inherit clients;
        };

        # ── NGINX-SPECIFIC, SO DECLARED AS SUCH ──
        #
        # The key rewrite is a fact about the PROXY: the gate validates the caller's
        # own key and substitutes this app's internal one on the way through, and
        # `internalEnv` is the variable the app reads it from. Taking `nginx` says
        # this app depends on that particular proxy -- a host running traefik does
        # not give `nginx`, so it fails to resolve rather than being published with
        # the rewrite silently missing and every caller's key rejected.
        takes.nginx.api = {
          enable = true;
          internalEnv = "${lib.toUpper name}__AUTH__APIKEY";
        }
        // lib.optionalAttrs (apiOwner != null) { owner = apiOwner; };

        # ── THE DOWNLOAD CLIENT, AS A TAKE ──
        #
        # `downloadClient = true` means "this app's download client is qbittorrent",
        # and until now that sentence existed only inside the reconcile script's
        # shell, as `127.0.0.1` and `9090` written out by hand. Three things were
        # wrong with that and none of them were visible:
        #
        #   * the PORT was a second copy of a number ../media/qbittorrent.nix
        #     already publishes through `gives.tcp.ports.http`, so changing it there
        #     left every *arr app pointing at a closed port -- with the reconcile
        #     unit reporting success, because the app accepts a download client it
        #     cannot reach;
        #   * the ADDRESS was a literal loopback, which is only correct while the
        #     two are co-located. Moving qbittorrent to another box is a silent
        #     mis-point rather than an error;
        #   * and the DEPENDENCY was invisible to the capability layer, so nothing
        #     could answer "which apps need qbittorrent" -- not the firewall, not
        #     `linksBetween`, not ../../base/service-reach.nix's runtime check.
        #
        # One take fixes all three. `name` picks the service rather than a port, the
        # same way ../vban.nix picks its receiver, and the address and port come
        # back from the resolved link below. `recursiveUpdate` at the bottom of this
        # aspect is what adds it, rather than a `takes.tcp` here: a plain `//` would
        # shallow-replace the whole `takes` set and silently drop `gated` and
        # `nginx` with it.

        nixos =
          {
            config,
            pkgs,
            host ? { },
            ...
          }:
          let
            # `host` and not a captured value: a forwarded body does not get den's
            # `host`, which cost an "attribute 'host' missing" on every secondpc
            # eval once already, so the dial lives in the `nixos` body where it is
            # actually supplied.
            client =
              if downloadClient then den.lib.capability.dial "tcp" { name = "qbittorrent"; } host else null;

            # A THROW, not a fallback, and for ../vban.nix's reason: there is no safe
            # default endpoint for "reconcile a download client". The old literals
            # produced a unit that ran, reported success, and pointed the app at
            # whatever happened to be on 127.0.0.1:9090.
            endpoint =
              if !downloadClient then
                null
              else if client != null then
                client
              else
                throw "${name}: nothing on ${host.name or "this host"} resolves `tcp` from a service named `qbittorrent`, so its download client cannot be reconciled. Either include `den.aspects.qbittorrent` on this host, or pass `downloadClient = false` to the servarr maker for an app that uses a different client.";
          in
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
              # `after` only, deliberately: qbittorrent may legitimately be on
              # another box, in which case there is no local unit to wait for and
              # the resolved address above is not a loopback one. The curl retry
              # loop below is what actually tolerates a slow start, on either box.

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

                api() { curl -sS -H "X-Api-Key: $key" "$@"; }

                # Build the body FROM the app's own schema rather than hand-writing
                # it. A download client has required fields beyond the handful worth
                # setting, and they differ between apps and versions -- omitting any
                # of them is a flat 400 with no indication which. Taking the schema
                # entry and overriding only what we care about keeps every default
                # the app expects.
                tmpl=$(api "$base/downloadclient/schema" \
                  | jq '[.[] | select(.implementation == "QBittorrent")] | .[0]')
                if [ -z "$tmpl" ] || [ "$tmpl" = "null" ]; then
                  echo "${name}: no QBittorrent download client in its schema" >&2
                  exit 1
                fi

                desired=$(printf '%s' "$tmpl" | jq --arg cat "${category}" '
                  .name = "qbittorrent"
                  | .enable = true
                  | .fields = (.fields | map(
                      if   .name == "host"     then .value = "${endpoint.address}"
                      elif .name == "port"     then .value = ${toString endpoint.ports.http.port}
                      elif .name == "useSsl"   then .value = false
                      elif .name == "category" then .value = $cat
                      else . end))')

                # Matched on implementation, not name: an entry added through the UI
                # is called "qBittorrent" with a capital B, so a name lookup misses
                # it, the POST below collides with it ("Name: Should be unique") and
                # the stale entry -- pointing whereever it was originally pointed --
                # keeps being polled.
                id=$(api "$base/downloadclient" \
                  | jq -r '.[] | select(.implementation == "QBittorrent") | .id' | head -n1)

                # No `curl -f`: it hides the response body, and the body is the only
                # thing that says WHICH field the app objected to.
                resp=$(mktemp)
                if [ -n "$id" ]; then
                  desired=$(printf '%s' "$desired" | jq --argjson id "$id" '. + {id: $id}')
                  code=$(api -o "$resp" -w '%{http_code}' -X PUT \
                    -H 'Content-Type: application/json' -d "$desired" "$base/downloadclient/$id")
                else
                  code=$(api -o "$resp" -w '%{http_code}' -X POST \
                    -H 'Content-Type: application/json' -d "$desired" "$base/downloadclient")
                fi

                case "$code" in
                  2*) ;;
                  *)
                    echo "${name} rejected the qbittorrent download client (HTTP $code):" >&2
                    cat "$resp" >&2
                    exit 1
                    ;;
                esac
              '';
            };
          };
      }
      (
        lib.optionalAttrs downloadClient {
          takes.tcp = {
            name = "qbittorrent";
          };
        }
      );
}
