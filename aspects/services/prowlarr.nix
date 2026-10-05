{
  den,
  lib,
  capability,
  accessLib,
  ...
}:
# Indexer manager. The servarr shape it shares with sonarr/radarr/lidarr comes
# from ./media/_servarr.nix; what is actually prowlarr's own is below -- the
# flaresolverr it drives, and the Applications list that is the reason the
# per-caller keys exist at all.
#
# It has no download client of its own (`downloadClient = false`): it hands
# releases to the *arr apps, which have theirs.
let
  servarr = import ./media/_servarr.nix {
    inherit
      den
      lib
      capability
      accessLib
      ;
  };
in
{
  imports = [
    {
      den.aspects = servarr {
        name = "prowlarr";
        port = 9696;
        # One grant covers the whole *arr stack; see the generator.
        serviceGroup = "arr";
        # nixpkgs runs prowlarr with `DynamicUser`, whose uid no rule can name,
        # so it is pinned to a static account.
        route = {
          segment = "internet";
          private = true;
          user = "prowlarr";
        };
        apiVersion = "v1";
        downloadClient = false;
        # Shares the arr vhost with sonarr/radarr/lidarr/bazarr. `/indexers`
        # rather than `/prowlarr`: the siblings are named after what they manage
        # (/tv, /movies, /music, /subs) and this is the indexer manager.
        domain = "arr";
        subpath = "/indexers";
        clients = [ "homepage" ];
      };
    }

    {
      den.aspects.prowlarr = {
        includes = [ den.aspects.packages.flaresolverr ];

        # ── PROWLARR IS A CALLER TOO, AND THAT HALF IS ITS OWN FACT ──
        #
        # It holds one key and presents it to every *arr it syncs to, so the
        # account comes from the siblings that name `prowlarr` in their
        # `api.clients` (./media/servarr.nix) and ../services/gateway.nix
        # derives it. What is left is how THIS end reads the key: rendered into
        # one env file under this prefix, and read from the environment by the
        # reconcile unit below -- which therefore has to be restarted when the
        # file is rewritten.
        #
        # Was `gateway.serviceAccounts.prowlarr` in the `nixos` body below. The
        # facts are the same; stating them as a take is what keeps them from
        # also being what BRINGS the account into being.
        takes.credential = {
          envPrefix = "PROWLARR_";
          restartUnits = [ "prowlarr-connect.service" ];
        };

        nixos =
          {
            config,
            pkgs,
            ...
          }:
          {
            # The Applications list is a table in prowlarr's database with no
            # config.xml or env equivalent, so it gets reconciled over prowlarr's own
            # API. This is the entry that makes the per-caller keys earn their keep:
            # each target is registered at its PUBLIC url with prowlarr's own key for
            # it, so nginx does the swap and the *arr access log attributes every
            # sync to `prowlarr->sonarr` rather than to an anonymous shared secret.
            systemd.services.prowlarr-connect =
              let
                targets = lib.filterAttrs (
                  n: svc: n != "prowlarr" && svc.api.enable && lib.elem "prowlarr" svc.api.clients
                ) config.gateway.services;
              in
              lib.mkIf (targets != { }) {
                description = "Reconcile prowlarr's application list";
                after = [ "prowlarr.service" ] ++ map (n: "${n}.service") (lib.attrNames targets);
                requires = [ "prowlarr.service" ];
                wantedBy = [ "multi-user.target" ];
                path = [
                  pkgs.curl
                  pkgs.jq
                ];
                serviceConfig = {
                  Type = "oneshot";
                  RemainAfterExit = true;
                  EnvironmentFile = [
                    config.age.templates."gateway/prowlarr.env".path
                    config.age.templates."gateway/account-prowlarr.env".path
                  ];
                };
                script = ''
                      key="$(printenv PROWLARR__AUTH__APIKEY)"
                  # Prowlarr's own service-account credential: one key, presented to
                  # every *arr it syncs to, swapped for that *arr's internal key by
                  # nginx on the way through.
                  key_out="$(printenv PROWLARR_KEY)"
                      base="${config.gateway.services.prowlarr.url}/api/v1"

                      for _ in $(seq 60); do
                        if curl -sfS -H "X-Api-Key: $key" "$base/system/status" >/dev/null 2>&1; then
                          break
                        fi
                        sleep 2
                      done

                      api() { curl -sS -H "X-Api-Key: $key" "$@"; }

                      # Built from prowlarr's own /applications/schema, not
                      # hand-written: an application entry carries required
                      # fields beyond the three worth setting, and omitting any
                      # is a flat 400 naming none of them.
                      upsert() {
                        local label="$1" impl="$2" target="$3" targetkey="$4"
                        local tmpl desired id code resp

                        tmpl=$(api "$base/applications/schema" \
                          | jq --arg i "$impl" '[.[] | select(.implementation == $i)] | .[0]')
                        if [ -z "$tmpl" ] || [ "$tmpl" = "null" ]; then
                          echo "prowlarr has no $impl application in its schema" >&2
                          return 1
                        fi

                        desired=$(printf '%s' "$tmpl" | jq \
                          --arg name "$label" \
                          --arg prowlarr "${config.gateway.services.prowlarr.localUrl}" \
                          --arg base "$target" --arg key "$targetkey" '
                            .name = $name
                            | .syncLevel = "fullSync"
                            | .fields = (.fields | map(
                                if   .name == "prowlarrUrl" then .value = $prowlarr
                                elif .name == "baseUrl"     then .value = $base
                                elif .name == "apiKey"      then .value = $key
                                else . end))')

                        # `prowlarrUrl` is the LOOPBACK url, unlike `baseUrl`:
                        # prowlarr validates the app by making the *arr call back
                        # to it, and that callback carries no key, so through the
                        # gate it is just an unauthenticated request that gets
                        # refused -- "Prowlarr URL is invalid, <app> cannot
                        # connect". `baseUrl` stays public because that leg does
                        # carry prowlarr's per-caller key.
                        #
                        # Matched on implementation: an entry added through the
                        # UI is named "Sonarr", so a name lookup misses it and
                        # the POST then collides with it.
                        id=$(api "$base/applications" \
                          | jq -r --arg i "$impl" '.[] | select(.implementation == $i) | .id' | head -n1)

                        resp=$(mktemp)
                        if [ -n "$id" ]; then
                          desired=$(printf '%s' "$desired" | jq --argjson id "$id" '. + {id: $id}')
                          code=$(api -o "$resp" -w '%{http_code}' -X PUT \
                            -H 'Content-Type: application/json' -d "$desired" "$base/applications/$id")
                        else
                          code=$(api -o "$resp" -w '%{http_code}' -X POST \
                            -H 'Content-Type: application/json' -d "$desired" "$base/applications")
                        fi

                        case "$code" in
                          2*) ;;
                          *)
                            echo "prowlarr rejected the $impl application (HTTP $code):" >&2
                            cat "$resp" >&2
                            return 1
                            ;;
                        esac
                      }

                      ${lib.concatMapStringsSep "
" (n: ''
                        upsert ${lib.escapeShellArg n} ${
                          lib.escapeShellArg (lib.toUpper (lib.substring 0 1 n) + lib.substring 1 (lib.stringLength n) n)
                        }                   ${
                          lib.escapeShellArg config.gateway.services.${n}.url
                        }                   "$key_out"
                      '') (lib.attrNames targets)}
                '';
              };

            # mkDefault so a podman host can still say otherwise; the module turns
            # on whichever runtime this names.
            virtualisation.oci-containers.backend = lib.mkDefault "docker";

            # Cloudflare-challenge solver, packaged upstream only as a container.
            # Prowlarr reaches it at http://127.0.0.1:8191 as an indexer proxy --
            # host networking keeps it there instead of publishing a port, which
            # would bypass the firewall.
            virtualisation.oci-containers.containers.flaresolverr = {
              # `image` has to spell the same name:tag the tarball carries, or the
              # backend pulls over the network anyway.
              imageFile = pkgs.flaresolverr-image;
              image = "ghcr.io/flaresolverr/flaresolverr:${pkgs.flaresolverr-image.imageTag}";
              environment.LOG_LEVEL = "info";
              extraOptions = [ "--network=host" ];
            };
          };
      };
    }
  ];
}
