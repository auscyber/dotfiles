{
  den,
  lib,
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
  servarr = import ./media/_servarr.nix { inherit den lib; };
in
{
  imports = [
    {
      den.aspects = servarr {
        name = "prowlarr";
        port = 9696;
        apiVersion = "v1";
        downloadClient = false;
        # Shares the arr vhost with sonarr/radarr, so links have to be generated
        # at the path nginx routes on.
        domain = "arr";
        subpath = "/prowlarr";
        urlbase = "/prowlarr";
        clients = [ "homepage" ];
      };
    }

    {
      den.aspects.prowlarr = {
        includes = [ den.aspects.packages.flaresolverr ];

        nixos =
          {
            config,
            pkgs,
            ...
          }:
          {
            # Prowlarr is a caller of every *arr it syncs to, so it holds one key per
            # target. `envPrefix` renders them into a single env file as
            # PROWLARR_<SERVICE>_KEY, which the reconcile unit below reads.
            gateway.serviceAccounts.prowlarr.envPrefix = "PROWLARR_";

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

                      upsert() {
                        local name="$1" impl="$2" target="$3" targetkey="$4"
                        local desired id
                        desired=$(jq -n                   --arg name "$name" --arg impl "$impl"                   --arg prowlarr "${config.gateway.services.prowlarr.url}"                   --arg base "$target" --arg key "$targetkey" '{
                            name: $name,
                            implementation: $impl,
                            configContract: ($impl + "Settings"),
                            syncLevel: "fullSync",
                            fields: [
                              { name: "prowlarrUrl", value: $prowlarr },
                              { name: "baseUrl",     value: $base },
                              { name: "apiKey",      value: $key }
                            ]
                          }')
                        id=$(curl -sfS -H "X-Api-Key: $key" "$base/applications" 2>/dev/null                   | jq -r --arg n "$name" '.[] | select(.name == $n) | .id' | head -n1)
                        if [ -n "$id" ]; then
                          curl -sfS -X PUT -H "X-Api-Key: $key" -H 'Content-Type: application/json'                     -d "$(printf '%s' "$desired" | jq --argjson id "$id" '. + {id: $id}')"                     "$base/applications/$id" >/dev/null
                        else
                          curl -sfS -X POST -H "X-Api-Key: $key" -H 'Content-Type: application/json'                     -d "$desired" "$base/applications" >/dev/null
                        fi
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
