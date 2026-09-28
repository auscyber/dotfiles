{ den, ... }:
# secondpc's public reverse-proxy surface: ACME (cloudflare DNS-01) certs plus
# the nginx vhosts fronting the media/vault services. Included by the secondpc
# host aspect. Secrets are agenix-rekey sources (encrypted to the gpg-yubikey
# master identity) that must be rekeyed with `nix run .#rekey` before deploy.
{
  den.aspects.secondpc-web = {
    includes = [
      den.aspects.nginx
      den.aspects.agenix-rekey
      den.aspects.cloudflare
    ];

    # No cloudflare credential of its own any more: the DNS-01 environment file
    # is generated from the fleet's one account token
    # (`scoped.cloudflare.secrets.acme_env`, ../../network/cloudflare.nix),
    # which is the same token the tunnels are minted with. This aspect used to
    # own `acme_cloudflare.age`, holding that token already wrapped in lego's
    # variable name; the token moved up and the wrapping became a generator --
    # so the token itself is `intermediary` and never lands on this box.
    #
    # navidrome external-integration env (LastFM/Spotify keys, etc.) -- the one
    # secret left in this scope. `restartUnits` is still named per secret
    # rather than inferred: the scope is `secondpc-web`, which is not a
    # service, so there is nothing for inference to find.
    secrets.navidrome_env = {
      rekeyFile = ./navidrome.age;
      restartUnits = [ "navidrome.service" ];
    };

    # nginx virtualHosts (forwarded to services.nginx.virtualHosts by the nginx
    # aspect's `vhosts` class). Ports mirror the services enabled in secondpc.nix.
    vhosts = {
      "music.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1:4533"; # navidrome default
      };
      "audiobookshelf.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8000"; # audiobookshelf default
          proxyWebsockets = true;
        };
      };
      # lidarr / arr / bazarr / qbittorrent / tautulli / grafana / homepage are
      # absent on purpose: the gateway builds those vhosts, certificate
      # included, from each service aspect's own `gated` entry. Declaring one
      # here as well is a proxyPass conflict, not an override.
      # loki, on the same apex as everything else so one oauth2-proxy cookie
      # covers it (see sso.nix). The older logs.pierlot.com.au vhost in
      # media.nix stays on basic auth for whatever is already pointed at it.
      "logs.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:3100";
          recommendedProxySettings = true;
        };
      };
      "requests.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1:5055"; # seerr (ex-overseerr/jellyseerr) default
      };
      "bitwarden.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1:8222"; # vaultwarden ROCKET_PORT
      };
    };

    nixos =
      {
        config,
        pkgs,
        scoped,
        ...
      }:
      let
        # Minimal static page that turns a browser's `magnet:` protocol
        # handler into an aria2 RPC call, so clicking a magnet link anywhere
        # on the web hands it straight to aria2 (and shows the AriaNg UI to
        # track it) instead of prompting to download a .torrent-less client.
        # No RPC secret is embedded here: aria2's RPC port is bound to
        # loopback only (see plex.nix's aria2 settings) and reachable solely
        # through this vhost, which the basic-auth below gates.
        magnetHandler = pkgs.writeTextDir "index.html" ''
          <!doctype html>
          <html>
          <head><meta charset="utf-8"><title>Add magnet to aria2</title></head>
          <body style="font-family: sans-serif; max-width: 40em; margin: 3em auto;">
            <h1>Add magnet to aria2</h1>
            <p id="status">Working…</p>
            <hr>
            <button onclick="navigator.registerProtocolHandler('magnet', location.origin + '/magnet-handler/?magnet=%s', 'aria2 magnet handler')">
              Register this page as my magnet: link handler
            </button>
            <script>
              const status = document.getElementById('status');
              const magnet = new URLSearchParams(location.search).get('magnet');
              if (!magnet) {
                status.textContent = 'No magnet link provided.';
              } else {
                fetch('/jsonrpc', {
                  method: 'POST',
                  headers: { 'Content-Type': 'application/json' },
                  body: JSON.stringify({
                    jsonrpc: '2.0',
                    id: 'magnet-handler',
                    method: 'aria2.addUri',
                    params: [[magnet]],
                  }),
                })
                  .then((r) => r.json())
                  .then((data) => {
                    if (data.error) {
                      status.textContent = 'aria2 error: ' + data.error.message;
                    } else {
                      status.innerHTML = 'Added (gid ' + data.result + '). <a href="/">Open AriaNg</a>';
                    }
                  })
                  .catch((e) => (status.textContent = 'Request failed: ' + e));
              }
            </script>
          </body>
          </html>
        '';
      in
      {
        # DNS-01 certs. dnsProvider/acceptTerms/email come from the nginx aspect
        # defaults; here we only add the per-cert cloudflare credential + group.
        security.acme.certs = {
          "ivymect.in" = {
            domain = "*.ivymect.in";
            environmentFile = scoped.cloudflare.secrets.acme_env.path;
            group = config.services.nginx.group;
          };
          # jitsi auto-creates the meet.ivymect.in cert; just supply DNS creds.
          "meet.ivymect.in".environmentFile = scoped.cloudflare.secrets.acme_env.path;
        };

        # navidrome reads its external API keys from the rekeyed env file.
        services.navidrome.environmentFile = scoped.secondpc-web.secrets.navidrome_env.path;

        # AriaNg (static UI) + aria2's JSON-RPC (same-origin, so the
        # magnet-handler page needs no CORS setup) + the magnet-handler page
        # itself. aria2's RPC has no auth of its own beyond binding to
        # loopback, so this vhost is the only thing that can reach it -- and
        # sso.nix puts the whole vhost behind oauth2-proxy.
        services.nginx.virtualHosts."aria2.ivymect.in" = {
          useACMEHost = "ivymect.in";
          forceSSL = true;
          root = pkgs.ariang;
          locations."/jsonrpc".proxyPass = "http://127.0.0.1:6800/jsonrpc";
          locations."/magnet-handler/".alias = "${magnetHandler}/";
        };
      };
  };
}
