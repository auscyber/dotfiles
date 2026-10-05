{ den, ... }:
# secondpc's public reverse-proxy surface: ACME (cloudflare DNS-01) certs, and
# the secrets that go with them. Included by the secondpc host aspect. Secrets
# are agenix-rekey sources (encrypted to the gpg-yubikey master identity) that
# must be rekeyed with `nix run .#rekey` before deploy.
#
# THE VHOSTS ARE NO LONGER HERE. Each service declares `takes.proxy` beside its
# own config and ../../services/proxy-nginx.nix renders them -- see the note
# below.
{
  den.aspects.secondpc-web = {
    # `aria2.<zone>`, the raw vhost below -- deliberately raw (see the note
    # there), which is exactly why ../../network/roaming-resolver.nix has to be
    # told rather than being able to fold it out of `takes.proxy`.
    alsoPublishes = [ "aria2" ];

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
    # ── THE FOUR RAW VHOSTS ARE GONE FROM HERE ──
    #
    # `music`, `logs`, `requests` and `bitwarden` were declared here while their
    # services were configured in ./media.nix, ./observability.nix and
    # ./vaultwarden.nix -- each service spread across two aspects, with the
    # vhost in a file that knows nothing about the service.
    #
    # Each is now its own aspect declaring `takes.proxy`, beside its own
    # service config, and ../../services/proxy-nginx.nix renders the vhost. The
    # rendering is the same: `gateway.acmeHost` defaults to the same zone this
    # wrote out, and `recommendedProxySettings` is global in
    # ../../services/nginx.nix rather than per location.
    #
    # The arr apps, qbittorrent, tautulli, grafana, homepage and audiobookshelf
    # were never here either, and for the same reason one step earlier: the
    # gateway builds those vhosts from each service's own entry, and declaring
    # one here as well is a proxyPass conflict rather than an override.

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

        # ── NOT CONVERTED TO `takes.proxy`, DELIBERATELY ──
        #
        # This is not a service publication. It is a static UI served from a
        # `root`, an `alias` location for the magnet handler, and a proxyPass on
        # ONE sub-path -- three nginx features and one proxy. Expressing it
        # through the capability would mean putting `root` and `alias` on
        # `den.capabilities.proxy`, which is nginx vocabulary: the type would
        # become the union of one implementation's options, and traefik would
        # inherit fields that mean nothing to it.
        #
        # The capability covers "publish this service under a name and forward
        # to it". This is a web page that happens to proxy, so it stays raw.
        #
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
