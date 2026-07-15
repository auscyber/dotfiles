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
    ];

    # Cloudflare API credentials for DNS-01 (shared by every cert below).
    secrets."acme_cloudflare.env".rekeyFile = ./acme_cloudflare.age;
    # navidrome external-integration env (LastFM/Spotify keys, etc.).
    secrets.navidrome_env.rekeyFile = ./navidrome.age;

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
      "lidarr.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1:8686"; # lidarr default
      };
      "bitwarden.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1:8222"; # vaultwarden ROCKET_PORT
      };
      "jellyfin.pierlot.com.au" = {
        useACMEHost = "jellyfin.pierlot.com.au";
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8096";
          proxyWebsockets = true;
        };
      };
    };

    nixos = { config, ... }: {
      # DNS-01 certs. dnsProvider/acceptTerms/email come from the nginx aspect
      # defaults; here we only add the per-cert cloudflare credential + group.
      security.acme.certs = {
        "ivymect.in" = {
          domain = "*.ivymect.in";
          environmentFile = config.age.secrets."acme_cloudflare.env".path;
          group = config.services.nginx.group;
        };
        "jellyfin.pierlot.com.au" = {
          environmentFile = config.age.secrets."acme_cloudflare.env".path;
          group = config.services.nginx.group;
        };
        # jitsi auto-creates the meet.ivymect.in cert; just supply DNS creds.
        "meet.ivymect.in".environmentFile = config.age.secrets."acme_cloudflare.env".path;
      };

      # navidrome reads its external API keys from the rekeyed env file.
      services.navidrome.environmentFile = config.age.secrets.navidrome_env.path;
    };
  };
}
