{ den, ... }:
# secondpc media stack + service secrets, ported from the old dotfiles
# navidrome.nix / general/default.nix. Uses raw age.secrets/age.templates in the
# nixos block (faithful to the originals; the agenix-rekey `secrets` class routes
# to the same option). Generated/rekeyed secrets all need `nix run .#rekey` and
# `nix run .#gen-secrets` before deploy.
{
  den.aspects.secondpc = {
    includes = [
      den.aspects.agenix-rekey
      den.aspects.user-pwd
    ];

    nixos =
      {
        config,
        pkgs,
        lib,
        scoped,
        ...
      }:
      {
        # Media + downloading
        services.audiobookshelf.enable = true;
        users.groups.music = { };
        users.users.music = {
          isSystemUser = true;
          group = "music";
        };
        services.navidrome = {
          enable = true;
          group = "music";
          settings = {
            user = "music";
            MusicFolder = "/mnt/hdd/Music";
          };
        };
        services.lidarr = {
          enable = true;
          user = "music";
        };
        # urlbase matches the nginx path-routing in web.nix's arr.ivymect.in
        # vhost (freeform servarr setting -> config.xml's <UrlBase>).
        services.sonarr = {
          enable = true;
          settings.server.urlbase = "/sonarr";
        };
        services.radarr = {
          enable = true;
          settings.server.urlbase = "/radarr";
        };
        services.bazarr.enable = true;
        # Indexer manager: holds the actual indexer/tracker definitions and
        # syncs them out to sonarr/radarr/lidarr's Indexers via its "Apps"
        # sync -- there is no declarative NixOS option for indexers
        # themselves, so add them through the prowlarr web UI post-deploy.
        services.prowlarr = {
          enable = true;
          settings.server.urlbase = "/prowlarr";
        };
        services.qbittorrent = {
          enable = true;
          webuiPort = 9090;
          openFirewall = true;
        };
        # Request frontend (browse/request shows+movies, forwards to
        # sonarr/radarr) -- nixpkgs merged jellyseerr/overseerr into one
        # package, still supports Plex as its backend via its own setup
        # wizard. Plex stats/monitoring dashboard alongside it.
        services.seerr.enable = true;
        services.tautulli.enable = true;

        # *arr apps and the download client all need write access to the
        # shared library at /mnt/hdd/Media (owned by the `media` group, see
        # plex.nix) so imports/hardlinks land where plex/jellyfin can see them.
        users.users.sonarr.extraGroups = [ "media" ];
        users.users.radarr.extraGroups = [ "media" ];
        users.users.bazarr.extraGroups = [ "media" ];
        users.users.qbittorrent.extraGroups = [ "media" ];

        # --- user password: ivy-password (source, intermediary) hashed into
        #     ivy-pwd-hash (generated via openssl passwd -6). ---

        # --- vaultwarden admin token: regenerated as an agenix secret (the
        #     original used sops). Fresh random token on gen-secrets. ---
        age.secrets."vaultwarden.env".generator.script =
          {
            pkgs,
            lib,
            ...
          }:
          ''
            printf 'ADMIN_TOKEN=%s\n' "$(${lib.getExe pkgs.openssl} rand -base64 48)"
          '';
        services.vaultwarden.environmentFile = config.age.secrets."vaultwarden.env".path;

        systemd.tmpfiles.settings.music = {
          "/mnt/hdd/AudioBooks"."d" = {
            user = "audiobookshelf";
            group = "music";
            mode = "0770";
          };
          "/mnt/hdd/Music/Downloads"."d" = {
            user = "music";
            group = "music";
            mode = "0770";
          };
        };

        # --- loki logs behind nginx basic-auth. htpasswd is regenerated as an
        #     agenix secret (original was sops) from ivy-password for user "ivy". ---
        age.secrets.htpasswd = {
          owner = config.services.nginx.user;
          generator = {
            dependencies = { inherit (scoped.user-pwd.secrets) ivy-password; };
            script =
              {
                pkgs,
                lib,
                decrypt,
                deps,
                ...
              }:
              ''
                ${pkgs.apacheHttpd}/bin/htpasswd -nbB ivy "$(${decrypt} ${lib.escapeShellArg deps.ivy-password.file})"
              '';
          };
        };
        security.acme.certs."logs.pierlot.com.au" = {
          environmentFile = scoped.secondpc-web.secrets."acme_cloudflare.env".path;
          group = config.services.nginx.group;
        };
        services.nginx.virtualHosts."logs.pierlot.com.au" = {
          useACMEHost = "logs.pierlot.com.au";
          forceSSL = true;
          basicAuthFile = config.age.secrets.htpasswd.path;
          locations."/" = {
            proxyPass = "http://localhost:3100";
            recommendedProxySettings = true;
          };
        };
      };
  };
}
