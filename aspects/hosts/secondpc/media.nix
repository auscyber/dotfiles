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
      den.aspects.gateway
      den.aspects.sonarr
      den.aspects.radarr
      den.aspects.lidarr
      den.aspects.bazarr
      den.aspects.qbittorrent
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
        # uid/gid pinned because the soularr container (slskd.nix) can only
        # name the account numerically. On a host that already allocated `media`
        # dynamically this needs a one-off `chown -R media:media /mnt/hdd`.
        # 1000, not a fresh number: the group predates this pin (plex.nix created
        # it) and NixOS will not renumber an existing group -- it silently keeps
        # the old gid while Nix believes the new one. That divergence is invisible
        # until something reads the gid at eval time, as slskd.nix does to build
        # the soularr container's numeric user, which then ran as a gid that does
        # not exist. Aligning to reality also means no data has to be chowned.
        users.groups.media.gid = 1000;
        users.users.media = {
          isSystemUser = true;
          group = "media";
          uid = 700;
        };
        services.navidrome = {
          enable = true;
          group = "media";
          settings = {
            user = "media";
            MusicFolder = "/mnt/hdd/Music";
          };
        };
        # sonarr / radarr / lidarr / bazarr / qbittorrent each live in their own
        # aspect under ../../services/media, where their gateway entry and auth
        # config sit next to the service itself. prowlarr is in
        # ../../services/prowlarr.nix, with the flaresolverr it drives.

        # Request frontend (browse/request shows+movies, forwards to
        # sonarr/radarr) -- nixpkgs merged jellyseerr/overseerr into one
        # package, still supports Plex as its backend via its own setup
        # wizard. Tautulli moved to plex.nix, which is whose stats they are.
        services.seerr.enable = true;

        # Callers of the gated APIs that are not gated services themselves, so
        # nothing registers them automatically.
        gateway.serviceAccounts = {
          seerr.description = "Request frontend, talks to sonarr and radarr";
          homepage = {
            description = "Dashboard widgets";
            # Its widgets read the keys from the environment.
            envPrefix = "HOMEPAGE_VAR_";
          };
        };

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

        systemd.tmpfiles.settings.media = {
          "/mnt/hdd/AudioBooks"."d" = {
            user = "audiobookshelf";
            group = "media";
            mode = "0770";
          };
          "/mnt/hdd/Music/Downloads"."d" = {
            user = "media";
            group = "media";
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
