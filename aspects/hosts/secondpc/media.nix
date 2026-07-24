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
        ...
      }:
      {
        # Hardware accel (intel) — feeds jellyfin's LIBVA transcoding below.
        hardware.graphics = {
          enable = true;
          extraPackages = with pkgs; [
            intel-ocl
            intel-vaapi-driver
            libva-vdpau-driver
          ];
        };
        systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "i965";
        environment.sessionVariables.LIBVA_DRIVER_NAME = "i965";

        # Media + downloading
        services.jellyfin = {
          enable = true;
          openFirewall = true;
        };
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
        services.qbittorrent = {
          enable = true;
          webuiPort = 9090;
          openFirewall = true;
        };

        environment.systemPackages = with pkgs; [
          jellyfin
          jellyfin-web
          jellyfin-ffmpeg
        ];

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
            dependencies = { inherit (config.age.secrets) ivy-password; };
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
          environmentFile = config.age.secrets."acme_cloudflare.env".path;
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
