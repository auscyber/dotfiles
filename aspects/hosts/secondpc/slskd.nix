{
  den,
  inputs,
  ...
}:
# slskd (soulseek daemon) + its soularr (lidarr<->slskd) bridge. Everything here
# runs as `music`, the same user lidarr and navidrome use, so a grab lands in
# /mnt/hdd/Music/Downloads already owned by the account that imports it.
# Secrets (slskd.env, slskd_soularr_apikey, slskd_secrets_env, lidar_key) need
# `nix run .#rekey` and `nix run .#gen-secrets` before deploy.
{
  den.aspects.slskd = {
    includes = [
      den.aspects.agenix-rekey
      den.aspects.user-pwd
      den.aspects.gateway
      den.aspects.lidarr
      # /mnt/hdd/Music, the `music` user and lidarr itself live in media.nix,
      # which the secondpc host aspect carries; secondpc-web owns the wildcard
      # `ivymect.in` cert the vhost below rides on.
      den.aspects.secondpc-web
    ];

    nixos =
      {
        config,
        pkgs,
        lib,
        scoped,
        ...
      }:
      let
        downloads = "/mnt/hdd/Music/Downloads";
        # The container can only name the account numerically, hence the pinned
        # uid/gid in media.nix.
        mediaId = "${toString config.users.users.media.uid}:${toString config.users.groups.media.gid}";
      in
      {
        imports = [ inputs.arion.nixosModules.arion ];

        services.slskd = {
          enable = true;
          openFirewall = true;
          user = "media";
          group = "media";
          settings = {
            shares.directories = [ "/mnt/hdd/Music" ];
            directories.downloads = downloads;
            web.ip_address = "0.0.0.0";
            web.logging = true;
          };
        };
        systemd.services.slskd.serviceConfig.UMask = "0002";

        # --- soularr container (arion/docker), replaces the old compose stack ---
        virtualisation.arion = {
          backend = "docker";
          projects.soularr = {
            serviceName = "soularr";
            settings.services.soularr.service = {
              image = "mrusse08/soularr:latest";
              container_name = "soularr";
              hostname = "soularr";
              user = mediaId;
              environment = {
                TZ = "Australia/Melbourne";
                SCRIPT_INTERVAL = 300;
              };
              volumes = [
                "${downloads}:/downloads"
                "/var/lib/soularr:/data"
              ];
              # Host networking is what lets config.ini below talk to lidarr and
              # slskd over loopback instead of back out through nginx.
              network_mode = "host";
              restart = "unless-stopped";
            };
          };
        };

        # --- slskd (soulseek): its env is generated from ivy-password +
        #     a generated API key + the slsk_creds source env. ---
        age.secrets.slskd_secrets_env = {
          rekeyFile = ./slsk_creds.env.age;
          intermediary = true;
        };
        age.secrets.slskd_soularr_apikey.generator.script =
          {
            pkgs,
            lib,
            ...
          }:
          ''
            ${lib.getExe pkgs.openssl} rand -base64 48
          '';
        age.secrets."slskd.env" = {
          owner = "media";
          group = "media";
          restartUnits = [ "slskd.service" ];
          generator = {
            dependencies = {
              inherit (config.age.secrets) slskd_soularr_apikey slskd_secrets_env;
              inherit (scoped.user-pwd.secrets) ivy-password;
            };
            script =
              {
                pkgs,
                lib,
                decrypt,
                deps,
                ...
              }:
              ''
                printf 'SLSKD_API_KEY="role=Administrator;cidr=0.0.0.0/0,::/0;%s"\n' $(${decrypt} ${lib.escapeShellArg deps.slskd_soularr_apikey.file})
                printf 'SLSKD_USERNAME=ivy\n'
                printf 'SLSKD_PASSWORD=%s\n' $(${decrypt} ${lib.escapeShellArg deps.ivy-password.file})
                ${decrypt} ${lib.escapeShellArg deps.slskd_secrets_env.file}
              '';
          };
        };
        services.slskd = {
          environmentFile = config.age.secrets."slskd.env".path;
          domain = "slsk.ivymect.in";
          nginx = {
            forceSSL = true;
            useACMEHost = "ivymect.in";
          };
        };

        # --- soularr: lidarr<->slskd bridge. config.ini is an agenix template
        #     with the lidarr + slskd api keys injected as placeholders. ---
        #
        # soularr is a caller of lidarr's API, so it presents its own service
        # account's key rather than lidarr's real one, and reaches lidarr through
        # nginx so the access log attributes the calls to it. One key per
        # principal, so this is the same credential it would use anywhere else.
        gateway.serviceAccounts.soularr.description = "slskd <-> lidarr bridge";
        age.templates.soularr = {
          dependencies = {
            lidar_key = config.age.secrets."soularr/api-key";
            slskd_api_key = config.age.secrets.slskd_soularr_apikey;
          };
          content =
            {
              placeholders,
              pkgs,
              ...
            }:
            pkgs.lib.generators.toINI { } {
              Lidarr = {
                api_key = placeholders.lidar_key;
                host_url = "https://lidarr.${config.gateway.domain}";
                download_dir = downloads;
                disable_sync = "False";
              };
              Slskd = {
                api_key = placeholders.slskd_api_key;
                host_url = "http://127.0.0.1:${toString config.services.slskd.settings.web.port}";
                url_base = "/";
                download_dir = downloads;
                delete_searches = "False";
                stalled_timeout = 3600;
              };
              "Release Settings" = {
                use_most_common_tracknum = "True";
                allow_multi_disc = "True";
                accepted_countries = "Europe,Japan,United Kingdom,United States,[Worldwide],Australia,Canada";
                accepted_formats = "CD,Digital Media,Vinyl";
              };
              "Search Settings" = {
                search_timeout = 5000;
                maximum_peer_queue = 50;
                minimum_peer_upload_speed = 0;
                minimum_filename_match_ratio = 0.8;
                allowed_filetypes = "flac 24/192,flac 16/44.1,flac,mp3 320,mp3";
                search_for_tracks = "True";
                album_prepend_artist = "False";
                track_prepend_artist = "True";
                search_type = "incrementing_page";
                number_of_albums_to_grab = 10;
                remove_wanted_on_failure = "False";
                title_blacklist = "Word1,word2";
                search_source = "missing";
              };
              Logging = {
                level = "INFO";
                format = "[%(levelname)s|%(module)s|L%(lineno)d] %(asctime)s: %(message)s";
                datefmt = "%Y-%m-%dT%H:%M:%S%z";
              };
            };
          restartUnits = [ "soularr.service" ];
          symlink = false;
          owner = "media";
          group = "media";
          path = "/var/lib/soularr/config.ini";
        };

        systemd.tmpfiles.settings.soularr."/var/lib/soularr"."d" = {
          user = "media";
          group = "media";
          mode = "0770";
        };
      };
  };
}
