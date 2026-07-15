{
  den,
  inputs,
  ...
}:
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
        imports = [ inputs.arion.nixosModules.arion ];

        # --- soularr container (arion/docker), replaces the old compose stack ---
        virtualisation.arion = {
          backend = "docker";
          projects.soularr = {
            serviceName = "soularr";
            settings = {
              networks.main.ipam = {
                driver = "default";
                config = [ { subnet = "172.28.0.0/24"; } ];
              };
              services.soularr.service = {
                image = "mrusse08/soularr:latest";
                container_name = "soularr";
                hostname = "soularr";
                user = "1000:1000";
                environment = {
                  TZ = "Australia/Melbourne";
                  SCRIPT_INTERVAL = 300;
                };
                volumes = [
                  "/mnt/hdd/Music/Downloads:/downloads"
                  "/var/lib/soularr:/data"
                ];
                network_mode = "host";
                restart = "unless-stopped";
              };
            };
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
          owner = "music";
          restartUnits = [ "slskd.service" ];
          generator = {
            dependencies = {
              inherit (config.age.secrets) ivy-password slskd_soularr_apikey slskd_secrets_env;
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
        age.secrets.lidar_key.rekeyFile = ./lidar_key.age;
        age.templates.soularr = {
          dependencies = {
            lidar_key = config.age.secrets.lidar_key;
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
                host_url = "https://lidarr.ivymect.in";
                download_dir = "/mnt/hdd/Music/Downloads";
                disable_sync = "False";
              };
              Slskd = {
                api_key = placeholders.slskd_api_key;
                host_url = "http://127.0.0.1:5030";
                url_base = "/";
                download_dir = "/mnt/hdd/Music/Downloads";
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
          owner = "1000";
          group = "1000";
          path = "/var/lib/soularr/config.ini";
        };

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
          "/var/lib/soularr"."d" = {
            user = "1000";
            group = "1000";
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
