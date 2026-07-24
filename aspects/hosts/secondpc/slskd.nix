{
  den,
  inputs,
  ...
}:
# slskd (soulseek daemon) + its soularr (lidarr<->slskd) bridge, split out of the
# secondpc media stack into a standalone aspect. Nothing includes this — add
# `den.aspects.slskd` to a host's includes to run it. Secrets here (slskd.env,
# slskd_soularr_apikey, slskd_secrets_env, lidar_key) still need `nix run .#rekey`
# and `nix run .#gen-secrets` on whichever host picks it up.
{
  den.aspects.slskd = {
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

        services.slskd = {
          enable = true;
          openFirewall = true;
          user = "music";
          settings = {
            shares.directories = [ "/mnt/hdd/Music" ];
            directories.downloads = "/mnt/hdd/Music/Downloads";
            web.ip_address = "0.0.0.0";
            web.logging = true;
          };
        };

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

        systemd.tmpfiles.settings.soularr."/var/lib/soularr"."d" = {
          user = "1000";
          group = "1000";
          mode = "0770";
        };
      };
  };
}
