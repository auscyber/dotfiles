{ config, pkgs, ... }:
let
  host = "music.ivymect.in";
  path = "/mnt/hdd/Music";
in
{
  services.audiobookshelf = {
    enable = true;

  };
  services.samba = {
    enable = true;
    securityType = "user";
    openFirewall = true;
    settings = {
      global = {
        "workgroup" = "WORKGROUP";
        "server string" = "smbnix";
        "netbios name" = "smbnix";
        "security" = "user";
        #"use sendfile" = "yes";
        #"max protocol" = "smb2";
        # note: localhost is the ipv6 localhost ::1
        "hosts allow" = "192.168.0. 127.0.0.1 localhost 100.64.0.";
        "hosts deny" = "0.0.0.0/0";
        "guest account" = "nobody";
        "map to guest" = "bad user";
      };
      public = {
        browseable = "yes";
        comment = "Public samba share.";
        "guest ok" = "yes";
        path = "/srv/public";
        "read only" = "yes";
      };

      "hdd" = {
        "path" = "/mnt/hdd";
        "browseable" = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        #      "force user" = "username";
        "force group" = "music";
      };

    };

  };
  age.secrets.slskd_secrets_env = {
    rekeyFile = ../../../secrets/slsk_creds.env.age;

    intermediary = true;
  };
  age.secrets.navidrome_env = {
    rekeyFile = ./navidrome.age;
	restartUnits = ["navidrome.service"];

  };
  age.secrets.lidar_key = {
    rekeyFile = ./lidar_key.age;
  };
  age.secrets.soularr = {
    rekeyFile = ./soularr.age;
    generator = {
      dependencies = {
        soularr_api_key = config.age.secrets.lidar_key;
      };
      value =
        { placeholders }:
        {
          Lidarr = {
            # Get from Lidarr: Settings > General > Security
            api_key = placeholders.soularr_api_key;
            # URL Lidarr uses (e.g., what you use in your browser)
            host_url = "https://lidarr.ivymect.in";
            # Path to slskd downloads inside the Lidarr container
            download_dir = "/mnt/hdd/Music/Downloads";
            # If true, Lidarr won't auto-import from Slskd
            disable_sync = "False";
          };

          Slskd = {
            # Create manually (see docs)
            api_key = "soulseekpasswordddd";
            # URL Slskd uses
            host_url = "http://127.0.0.1:5030";
            url_base = "/";
            # Download path inside Slskd container
            download_dir = "/mnt/hdd/Music/Downloads";
            # Delete search after Soularr runs
            delete_searches = "False";
            # Max seconds to wait for downloads (prevents infinite hangs)
            stalled_timeout = 3600;
          };

          "Release Settings" = {
            # Pick release with most common track count
            use_most_common_tracknum = "True";
            allow_multi_disc = "True";
            # Accepted release countries
            accepted_countries = "Europe,Japan,United Kingdom,United States,[Worldwide],Australia,Canada";
            # Accepted formats
            accepted_formats = "CD,Digital Media,Vinyl";
          };
          "Search Settings" = {
            search_timeout = 5000;
            maximum_peer_queue = 50;
            # Minimum upload speed (bits/sec)
            minimum_peer_upload_speed = 0;
            # Minimum match ratio between Lidarr track and Soulseek filename
            minimum_filename_match_ratio = 0.8;
            # Preferred file types and qualities (most to least preferred)
            # Use "flac" or "mp3" to ignore quality details
            allowed_filetypes = "flac 24/192,flac 16/44.1,flac,mp3 320,mp3";
            #ignored_users = User1,User2,Fred,Bob
            # Set to False to only search for album titles (Note Soularr does not search for individual tracks, this setting searches for track titles but still tries to match to the full album).
            search_for_tracks = "True";
            # Prepend artist name when searching for albums
            album_prepend_artist = "False";
            track_prepend_artist = "True";
            # Search modes: all, incrementing_page, first_page
            # "all": search for every wanted record, "first_page": repeatedly searchs the first page, "incrementing_page": starts with the first page and increments on each run.
            search_type = "incrementing_page";
            # Albums to process per run
            number_of_albums_to_grab = 10;
            # Unmonitor album on failure; logs to failure_list.txt
            remove_wanted_on_failure = "False";
            # Blacklist words in album or track titles (case-insensitive)
            title_blacklist = "Word1,word2";
            # Lidarr search source: "missing" or "cutoff_unmet"
            search_source = "missing";
          };

          Logging = {
            # Passed to Python's logging.basicConfig()
            # See: https://docs.python.org/3/library/logging.html
            level = "INFO";
            format = "[%(levelname)s|%(module)s|L%(lineno)d] %(asctime)s: %(message)s";
            datefmt = "%Y-%m-%dT%H:%M:%S%z";

          };
        };
      script = config.age.generators.toINI;
    };
	restartUnits = ["soularr.service"];
    symlink = false;
    owner = "1000";
    group = "1000";
    path = "/var/lib/soularr/config.ini";

  };
  age.secrets."soularr_api_key" = {

    generator = {
      script =
        {
          pkgs,
          lib,
          ...
        }:
        ''
          		${lib.getExe pkgs.openssl} rand -base64 48
          		'';
    };
  };

  systemd.tmpfiles.settings.music = {
    "/mnt/hdd/AudioBooks"."d" = {
      user = "audiobookshelf";
      group = ":music";
      mode = ":770";

    };
    "/mnt/hdd/Music/Downloads"."d" = {
      user = ":music";
      group = ":music";
      mode = ":770";

    };
    "/var/lib/soularr"."d" = {
      user = "1000";
      group = "1000";
      mode = ":770";

    };
  };
  virtualisation.arion = {
    backend = "docker"; # or "docker"
    projects.soularr = {
      serviceName = "soularr"; # optional systemd service name, defaults to arion-example in this case

      settings = {

	  networks.main.ipam = {
	  driver= "default";
	  config = [{subnet= "172.28.0.0/24";}];
	  };






#        networks."main".ipam = {
#          driver = "default";
#          config = [ { subnet = "172.28.0.0/24"; gateway = "172.28.0.1"; } ];
#        };

        services.soularr = {
          service = {
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
        # Specify you project here, or import it from a file.
        # NOTE: This does NOT use ./arion-pkgs.nix, but defaults to NixOS' pkgs.
      };
    };
  };

  age.secrets."slskd.env" = {
    owner = "music";
	restartUnits = ["slskd.service"];
    generator = {
      dependencies = {
        inherit (config.age.secrets) ivy-password slskd_secrets_env soularr_api_key;
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
          						printf 'SLSKD_API_KEY="role=Administrator;cidr=0.0.0.0.0/0,::/0;%s"\n' $(${decrypt} ${lib.escapeShellArg deps.soularr_api_key.file})
                              	printf 'SLSKD_USERNAME=ivy\n'
                              	printf 'SLSKD_PASSWORD=%s\n' $(${decrypt} ${lib.escapeShellArg deps.ivy-password.file})
                    			${decrypt} ${lib.escapeShellArg deps.slskd_secrets_env.file}
                              	'';

    };

  };
  users.groups.music = {
  };
  users.users.music = {
    isSystemUser = true;
    group = "music";

  };

  services.navidrome = {
    enable = true;
    environmentFile = config.age.secrets.navidrome_env.path;
    group = "music";
    settings = {
      user = "music";
      MusicFolder = "${path}";
    };
  };
  services.lidarr = {
    enable = true;
    user = "music";
  };

  services.slskd = {
    enable = true;
    openFirewall = true;
    user = "music";
    environmentFile = config.age.secrets."slskd.env".path;
    settings = {
      shares.directories = [ "${path}" ];
      directories.downloads = "/mnt/hdd/Music/Downloads";

      web.authentication.api_keys.lol = {
        key = "soulseekpasswordddd";
        cidr = "127.0.0.0/24";
        role = "Administrator";
      };
	  web.ip_address = "0.0.0.0";
      web.logging = true;
    };
    domain = "slsk.ivymect.in";
    nginx = {

      forceSSL = true;
      useACMEHost = "ivymect.in";

    };

  };

  services.nginx.virtualHosts = {
    "audiobookshelf.ivymect.in" = {
      useACMEHost = "ivymect.in";
      forceSSL = true;
      locations."/" = {

        proxyWebsockets = true;
        proxyPass = "http://127.0.0.1:${builtins.toString config.services.audiobookshelf.port}";
      };

    };
    "lidarr.ivymect.in" = {
      useACMEHost = "ivymect.in";
      forceSSL = true;

      locations."/" = {

        proxyPass = "http://127.0.0.1:${builtins.toString config.services.lidarr.settings.server.port}";
      };
    };

    "${host}" = {
      useACMEHost = "ivymect.in";
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://${config.services.navidrome.settings.Address}:${builtins.toString config.services.navidrome.settings.Port}";
      };

    };
  };

}
