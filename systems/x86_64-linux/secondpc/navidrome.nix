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

  };
  age.secrets.soularr = {
    rekeyFile = ./soularr.age;
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
        networks."main" = {
          ipam = {
            driver = "default";
            config = [ { subnet = "172.16.238.0/24"; } ];
          };
        };

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
            networks = [ "main" ];
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
      #      web.authentication.api_keys = {
      #        my_api_key = "soulseekpasswordddd";
      #        cidr = "";
      #      };
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
