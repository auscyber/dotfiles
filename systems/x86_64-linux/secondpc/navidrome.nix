{ config, pkgs, ... }:
let
  host = "music.ivymect.in";
  path = "/mnt/hdd/Music";
in
{
  age.secrets.slskd_secrets_env = {
    rekeyFile = ../../../secrets/slsk_creds.env.age;

    intermediary = true;
  };
  age.secrets.navidrome_env = {
    rekeyFile = ./navidrome.age;

  };
  systemd.tmpfiles.settings.music = {
    "/mnt/hdd/Music/Downloads"."d" = {
      user = ":music";
      group = ":music";
      mode = ":770";

    };
  };

  age.secrets."slskd.env" = {
    owner = "music";
    generator = {
      dependencies = {
        inherit (config.age.secrets) ivy-password slskd_secrets_env;
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
    };
    domain = "slsk.ivymect.in";
    nginx = {

	forceSSL = true;
      useACMEHost = "ivymect.in";

    };

  };

  services.nginx.virtualHosts = {
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
