{ config, pkgs, ... }:
let
  host = "music.ivymect.in";
  path = "/mnt/hdd/Music";
in
{
  age.secrets."slskd.env" = {
    owner = config.services.slskd.user;
    generator = {
      dependencies = {
        inherit (config.age.secrets) ivy-password;
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
          	printf 'SLSKD_SLSK_USERNAME=ivy\n'
          	printf 'SLSKD_USERNAME=ivy\n'
          	printf 'SLSKD_SLSK_PASSWORD=%s\n' $(${decrypt} ${lib.escapeShellArg deps.ivy-password.file})
          	printf 'SLSKD_PASSWORD=%s\n' $(${decrypt} ${lib.escapeShellArg deps.ivy-password.file})
          	'';

    };

  };

  services.navidrome = {
    enable = true;
    settings = {
      MusicFolder = "${path}";
    };
  };
  services.lidarr = {
    enable = true;
  };
  services.slskd = {
    enable = true;
    openFirewall = true;
	environmentFile = config.age.secrets."slskd.env".path;
    settings = {
      shares.directories = [ "${path}" ];

    };
    domain = "slskd.music.ivymect.in";
    nginx = {
      useACMEHost = "ivymect.in";

    };

  };

  services.nginx.virtualHosts = {
    "download.${host}" = {
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
