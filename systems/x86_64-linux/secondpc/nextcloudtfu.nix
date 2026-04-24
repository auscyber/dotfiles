{ config, pkgs, ... }:

let
  accessKey = "nextcloud";
  hostName = "tfu.ivymect.in";
in

{
  age.secrets."minio/root-password" = {
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
  age.secrets."nextcloud/minio-password" = {
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
  age.templates."minio/credentials-file" = {
    dependencies = {
      root-password = config.age.secrets."minio/root-password";
    };
    content =
      { placeholders, pkgs, ... }:
      ''
        	MINIO_ROOT_USER="root"
            MINIO_ROOT_PASSWORD=${placeholders.root-password}
        	'';
  };

  containers.webserver = rec {
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.10";
    localAddress = "192.168.100.11";
    hostAddress6 = "fc00::1";
    localAddress6 = "fc00::2";
    config =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        services.nextcloud = {
          enable = true;
          inherit hostName;
          datadir = "/mnt/hdd/tfu-nextcloud-file";
          config.dbtype = "psql";
          config.objectstore.s3 = {
            enable = true;
            bucket = "nextcloud";
            autocreate = true;
            key = accessKey;
            secretFile = config.age.secrets."nextcloud/minio_password";
            hostname = hostAddress;
            useSsl = false;
            port = 9000;
            usePathStyle = true;
            region = "us-east-1";
          };
        };
      };
  };
  services.nginx.virtualHosts."${hostName}".locations."/".proxyPass =
    "http://${config.containers.webserver.localAddress}";
  services.minio = {
    enable = true;
    dataDir = [ "/mnt/hdd/minio" ];
    listenAddress = "0.0.0.0:9000";
    consoleAddress = "127.0.0.1:9001";
    rootCredentialsFile = config.age.templates."minio/credentials-file".path;
  };

}
