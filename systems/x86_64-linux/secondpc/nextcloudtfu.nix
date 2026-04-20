{ config, pkgs, ... }:

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
  age.templates."minio/credentials-file" = {
    dependencies = {
      root-password = "minio/root-password";
    };
    content =
      { placeholders, pkgs, ... }:
      ''
        	MINIO_ROOT_USER="root"
            MINIO_ROOT_PASSWORD=${placeholders.root-password}
        	'';
  };
  #  containers.webserver = rec {
  #    autoStart = true;
  #    privateNetwork = true;
  #    hostAddress = "192.168.100.10";
  #    localAddress = "192.168.100.11";
  #    hostAddress6 = "fc00::1";
  #    localAddress6 = "fc00::2";
  #    config =
  #      {
  #        config,
  #        pkgs,
  #        lib,
  #        ...
  #      }:
  #      {
  #        services.nextcloud = {
  #          config.objectstore.s3 = {
  #            enable = true;
  #            bucket = "nextcloud";
  #            autocreate = true;
  #            key = accessKey;
  #            secretFile = "${pkgs.writeText "secret" "test12345"}";
  #            hostname = hostAddress;
  #            useSsl = false;
  #            port = 9000;
  #            usePathStyle = true;
  #            region = "us-east-1";
  #          };
  #        };
  #      };
  #  };
  services.minio = {
    enable = true;
    dataDir = [ "/mnt/hdd/minio" ];
    listenAddress = "0.0.0.0:9000";
    consoleAddress = "127.0.0.1:9001";
    rootCredentialsFile = config.age.templates."minio/credentials-file".path;
  };

}
