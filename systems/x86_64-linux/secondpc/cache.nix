{ config, pkgs, ... }:
{

  age.secrets."attic_env" = {
    rekeyFile = ./attic_env.age;
    generator = {
      script =
        { pkgs, ... }:
        ''
          printf 'ATTIC_SERVER_TOKEN_RS256_SECRET_BASE64=%s\n' "$(${pkgs.openssl}/bin/openssl genrsa -traditional 4096 | base64 -w0)"
        '';

    };

  };
  services.nginx.virtualHosts."cache.ivymect.in" = {
    useACMEHost = "ivymect.in";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:8069";
    };
    extraConfig = ''
      	client_max_body_size 500m;
      	'';
  };

  services.atticd = {
    enable = true;
    environmentFile = config.age.secrets."attic_env".path;
    settings = {
      listen = "[::]:8069";
      storage = {
        type = "local";
        path = "/mnt/hdd/attic";

      };

      jwt = { };

      # Data chunking
      #
      # Warning: If you change any of the values here, it will be
      # difficult to reuse existing chunks for newly-uploaded NARs
      # since the cutpoints will be different. As a result, the
      # deduplication ratio will suffer for a while after the change.
      chunking = {
        # The minimum NAR size to trigger chunking
        #
        # If 0, chunking is disabled entirely for newly-uploaded NARs.
        # If 1, all NARs are chunked.
        nar-size-threshold = 64 * 1024; # 64 KiB

        # The preferred minimum size of a chunk, in bytes
        min-size = 16 * 1024; # 16 KiB

        # The preferred average size of a chunk, in bytes
        avg-size = 64 * 1024; # 64 KiB

        # The preferred maximum size of a chunk, in bytes
        max-size = 256 * 1024; # 256 KiB
      };
    };

  };
  services.ncps = {
    enable = true;
    cache = {
      hostName = "secondpc";
      storage.local = "/mnt/hdd/ncps";
      maxSize = "200G";
      lru.schedule = "0 2 * * *"; # Clean up daily at 2 AM
      #      allowPutVerb = true;
      #      allowDeleteVerb = true;
      upstream = {
        urls = [
          "https://nix-community.cachix.org"
          "https://iohk.cachix.org"
          "https://cache.nixos.org"
          "https://devenv.cachix.org"
          "https://auscyber.cachix.org"
          "https://cache.ivymect.in/main"
        ];
        publicKeys = [
          "main:4PgSIjmT7n9adSn4hDnnKXoERhCZR1dTlvj74k+6vT0="
          "auscyber.cachix.org-1:RPlENxXc/irvLimM0Yz8Au3ntk/sxZ8bwXPwuXL3v5c="
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
          "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        ];
      };
    };
    server.addr = "0.0.0.0:8501";

    prometheus.enable = true;
  };

}
