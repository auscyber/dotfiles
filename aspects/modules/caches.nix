{
  inputs,
  den,
  lib,
  ...
}:
let
  caches = {
    "https://nix-community.cachix.org" =
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
    #    "https://iohk.cachix.org" = "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=";
    "https://cache.nixos.org" = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
    "https://devenv.cachix.org" = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    #    "https://auscyber.cachix.org" =
    #      "auscyber.cachix.org-1:RPlENxXc/irvLimM0Yz8Au3ntk/sxZ8bwXPwuXL3v5c=";
    #    "https://cache.ivymect.in/main" = "main:4PgSIjmT7n9adSn4hDnnKXoERhCZR1dTlvj74k+6vT0=";
    ##    "https://attic.xuyh0120.win/lantian" = "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc=";

  };
  vhost = "cache.ivymect.in";

in
{

  den.aspects.celler =
    {
      lib,
      config,
      inputs',
      ...
    }:
    let
      port = 8069;
    in
    {
      overlays = {
        celler = lib.optional (inputs ? celler) inputs.celler.overlays.default;
      };

      vhosts."cache.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:${port}";
        };
        extraConfig = ''
          	client_max_body_size 15g;
          	'';
      };
      nixos = lib.mkIf (inputs' ? celler) {
        services.cellerd = {
          enable = true;
          environmentFile = config.templates."celler_env".path;
          useFlakeCompatOverlay = false;
          settings = {
            listen = "[::]:${port}";
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

        secrets.cache_key.rekeyFile = ./cache.age;
        templates.celler_env = {
          dependencies.cache_key = config.secrets.cache_key;
          content =
            { placeholders, ... }:
            ''
              	CELLER_SERVER_TOKEN_RS256_SECRET_BASE64=${placeholders.cache_key}
              	'';

        };
      };
      den.aspects.nix = {
        provides.secondpc = { config, ... }: {

          includes = [
            den.aspects.nginx
            den.aspects.celler
          ];

          nixos = {

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
                urls = builtins.attrNames caches;
                publicKeys = builtins.attrValues caches;
              };
            };
            server.addr = "0.0.0.0:8501";

            prometheus.enable = true;
          };

        };
      };
    };
  ff.celler = {
    url = "github:blitz/celler/main";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake-file.nixConfig = {
    extra-substituters = builtins.attrNames caches;
    extra-trusted-public-keys = builtins.attrValues caches;
  };

}
