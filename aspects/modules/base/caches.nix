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
    "https://cache.ivymect.in/main" = "main:4PgSIjmT7n9adSn4hDnnKXoERhCZR1dTlvj74k+6vT0=";
    #    "https://attic.xuyh0120.win/lantian" = "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc=";

  };
  vhost = "cache.ivymect.in";

in
{

  den.aspects.nix.nix.settings = {
    trusted-substituters = builtins.attrNames caches;
    trusted-public-keys = builtins.attrValues caches;

  };

  # secondpc runs the ncps binary cache (served directly on :8501) plus celler
  # (the attic-style cache behind cache.ivymect.in). Attached to the `nix`
  # aspect's per-host provider so they only land on secondpc.
  den.aspects.nix.provides.secondpc = {
    includes = [
      den.aspects.nginx
      den.aspects.celler
    ];
    nixos.services.ncps = {
      enable = true;
      cache = {
        hostName = "secondpc";
        storage.local = "/mnt/hdd/ncps";
        maxSize = "200G";
        lru.schedule = "0 2 * * *"; # Clean up daily at 2 AM
        upstream = {
          urls = builtins.attrNames caches;
          publicKeys = builtins.attrValues caches;
        };
      };
      server.addr = "0.0.0.0:8501";
      prometheus.enable = true;
    };
  };

  den.aspects.celler =
    let
      port = 8069;
    in
    {
      overlays.celler = lib.optional (inputs ? celler) inputs.celler.overlays.default;

      vhosts."cache.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString port}";
        };
        extraConfig = ''
          client_max_body_size 15g;
        '';
      };

      nixos =
        { config, ... }:
        {
          imports = [ inputs.celler.nixosModules.cellerd ];

          services.cellerd = {
            enable = true;
            environmentFile = config.age.templates."celler_env".path;
            useFlakeCompatOverlay = false;
            settings = {
              listen = "[::]:${toString port}";
              storage = {
                type = "local";
                path = "/mnt/hdd/attic";
              };

              jwt = { };

              # Data chunking. Changing these makes existing chunks unreusable
              # (different cutpoints), hurting dedup until re-uploaded.
              chunking = {
                nar-size-threshold = 64 * 1024; # 64 KiB
                min-size = 16 * 1024; # 16 KiB
                avg-size = 64 * 1024; # 64 KiB
                max-size = 256 * 1024; # 256 KiB
              };
            };
          };

          age.secrets.cache_key.rekeyFile = ./cache.age;
          age.templates.celler_env = {
            dependencies.cache_key = config.age.secrets.cache_key;
            content =
              { placeholders, ... }:
              ''
                CELLER_SERVER_TOKEN_RS256_SECRET_BASE64=${placeholders.cache_key}
              '';
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
