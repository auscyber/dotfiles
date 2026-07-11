{
  inputs,
  den,
  self,
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

  # Mint a scoped celler JWT with an agenix-rekey generator, replacing the
  # runtime `celleradm` systemd wrapper. The RS256 signing key is handed in as
  # the `cache_key` dependency and decrypted with the master identity at
  # `agenix generate` time, so the private key never leaves the admin host --
  # only the resulting per-host token is rekeyed onto the target.
  #
  # `celleradm make-token` reads only the [jwt] block; database/storage/chunking
  # merely have to parse, so they are stubbed. Empty [jwt] matches the server
  # (no bound issuer/audience).
  cellerTokenScript =
    {
      sub,
      pull ? [ "main" ],
      push ? [ "main" ],
      validity ? "10y",
    }:
    {
      pkgs,
      lib,
      decrypt,
      deps,
      ...
    }:
    let
      tokenConfig =
        (pkgs.formats.toml { }).generate "celler-token.toml"
          self.nixosConfigurations.secondpc.config.services.cellerd.settings;
      patternArgs = flag: lib.concatMapStringsSep " " (p: "${flag} ${lib.escapeShellArg p}");
    in
    ''
      export CELLER_SERVER_TOKEN_RS256_SECRET_BASE64="$(${decrypt} ${lib.escapeShellArg (lib.head deps).file})"
      ${lib.getExe' pkgs.celler "celleradm"} -f ${tokenConfig} make-token \
        --sub ${lib.escapeShellArg sub} \
        --validity ${lib.escapeShellArg validity} \
        ${patternArgs "--pull" pull} \
        ${patternArgs "--push" push}
    '';

in
{

  debug = true;
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

      vhosts.${vhost} = {
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
            environmentFile = "${config.age.templates."celler_env".path}";
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

          age.secrets.cache_keyy.rekeyFile = ./cache.age;
          age.templates.celler_env = {
            dependencies.cache_key = config.age.secrets.cache_keyy;
            content =
              { placeholders, ... }:
              ''
                CELLER_SERVER_TOKEN_RS256_SECRET_BASE64=${placeholders.cache_key}
              '';
          };
        };
    };

  # Opt-in per-host cache credentials. A host that includes this aspect gets its
  # own scoped push/pull token for cache.ivymect.in, generated from the shared
  # signing key. The key is declared here as an *intermediary* secret: present
  # in the repo for the generator, but never rekeyed/deployed onto the host --
  # only the minted `celler_token` lands there.
  den.aspects.celler-push = {
    includes = [
      den.aspects.agenix-rekey
    ];
    overlays.celler = lib.optional (inputs ? celler) inputs.celler.overlays.default;

    secrets =
      { secrets, host, ... }:
      {

        cache_key = {
          rekeyFile = ./cache.age;
          intermediary = lib.mkDefault true;
        };
        celler_token.generator = {
          tags = [ "celler_token" ];
          dependencies = [ secrets.cache_key ];
          script = cellerTokenScript { sub = host.name; };
        };
      };
    templates =
      { secrets, ... }:
      {
        netrc = {
          dependencies = {
            inherit (secrets) celler_token;
          };
          content =
            {
              pkgs,
              placeholders,
              ...
            }:
            ''
                      machine ${vhost}
              		password ${placeholders.celler_token}
              	  '';
        };
      };

    os =
      {
        config,
        pkgs,
        host,
        ...
      }:
      let
        build-hook = pkgs.writeTextFile {
          name = "build-hook";
          executable = true;
          destination = "/bin/build-hook";
          text =
            # sh
            ''
              #!/bin/sh
              set -eu
              set -f # disable globbing
              export IFS=' '
              export PATH="$PATH:/nix/var/nix/profiles/default/bin:${pkgs.celler}/bin:${pkgs.ts}/bin"
              celler login central https://${vhost} "$(cat ${config.age.secrets.celler_token.path})"

              echo "Uploading paths" $OUT_PATHS
              if [[ -n "''${OUT_PATHS:-}" ]]; then
              	export TS_MAXFINISHED=1000
              	export TS_SLOTS=10

              	echo "Uploading $OUT_PATHS"
              	printf "%s" "$OUT_PATHS" |
              		xargs ts celler push main
              fi

            '';

          meta.mainProgram = "build-hook";
        };
      in

      {
        nix.settings = {
          post-build-hook = "${build-hook}";
          netrc-file = config.age.templates.netrc.path;
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
