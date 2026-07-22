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
    #    "https://attic.xuyh0120.win/lantian" = "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc=";
  }
  // cellerCaches;
  vhost = "cache.ivymect.in";

  # Ed25519 signing keys for the celler caches on `vhost`. The server mints the
  # keypair when the cache is created (`celler cache create`) and only ever
  # hands out the public half, so unlike the JWT token below there is nothing to
  # derive offline -- it has to be read back off the server. `nix run
  # .#update-celler-keys` does that via `celler cache info` and commits the
  # result to celler-keys.json; this import is what feeds
  # trusted-public-keys/nixConfig, so a stale file means substitution from
  # cache.ivymect.in fails signature verification.
  cellerPublicKeys = lib.importJSON ./celler-keys.json;
  cellerCaches = lib.mapAttrs' (
    name: key: lib.nameValuePair "https://${vhost}/${name}" key
  ) cellerPublicKeys;

  # Mint a scoped celler JWT with an agenix-rekey generator, replacing the
  # runtime `celleradm` systemd wrapper. The RS256 signing key is handed in as
  # the `cache_key` dependency and decrypted with the master identity at
  # `agenix generate` time, so the private key never leaves the admin host --
  # only the resulting per-host token is rekeyed onto the target.
  #
  # `celleradm make-token` reads only the [jwt] block; database/storage/chunking
  # merely have to parse. Empty [jwt] matches the server (no bound
  # issuer/audience).
  #
  # Settings come from evaluating the `celler` aspect's own nixos content against
  # the upstream cellerd module in a throwaway fixpoint, so the token config
  # tracks the aspect directly instead of dragging a whole host evaluation in for
  # five lines of settings. `_module.check = false` lets the aspect's `age.*`
  # definitions (and cellerd's own systemd/assertions/nixpkgs output) sit
  # unmatched -- nothing forces them, we only pull `services.cellerd.settings`.
  cellerdSettings =
    pkgs:
    (lib.evalModules {
      modules = [
        inputs.celler.nixosModules.cellerd
        {
          _module.args = { inherit pkgs; };
          _module.check = false;
        }
      ]
      ++ den.lib.aspects.fx.contentUtil.unwrapContentValuesList den.aspects.celler.nixos;
    }).config.services.cellerd.settings;

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
      tokenConfig = (pkgs.formats.toml { }).generate "celler-token.toml" (cellerdSettings pkgs);
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
  den.default.nix.settings = {
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

      nixos = { config, ... }: {
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
          content = { placeholders, ... }: ''
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
    homeManager =
      {
        pkgs,
        config,
        ...
      }:
      {
        age.templates."celler_config" = {
          path = "${config.home.homeDirectory}/.config/celler/config.toml";
          dependencies = {
            inherit (config.age.secrets) celler_token;
          };
          content =
            {
              pkgs,
              placeholders,
              ...
            }:
            ''
              default-server = "central"
              [servers.central]
              endpoint = "https://${vhost}"
              token = "${placeholders.celler_token}"
            '';
        };
        home.packages = [ pkgs.celler ];
      };

    secrets =
      {
        secrets,
        host,
        ...
      }:
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
      {
        secrets,
        config,
        ...
      }:
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
          post-build-hook = "${lib.getExe build-hook}";
          netrc-file = config.age.templates.netrc.path;
        };
      };
  };

  # `nix run .#update-celler-keys [cache...]` -- ask the server for each cache's
  # public signing key and refresh celler-keys.json. Defaults to the caches
  # already in the file. Uses the caller's own `~/.config/celler/config.toml`
  # (written by the celler-push aspect), so any host with a pull token can run
  # it: `cache-config` only requires `pull`. Merges rather than replaces, so
  # naming one cache never drops the others.
  perSystem =
    { pkgs, ... }:
    let
      updateCellerKeys = pkgs.writeShellApplication {
        name = "update-celler-keys";
        runtimeInputs = [
          pkgs.celler
          pkgs.jq
          pkgs.gnused
          pkgs.coreutils
        ];
        text = ''
          set -eu
          dest="aspects/modules/base/celler-keys.json"
          if [ ! -e flake.nix ]; then
          	echo "update-celler-keys: run from the repo root (flake.nix not found)" >&2
          	exit 1
          fi

          tmp="$(mktemp)"
          trap 'rm -f "$tmp"' EXIT
          if [ -e "$dest" ]; then cp "$dest" "$tmp"; else echo '{}' >"$tmp"; fi

          # No arguments: refresh every cache already tracked in the file.
          if [ "$#" -eq 0 ]; then
          	# shellcheck disable=SC2046 # cache names are never whitespace-y
          	set -- $(jq -r 'keys[]' "$tmp")
          fi
          if [ "$#" -eq 0 ]; then
          	echo "update-celler-keys: no caches tracked in $dest; pass names explicitly" >&2
          	exit 1
          fi

          for cache in "$@"; do
          	# `celler cache info` reports on stderr; the key line is
          	# "           Public Key: <name>:<base64>".
          	key="$(celler cache info "$cache" 2>&1 | sed -n 's/^ *Public Key: *//p')"
          	if [ -z "$key" ]; then
          		echo "update-celler-keys: no public key for cache '$cache' (not found, or token lacks pull)" >&2
          		exit 1
          	fi
          	echo "$cache -> $key"
          	jq --arg n "$cache" --arg k "$key" '.[$n] = $k' "$tmp" >"$tmp.next"
          	mv "$tmp.next" "$tmp"
          done

          cp "$tmp" "$dest"
          echo "Wrote $dest"
        '';
      };
    in
    {
      devshells.default.packages = [ updateCellerKeys ];
      packages.update-celler-keys = updateCellerKeys;
      apps.update-celler-keys = {
        type = "app";
        program = lib.getExe updateCellerKeys;
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
