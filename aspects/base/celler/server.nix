{
  inputs,
  den,
  lib,
  ...
}:
let
  inherit (den.lib.policy) route;
  celler = import ./_lib.nix { inherit lib; };
  keys = lib.importJSON ./celler-keys.json;
in
{
  # cellerd's `settings`, as a den class. `den.aspects.celler-server` holds what
  # every celler server shares; an aspect that runs one adds its own fragments,
  # all routed into `services.cellerd.settings`.
  den.classes.cellerd = { };

  den.policies.cellerd-to-nixos = _: [
    (route {
      fromClass = "cellerd";
      intoClass = "nixos";
      path = [
        "services"
        "cellerd"
        "settings"
      ];
    })
  ];

  den.quirks.celler-caches.description = "celler servers consumers push to and substitute from";

  den.aspects.celler-server = {
    cellerd = celler.sharedSettings;

    # A NixOS host running a celler server: the service, the shared settings and
    # its own generated RS256 signing key (`signingKey` in ./_lib.nix), at
    # `celler/signing_key`.
    #
    # What it is reachable at is read off the host's own config -- nginx
    # virtualHosts proxying to it, tailscale, a cloudflared tunnel -- and handed
    # to consumers on the `celler-caches` quirk (./push.nix collects it).
    provides.nixos = {
      includes = [
        den.aspects.celler-server
        den.aspects.celler-input
        den.aspects.packages.celler
        den.policies.cellerd-to-nixos
      ];
      secretScope = "celler";

      # Named after the host by where agenix puts its generated secrets, the
      # one thing a `secrets` body can see.
      secrets =
        { age, ... }:
        {
          signing_key = celler.signingKey (baseNameOf age.rekey.generatedSecretsDir);
        };

      # In the `templates` class rather than raw in `nixos`, because only the
      # classes an aspect re-emits get scope-local args: `secrets` here is the
      # `celler` scope, keyed by short name.
      templates.env =
        { secrets, ... }:
        {
          dependencies.signing_key = secrets.signing_key;
          content =
            { placeholders, ... }:
            ''
              CELLER_SERVER_TOKEN_RS256_SECRET_BASE64=${placeholders.signing_key}
            '';
        };

      celler-caches =
        { config, host, ... }:
        let
          k = keys.${host.name} or { };
        in
        {
          name = host.name;
          inherit (config.services.cellerd.expose) endpoint addresses;
          keys = k;
          caches = if k == { } then [ "main" ] else builtins.attrNames k;
        };

      # This server's own caches, into the flake's nixConfig. Read off the
      # host's evaluated config: the quirk's config thunk would resolve against
      # the flake-file evaluation here, not the host's.
      flake-file =
        { host, ... }:
        let
          expose = inputs.self.nixosConfigurations.${host.name}.config.services.cellerd.expose;
          k = keys.${host.name} or { };
        in
        {
          nixConfig = {
            extra-substituters = map (cache: "${expose.endpoint}/${cache}") (builtins.attrNames k);
            extra-trusted-public-keys = builtins.attrValues k;
          };
        };

      nixos =
        {
          config,
          pkgs,
          scoped,
          ...
        }:
        let
          cfg = config.services.cellerd.expose;
          local = [
            "http://localhost:${toString cfg.port}"
            "http://127.0.0.1:${toString cfg.port}"
          ];
          vhosts = lib.filterAttrs (
            _: v: lib.any (l: builtins.elem (l.proxyPass or null) local) (builtins.attrValues v.locations)
          ) config.services.nginx.virtualHosts;
          tailscaleName =
            let
              flag = lib.findFirst (lib.hasPrefix "--hostname=") null config.services.tailscale.extraUpFlags;
            in
            if flag != null then lib.removePrefix "--hostname=" flag else config.networking.hostName;
          tls = v: v.forceSSL || v.onlySSL || v.addSSL;
        in
        {
          imports = [ inputs.celler.nixosModules.cellerd ];

          options.services.cellerd.expose = {
            port = lib.mkOption {
              type = lib.types.port;
              default = 8080;
            };
            cloudflared = {
              hostname = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
                description = "Public hostname of a cloudflared tunnel to http://localhost:<port>.";
              };
              environmentFile = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
                description = "File holding TUNNEL_TOKEN=<token>.";
              };
            };
            addresses = lib.mkOption {
              type = lib.types.attrsOf lib.types.str;
              readOnly = true;
              default =
                lib.optionalAttrs (vhosts != { }) (
                  let
                    name = lib.head (builtins.attrNames vhosts);
                  in
                  {
                    virtualHost = "${if tls vhosts.${name} then "https" else "http"}://${name}";
                  }
                )
                // lib.optionalAttrs (cfg.cloudflared.hostname != null) {
                  cloudflared = "https://${cfg.cloudflared.hostname}";
                }
                // lib.optionalAttrs config.services.tailscale.enable {
                  tailscale = "http://${tailscaleName}:${toString cfg.port}";
                };
            };
            consumeVia = lib.mkOption {
              type = lib.types.enum [
                "virtualHost"
                "cloudflared"
                "tailscale"
              ];
              default = lib.findFirst (a: cfg.addresses ? ${a}) "tailscale" [
                "virtualHost"
                "cloudflared"
                "tailscale"
              ];
              description = "Which exposure consumers reach it by.";
            };
            endpoint = lib.mkOption {
              type = lib.types.str;
              readOnly = true;
              default = cfg.addresses.${cfg.consumeVia};
            };
          };

          config = {
            services.cellerd = {
              enable = true;
              useFlakeCompatOverlay = false;
              environmentFile = scoped.celler.templates.env.path;
              settings = {
                listen = "[::]:${toString cfg.port}";
                api-endpoint = "${cfg.endpoint}/";
              };
            };

            systemd.services.cloudflared-tunnel = lib.mkIf (cfg.cloudflared.hostname != null) {
              wantedBy = [ "multi-user.target" ];
              after = [ "network-online.target" ];
              wants = [ "network-online.target" ];
              serviceConfig = {
                ExecStart = "${lib.getExe pkgs.cloudflared} tunnel --no-autoupdate run";
                EnvironmentFile = cfg.cloudflared.environmentFile;
                DynamicUser = true;
                Restart = "always";
                RestartSec = 5;
              };
            };
          };
        };
    };
  };

  # secondpc's celler, cache.ivymect.in.
  den.aspects.celler =
    let
      port = 8069;
    in
    {
      includes = [ den.aspects.celler-server._.nixos ];

      vhosts."cache.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:${toString port}";
        extraConfig = ''
          client_max_body_size 15g;
        '';
      };
      nixos.services.cellerd.expose.port = port;

      cellerd = {
        storage = {
          type = "local";
          path = "/mnt/hdd/attic";
        };
        tracing.otlp = {
          enabled = true;
          endpoint = "insecure://127.0.0.1:4317";
          protocol = "grpc";
        };
      };

      # CI push token for GitHub Actions (sub=github, push=main).
      # `nix run .#sync-ci-secrets` uploads it as the CELLER_TOKEN secret that
      # auscyber/celler-action pushes with.
      secrets =
        { secrets, ... }:
        {
          github_cache_key = {
            rekeyFile = ../github_cache_key.age;
            generator = {
              tags = [ "github_cache_key" ];
              dependencies.signing_key = secrets.signing_key;
              script = celler.cellerTokenScript {
                sub = "github";
                push = [ "main" ];
              };
            };
          };
        };
    };

  # `nix run .#celler-token -- <server> --sub admin --pull '*' --push '*' --create-cache '*'`
  # mints a token for <server> by hand (e.g. to `celler cache create` on it),
  # decrypting its generated signing key with the master identity.
  perSystem =
    { pkgs, system, ... }:
    let
      mintToken = pkgs.writeShellApplication {
        name = "celler-token";
        runtimeInputs = [
          pkgs.celler
          pkgs.rage
          (inputs.age-plugin-gpg.packages.${system}.age-plugin-gpg.overrideAttrs (attrs: {
            postInstall = (attrs.postInstall or "") + ''
              ln -s $out/bin/age-plugin-gpg $out/bin/age-plugin-gpg-1
            '';
          }))
        ];
        text = ''
          if [ ! -e flake.nix ]; then
          	echo "celler-token: run from the repo root" >&2
          	exit 1
          fi
          if [ "$#" -lt 1 ]; then
          	echo "usage: celler-token <server> [make-token args...]" >&2
          	exit 1
          fi
          key="secrets/generated/$1/celler-signing-key.age"
          shift
          CELLER_SERVER_TOKEN_RS256_SECRET_BASE64="$(rage -d -i aspects/security/gpg-yubikey.pub "$key")"
          export CELLER_SERVER_TOKEN_RS256_SECRET_BASE64
          exec celleradm -f ${celler.tokenConfig pkgs} make-token --validity "''${VALIDITY:-10y}" "$@"
        '';
      };
    in
    {
      packages.celler-token = mintToken;
      apps.celler-token = {
        type = "app";
        program = lib.getExe mintToken;
      };
    };
}
