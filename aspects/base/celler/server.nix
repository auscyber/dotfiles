{
  inputs,
  den,
  lib,
  config,
  ...
}:
let
  # ── THE CAPABILITY HALF, PARAMETRIC IN THE PORT ──
  # A celler is a celler whether it runs as this host's service on 8069 or as
  # ../../hosts/celler2.nix's container image on 8080. What it GIVES is the same
  # shape with a different number -- and the number cannot be read off config,
  # because a giver's `gives` is evaluated in the ASKER's scope, so `host` there
  # is the consumer rather than the server.
  #
  # So the port is a PARAMETER of the declaration. Before this, celler2 could
  # not declare itself a celler at all: including `den.aspects.celler` fixes
  # `services.cellerd.expose.port` at 8069 against the 8080 it serves, so
  # nothing there gave the `celler` capability and a nested take had no root to
  # resolve from -- which silently dropped every edge to it.
  cellerCapability =
    { port, subdomain }:
    {
      # `reach = "fleet"`: any host may consume it. The laptop builds against it
      # over the tailnet, secondpc over loopback.
      reach = "fleet";

      gives =
        { self, ... }:
        {
          # NO `name`: a celler server is named after its host, and the
          # candidate already carries it -- ../capability.nix falls back to it
          # for `takes.celler.name = "secondpc"`. Furnishing it here would be
          # wrong, for the same scope reason the port is a parameter.
          #
          # A celler EXPOSES caches, so `cache` is furnished by each cache
          # aspect (../../services/celler-service.nix) rather than here -- this
          # level only identifies the server.
          celler = _: {
            carrier = self.name;
          };

          # THE ENDPOINT HALF ONLY: the port, the scheme, and the name it is
          # published under. NO ADDRESS -- a service has none; it is reached over
          # whichever carrier the two ends share, and ../capability.nix composes
          # the two halves.
          #
          # `subdomain` is what makes the public route derivable: composed with a
          # carrier serving the zone `ivymect.in` it is `cache.ivymect.in`, and
          # neither half holds that string.
          http = _: {
            inherit port subdomain;
            ports.http.meta.description = "cellerd's cache API: NAR pushes from the fleet and substituter fetches, published as ${subdomain}.ivymect.in.";
            protocol = "http://";
            carrier = self.name;
          };

          user = _: {
            name = "cellerd";
            carrier = self.name;
          };
        };
    };

  # NO `denServices` CAPTURE ANY MORE. It was here because a quirk body cannot
  # ask for the registry -- a quirk's thunk gets a fixed arg set, so
  # `{ denServices, ... }` fails with `called without required argument` -- so
  # the registry had to be closed over at file level. The cache list now comes
  # from `den.aspects` and `host.hasAspect`, both of which a quirk body already
  # has, so there is nothing to capture.
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

      # The unit is `cellerd`, not the scope name, so nothing would be inferred
      # -- and inference has to stay off rather than be pointed at `cellerd`:
      # `services.cellerd.user` is a DynamicUser, so there is no account to
      # chown to, and systemd reads the EnvironmentFile as root anyway. Same
      # escape hatch ../../services/gateway.nix uses: a literal unit name.
      secretSettings = {
        service = null;
        settings.restartUnits = [ "cellerd.service" ];
      };

      # Named after the host. This read `baseNameOf age.rekey.generatedSecretsDir`,
      # which was the host only while that directory was per host; it is one
      # directory for the fleet now and its basename is `generated` -- measured,
      # as `secrets/generated/generated/celler-signing-key.age`. The entity is
      # its own option (../../../lib/age-scoped.nix), so it is read directly.
      secrets = { age, ... }: {
        signing_key = celler.signingKey age.scopedEntity;
      };

      # In the `templates` class rather than raw in `nixos`, because only the
      # classes an aspect re-emits get scope-local args: `secrets` here is the
      # `celler` scope, keyed by short name.
      # The env file is the only part of cellerd's configuration that does not
      # end up world-readable in the store, so everything secret goes here --
      # the signing key always, and the R2 credentials on a server whose
      # storage is a bucket.
      #
      # Those are picked up rather than passed: a template implicitly depends
      # on every secret in its scope (../../../lib/age-scoped.nix), so a server
      # that declares `r2_access_key_id` in its own `celler` scope gets the
      # lines for free and one that does not has no mention of them. The AWS
      # SDK reads exactly these two names when celler's `[storage]` omits
      # `credentials`, which is how the keys stay out of the TOML.
      templates.env = { secrets, ... }: {
        dependencies.signing_key = secrets.signing_key;
        content =
          { placeholders, ... }:
          ''
            CELLER_SERVER_TOKEN_RS256_SECRET_BASE64=${placeholders.signing_key}
          ''
          + lib.optionalString (secrets ? r2_access_key_id) ''
            AWS_ACCESS_KEY_ID=${placeholders.r2_access_key_id}
            AWS_SECRET_ACCESS_KEY=${placeholders.r2_secret_access_key}
          '';
      };

      # The quirk stays as the TRANSPORT -- it carries `endpoint`/`addresses`,
      # which are detected from this host's evaluated config and so cannot come
      # from a flake-level entity. What it no longer carries is the cache LIST:
      # that is now derived from `den.services`, where each cache is an entity
      # identified by the celler it depends on (`main/celler@secondpc`).
      #
      # Before, this read `caches = if k == { } then [ "main" ] else attrNames k`
      # -- a hardcoded `main` fallback plus "whatever has a signing key". So the
      # set of caches was implied by the secrets rather than declared, and a
      # cache with no key yet was invisible.
      celler-caches =
        {
          config,
          host,
          ...
        }:
        let
          k = keys.${host.name} or { };

          # THE CACHES THIS HOST SERVES, BY INCLUSION. A cache is an aspect
          # carrying a `cache` record (../../services/celler-service.nix), so
          # this host serves the ones whose aspects it includes -- membership is
          # inclusion, and a host cannot claim a cache it does not include.
          #
          # Was a walk over `den.services` filtered by `serviceName == "celler"
          # && host == host.name`, which needed the registry to have spawned an
          # instance for this host and to have stamped `children` under it. Two
          # derived fields and a cascade, for a question that is one `hasAspect`
          # per candidate.
          # BY `cache.name`, NOT BY THE ASPECT NAME. The aspect is `celler-main`
          # and the cache is `main`, which is what celler-keys.json holds and
          # what a token is scoped to -- reading the aspect name here made every
          # cache look as though it had no signing key.
          declared = lib.mapAttrsToList (_: a: a.cache.name) (
            lib.filterAttrs (_: a: (a ? cache)) (den.lib.capability.heldBy host)
          );

          # A declared cache with no signing key is a cache that does not
          # exist. `celler.use` filters it out of `keys` SILENTLY, so without
          # this the token would be scoped to a cache the server cannot sign
          # for and the only symptom would be a 403 at run time.
          missingKeys = lib.subtractLists (builtins.attrNames k) (lib.unique declared);
        in
        {
          name = host.name;

          # `endpoint`/`addresses` STAY detected from this host's config, and
          # the capability does not replace them. Tried and reverted: the record
          # is pooled fleet-wide, so its `endpoint` has to be an address OTHER
          # hosts can use -- which is what `consumeVia` picks -- while the
          # capability resolves an endpoint PER CONSUMER, correctly giving
          # loopback here and the tailnet from the laptop.
          #
          # So the two answer different questions and both are wanted:
          # ./push.nix's `endpointFor` resolves per consumer and this remains
          # the fallback, plus the value `flake-file.nixConfig` needs below,
          # where there is no consuming host to resolve for.
          inherit (config.services.cellerd.expose) endpoint addresses;
          keys = k;
          # Declared caches win; the key-derived set remains the fallback for a
          # host that declares no cache aspect, so this stays additive.
          # A declared cache with no signing key is a cache that does not exist.
          # `celler.use` filters it out of `keys` silently, so without this the
          # token would be scoped to a cache the server cannot sign for, and the
          # only symptom would be a 403 at runtime.
          missingKeys = lib.subtractLists (builtins.attrNames k) (lib.unique declared);

          caches =
            if declared != [ ] then
              if missingKeys == [ ] || k == { } then
                lib.unique declared
              else
                throw "celler on ${host.name}: cache aspect(s) ${lib.concatStringsSep ", " missingKeys} with no signing key in aspects/base/celler/celler-keys.json (it has: ${lib.concatStringsSep ", " (builtins.attrNames k)}). A cache without a key cannot be pushed to -- add the key, or drop the cache aspect from this host."
            else if k == { } then
              [ "main" ]
            else
              builtins.attrNames k;
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
          host,
          scoped,
          ...
        }:
        let
          cfg = config.services.cellerd.expose;
          # The caches this server is meant to serve, by the same rule
          # `celler-caches` above publishes them.
          serverCaches =
            let
              k = keys.${host.name} or { };
            in
            if k == { } then [ "main" ] else builtins.attrNames k;
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

            # Create the caches this server serves, with a token it mints from
            # its own signing key -- so a fresh server (or a fresh R2 bucket)
            # comes up serving the caches ./celler-keys.json says it has,
            # rather than 404ing until someone runs `celler cache create` by
            # hand.
            #
            # `postStart`, NOT `preStart`: creating a cache is an API call to
            # this very server, so it cannot run before the listener exists.
            # The loop is still needed because the unit is `exec`-started --
            # systemd considers it up once the process is spawned, which is
            # earlier than the first accepted connection.
            #
            # Idempotent by asking first: `cache create` on an existing cache
            # is an error, and `|| true` would swallow real ones too.
            systemd.services.cellerd.postStart = ''
              for _ in $(seq 1 60); do
                if ${lib.getExe pkgs.curl} -sf -o /dev/null \
                  http://localhost:${toString cfg.port}/; then break; fi
                sleep 1
              done

              # Short validity and a throwaway subject: this token exists for
              # the length of this script and is never written anywhere. Full
              # permissions because it is the server administering itself.
              token="$(${lib.getExe' pkgs.celler "celleradm"} -f ${celler.tokenConfig pkgs} make-token \
                --sub cellerd-caches --validity 5m \
                --pull '*' --push '*' --delete '*' \
                --create-cache '*' --configure-cache '*' \
                --configure-cache-retention '*' --destroy-cache '*')"

              # The client keeps its config under XDG_CONFIG_HOME; cellerd is a
              # DynamicUser with no home, so point it at the runtime dir.
              export XDG_CONFIG_HOME="$RUNTIME_DIRECTORY/client"
              mkdir -p "$XDG_CONFIG_HOME"
              ${lib.getExe pkgs.celler-client} login self \
                http://localhost:${toString cfg.port} "$token"

              ${lib.concatMapStrings (c: ''
                if ${lib.getExe pkgs.celler-client} cache info ${lib.escapeShellArg "self:${c}"} \
                  >/dev/null 2>&1
                then
                  echo "cellerd: cache ${c} already exists"
                else
                  echo "cellerd: creating cache ${c}"
                  ${lib.getExe pkgs.celler-client} cache create ${lib.escapeShellArg "self:${c}"}
                fi
              '') serverCaches}
            '';
            systemd.services.cellerd.serviceConfig.RuntimeDirectory = "cellerd";

            systemd.services.cloudflared-tunnel = lib.mkIf (cfg.cloudflared.hostname != null) {
              wantedBy = [ "multi-user.target" ];
              after = [ "network-online.target" ];
              wants = [ "network-online.target" ];
              serviceConfig = {
                # `--config`: the tunnels this flake mints are
                # `config_src: "local"` (../../network/cloudflare.nix), so the
                # routes come from the closure and not from the account.
                ExecStart = "${lib.getExe pkgs.cloudflared} tunnel --no-autoupdate --config ${
                  (pkgs.formats.yaml { }).generate "cloudflared.yml" {
                    ingress = [
                      {
                        hostname = cfg.cloudflared.hostname;
                        service = "http://localhost:${toString cfg.port}";
                      }
                      { service = "http_status:404"; }
                    ];
                  }
                } run";
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
  # Exported so ../../hosts/celler2.nix can declare its own celler without
  # reaching for a path import.
  _module.args.cellerCapability = cellerCapability;

  den.aspects.celler =
    let
      port = 8069;
    in
    cellerCapability {
      inherit port;
      subdomain = "cache";
    }
    // {
      includes = [ den.aspects.celler-server._.nixos ];

      # ── THE SAME CONSTRAINT, DECLARED RATHER THAN HAND-EXCLUDED ──
      #
      # `client_max_body_size 15g` on the vhost below, and
      # `cache.ivymect.in` sitting in `cloudflared-tunnel.exclude` by hand,
      # are one fact written twice: `celler push` sends NARs far past
      # Cloudflare's 100MB cap. Declaring the limit lets
      # ../../network/cloudflare.nix derive the exclusion instead.
      #
      # No `port`/`upstream`: the vhost is declared below, with a `root` and
      # locations this file owns, so ../../services/proxy-nginx.nix records the
      # take and renders nothing. ../../hosts/celler2.nix takes no proxy at all
      # -- it publishes through its own cloudflared ingress.
      takes.proxy = {
        subdomain = "cache";
        maxBodyBytes = 15 * 1000 * 1000 * 1000;
      };

      vhosts."cache.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:${toString port}";
        extraConfig = ''
          client_max_body_size 15g;
        '';
      };
      # A cloudflared tunnel to the same cellerd, as a third way in next to the
      # nginx vhost and the tailnet. Remotely managed (the dashboard holds the
      # route to http://localhost:${toString port}), so all this side needs is
      # the connector token -- `cloudflared tunnel run` reads TUNNEL_TOKEN from
      # the environment and asks Cloudflare what to serve.
      #
      # ── `consumeVia` IS NOW A FALLBACK, NOT THE DECISION ──
      #
      # A consumer derives its carrier at the point of use: a connection needs a
      # transport, and the flow's own constraints pick which
      # (../base/capability.nix, reached from ./push.nix). This value is what
      # ../celler/_lib.nix still uses when that resolution finds nothing -- a
      # scope with no host binding, or a server whose host is not a den entity.
      #
      # It stays `tailscale` rather than reverting to the `virtualHost` default
      # precisely because the fallback must not be the public route: Cloudflare
      # caps a proxied request body at 100MB and `celler push` sends NARs far
      # past it (the vhost above says `client_max_body_size 15g`). The tailnet
      # has neither limit and is reachable from off the LAN, which the LAN
      # address is not.
      #
      # That reasoning is now also the DERIVATION -- `maxBodyBytes` on the
      # public carrier against `takes.http.maxBodyBytes = b: b == null` on the
      # push flow -- so the two agree by construction instead of by somebody
      # having worked it out once.
      #
      # `cache.ivymect.in` stays up and stays a direct A record -- it is
      # excluded from the host's tunnel in ../../hosts/secondpc/default.nix.
      nixos.services.cellerd.expose = {
        inherit port;
        consumeVia = "tailscale";
      };

      cellerd = _: {
        # Local storage on the ZFS data pool, deliberately rather than an
        # object store. This used to be Cloudflare R2, selected through a
        # `pathExists` check on the `r2-token` generator's plaintext metadata;
        # that whole arrangement is gone, along with this host's dependence on
        # a generated file being present before it would evaluate.
        #
        # A binary cache is regenerable by construction -- the cost of losing
        # it is a rebuild, not lost data -- so paying an object store to hold
        # it only buys durability that is not worth much here.
        #
        # One consequence worth stating: cache.ivymect.in is served publicly
        # and is excluded from the cloudflare tunnel (see
        # ../../hosts/secondpc/default.nix, where the exclusion exists because
        # Cloudflare caps a proxied body at 100MB and `celler push` sends NARs
        # past it). So cache egress now comes off the home connection with
        # nothing in front of it.
        #
        # NARs already uploaded to R2 are not orphaned by this: a NAR's backend
        # is recorded per file (celler's `RemoteFile` is `S3 | Local | Http`),
        # so anything stored there keeps being served from there while new
        # uploads land on disk.
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
      # No R2 credentials any more -- `storage` above is local, so there is no
      # bucket to authenticate against and `den.lib.cloudflare.r2Secrets` is
      # not called from anywhere in this tree.
      secrets = { secrets, ... }: {
        github_cache_key = {
          rekeyFile = ../github_cache_key.age;
          # Shares the `celler` scope, but the server never reads it.
          restartUnits = [ ];
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
    {
      config,
      pkgs,
      ...
    }:
    let
      mintToken = pkgs.writeShellApplication {
        name = "celler-token";
        # `secret-view` is agenix's own `view` subcommand (../../security/
        # agenix-rekey.nix), so it knows the configured `masterIdentities` and
        # carries the right age plugins. This used to be a bare `rage -d -i
        # aspects/security/gpg-yubikey.pub`, which pinned one master here as
        # well as there -- and broke the moment that master changed.
        runtimeInputs = [
          pkgs.celler
          config.packages.secret-view
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
          CELLER_SERVER_TOKEN_RS256_SECRET_BASE64="$(secret-view "$key")"
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
