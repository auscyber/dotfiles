{
  den,
  lib,
  ...
}:
# One place to say how a service is reached: which vhost and path, whether the
# browser leg goes through oauth2-proxy, and who may call its API under which
# key.
#
# The API-key half exists so callers are distinguishable. A service knows
# exactly one key -- `<svc>-internal`, which is also the key provisioned INTO
# the service -- while every caller gets a `<caller>-<svc>` key of its own.
# nginx rewrites the caller's key to the internal one before proxying, so the
# service is none the wiser, the access log records which caller it was, and a
# key can be rotated or revoked for one caller without touching the rest.
let
  # `vhosts` in ./nginx.nix is the model: an aspect writes the key next to the
  # service it belongs to, and it lands in the central option.
  mkForward =
    fromClass: path:
    {
      class,
      aspect-chain,
    }:
    den.batteries.forward {
      each = lib.singleton true;
      fromClass = _: fromClass;
      intoClass = _: "nixos";
      intoPath = _: path;
      fromAspect = _: lib.head aspect-chain;
      adaptArgs = lib.id;
    };

  # Lets a service aspect declare its own kanidm OAuth2 client, group or person
  # instead of them all piling up in one file.
  kanidmProvision = mkForward "provision" [
    "services"
    "kanidm"
    "provision"
  ];

  gatedServices = mkForward "gated" [
    "gateway"
    "services"
  ];
in
{
  den.aspects.gateway = {
    includes = [
      kanidmProvision
      gatedServices
      den.aspects.nginx
      den.aspects.agenix-rekey
    ];

    nixos =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        inherit (lib) mkOption types;

        # The entry submodule shadows `config` with its own; this keeps a handle
        # on the host's, for the `enable` default below.
        outer = config;

        # Templates stay at agenix's own default path. Writing them into a
        # directory of our own would race systemd-tmpfiles on boot, and this
        # one is created before any unit starts.
        mapFile = name: "${config.age.templateDir}/gateway/${name}.map";

        # nginx's `include` errors on a missing literal path but is content
        # with a glob that matches nothing -- which is what the build-time
        # `nginx -t` sees, before agenix has ever run. Bracketing the first
        # character makes the pattern a glob while still matching exactly one
        # name.
        globFile =
          name: mapFile "[${lib.substring 0 1 name}]${lib.substring 1 (lib.stringLength name) name}";

        # nginx lowercases header names and turns '-' into '_' to build $http_*;
        # the same sanitising keeps generated variable names legal.
        nginxName = lib.replaceStrings [ "-" "." ] [ "_" "_" ];
        headerVar = header: "$http_${nginxName (lib.toLower header)}";

        # nginx wants a trailing slash on both sides of a prefix proxy, but
        # writing one in every aspect is noise -- so accept either spelling.
        slashed = p: if lib.hasSuffix "/" p then p else "${p}/";

        entries = lib.filter (e: e.enable) (
          lib.mapAttrsToList (
            name: svc:
            svc
            // {
              inherit name;
              subpath = slashed svc.subpath;
              api = svc.api // {
                subpath = slashed svc.api.subpath;
              };
            }
          ) config.gateway.services
        );
        gated = lib.filter (e: e.api.enable) entries;

        # Exactly two keys are in play for a service: the one it ACCEPTS
        # (`<svc>-internal`, handed to the service itself) and the one it
        # PRESENTS when calling something else (`account-<svc>`, the credential
        # of its service account). A caller that is not a service of its own --
        # homepage, seerr, soularr -- has only the second.
        #
        # One credential per principal, not per (caller, service) pair: the same
        # account key appears in every map whose service admits that account, so
        # access is granted by map membership rather than by minting another
        # secret, and revoking one service's access never rotates a key the
        # account still needs elsewhere.
        # Scoped by WHOSE credential it is, not by what consumes it: a key lives
        # under the principal that holds it, so `sonarr/api-key` is sonarr's to
        # present and `sonarr/internal` is sonarr's to accept. Everything that
        # needs one -- the nginx maps, the env files -- takes it as an agenix
        # DEPENDENCY, which references the one secret rather than minting a copy
        # per consumer. So a key shared by several readers is one `.age` file.
        internalSecret = e: "${e.name}/internal";
        accountSecret = account: "${account}/api-key";

        accounts = lib.attrNames config.gateway.serviceAccounts;

        # Services this account may call, which is what its key has to open.
        callableBy = account: lib.filter (e: lib.elem account e.api.clients) gated;

        # An account's key is presented differently depending on the target, so
        # the attribution table needs one entry per distinct scheme it uses.
        prefixesFor = account: lib.unique (map (e: e.api.headerPrefix) (callableBy account));

        randomKey =
          {
            pkgs,
            lib,
            ...
          }:
          ''
            ${lib.getExe pkgs.openssl} rand -hex 32 | ${pkgs.coreutils}/bin/tr -d '\n'
          '';

        nginxOwned = {
          owner = config.services.nginx.user;
          restartUnits = [ "nginx.service" ];
          symlink = false;
        };
      in
      {
        options.gateway.domain = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "ivymect.in";
          description = ''
            Zone the gated services are published under. Service aspects name
            only their subdomain, so the zone stays a host-level decision and
            an aspect can move between hosts unchanged.
          '';
        };

        options.gateway.humanGroups = mkOption {
          type = types.listOf types.str;
          default = [ ];
          example = [ "media-users" ];
          description = ''
            Groups whose people may reach every gated service through a browser.
            Folded into each service's `groups`, because a per-service machine
            group on its own would lock out the humans the service is for.
          '';
        };

        options.gateway.authCacheTtl = mkOption {
          type = types.nullOr types.str;
          default = "1m";
          example = "30s";
          description = ''
            How long an oauth2-proxy verdict is reused before asking again.
            Without this every request -- including each of the dozens a single
            page load makes -- costs a subrequest, and with
            `skip-jwt-bearer-tokens` that means a kanidm round trip per API call.

            The cost is revocation lag: a removed group or a revoked token stays
            usable for up to this long. Set to null to disable caching.
          '';
        };

        options.gateway.acmeHost = mkOption {
          type = types.nullOr types.str;
          default = outer.gateway.domain;
          defaultText = "gateway.domain";
          description = "`useACMEHost` for every vhost the gateway creates.";
        };

        options.gateway.serviceAccounts = mkOption {
          default = { };
          description = ''
            Machine principals that may call a gated API. A service that
            exposes an API is registered as one automatically, so `prowlarr`
            is callable-from as soon as it exists; declare an entry here only
            for a caller that is not itself a gated service.

            These are NOT kanidm service accounts: kanidm-provision's schema
            covers persons, groups and oauth2 clients only, with no way to
            declare a service account or mint it an API token. So the account
            is modelled here and its credential is an agenix-generated key --
            same attribution and same per-caller rotation, resolved at nginx
            rather than at the IdP.
          '';
          type = types.attrsOf (
            types.submodule {
              options = {
                description = mkOption {
                  type = types.str;
                  default = "";
                };
                envPrefix = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                  example = "HOMEPAGE_VAR_";
                  description = ''
                    When set, every key this account holds is rendered into one
                    env file as `<prefix><SERVICE>_KEY`. Consumers that read
                    credentials from the environment (and are read by systemd as
                    root, so a DynamicUser unit can still use them) point their
                    `environmentFile` at it.
                  '';
                };
              };
            }
          );
        };

        options.gateway.services = mkOption {
          default = { };
          description = "Services published through the nginx gateway.";
          type = types.attrsOf (
            types.submodule (
              {
                name,
                config,
                ...
              }:
              {
                options = {
                  enable = mkOption {
                    type = types.bool;
                    default = outer.services.${name}.enable or true;
                    defaultText = "the service's own `enable`, or true when there is no such service";
                    description = ''
                      Turning a service off takes its vhost, its locations and
                      its keys with it, rather than leaving a 502 behind.
                    '';
                  };
                  domain = mkOption {
                    type = types.str;
                    default = "${name}.${outer.gateway.domain}";
                    defaultText = "\${name}.\${gateway.domain}";
                    example = "arr.ivymect.in";
                    description = ''
                      Virtual host this service is served under. Left alone it
                      is the service's own name under `gateway.domain`, which is
                      what keeps an aspect portable between hosts; name a whole
                      domain to share one vhost between several services.
                    '';
                  };
                  subpath = mkOption {
                    type = types.str;
                    default = "/";
                    example = "/prowlarr";
                    description = ''
                      Location prefix on that vhost. A trailing slash is added
                      if absent.
                    '';
                  };
                  url = mkOption {
                    type = types.str;
                    readOnly = true;
                    default = "https://${config.domain}${lib.removeSuffix "/" config.subpath}";
                    defaultText = "https://\${domain}\${subpath}";
                    description = ''
                      Where a caller reaches this service. Callers must use this
                      rather than the upstream: the per-caller key is only
                      swapped for the service's own key on the way through
                      nginx, so a loopback shortcut would present a key the
                      service has never heard of.
                    '';
                  };
                  upstream = mkOption {
                    type = types.str;
                    example = "http://127.0.0.1:8989";
                    description = "Scheme, host and port of the backend.";
                  };
                  banner = mkOption {
                    type = types.nullOr types.str;
                    default = null;
                    example = "Read-only: managed by Nix";
                    description = ''
                      Text injected into the app's own UI, for a service whose
                      settings are declared in Nix and whose in-app editors
                      therefore do nothing (or worse, appear to work until the
                      next activation overwrites them).
                    '';
                  };
                  sso = mkOption {
                    type = types.bool;
                    default = true;
                    description = "Put the browser leg behind oauth2-proxy.";
                  };
                  groups = mkOption {
                    type = types.listOf types.str;
                    default = outer.gateway.humanGroups ++ [ "svc-${name}" ];
                    defaultText = "gateway.humanGroups ++ [ \"svc-\u2039name\u203a\" ]";
                    description = ''
                      kanidm groups allowed to reach this service. oauth2-proxy
                      checks them for a browser session and, with
                      `skip-jwt-bearer-tokens`, for a machine caller's bearer
                      token too -- so authorisation is one rule enforced in one
                      place for both kinds of caller.
                    '';
                  };
                  websockets = mkOption {
                    type = types.bool;
                    default = false;
                  };
                  api = {
                    enable = mkOption {
                      type = types.bool;
                      default = false;
                      description = ''
                        Serve this service's API on its own location, gated by an
                        API key instead of a browser session.
                      '';
                    };
                    subpath = mkOption {
                      type = types.str;
                      default = "${config.subpath}/api";
                      defaultText = "\${subpath}/api";
                    };
                    header = mkOption {
                      type = types.str;
                      default = "X-Api-Key";
                      example = "Authorization";
                      description = "Request header the key is read from.";
                    };
                    headerPrefix = mkOption {
                      type = types.str;
                      default = "";
                      example = "Bearer ";
                      description = ''
                        Scheme prefix the header value carries, for services
                        that take their key as `Authorization: Bearer <key>`
                        rather than a bare header (qBittorrent 5.1+).
                      '';
                    };
                    internalKey = mkOption {
                      type = types.bool;
                      default = config.api.internalEnv != null;
                      defaultText = "internalEnv != null";
                      description = ''
                        Whether the service holds a key of its own for the
                        gateway to swap in. When false the caller's key is
                        validated and then STRIPPED, and the service is left
                        trusting loopback -- which is how a service whose key
                        can only be set from a world-readable store file avoids
                        having one there.
                      '';
                    };
                    clients = mkOption {
                      type = types.listOf types.str;
                      default = [ ];
                      example = [ "prowlarr" ];
                      description = ''
                        Callers that get their own key for this service. Each
                        yields `<caller>-<service>`, rewritten to the service's
                        internal key on the way through.
                      '';
                    };
                    internalEnv = mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      example = "SONARR__AUTH__APIKEY";
                      description = ''
                        Environment variable that hands the internal key to the
                        service itself. When set, a one-line env file is rendered
                        for the service's `environmentFiles`.
                      '';
                    };
                    owner = mkOption {
                      type = types.str;
                      default = name;
                      description = "Unix user that reads the generated env file.";
                    };
                  };
                };
              }
            )
          );
        };

        config = lib.mkIf (entries != [ ]) {
          # A gated service is a principal in its own right, so it can be named
          # as a client of another one without being declared twice.
          gateway.serviceAccounts = lib.listToAttrs (
            map (e: lib.nameValuePair e.name { description = "${e.name} service"; }) gated
          );

          assertions = lib.concatMap (
            e:
            map (client: {
              assertion = config.gateway.serviceAccounts ? ${client};
              message = ''
                gateway.services.${e.name}.api.clients names "${client}", which is
                not a declared service account. Add gateway.serviceAccounts.${client}
                or gate a service of that name.
              '';
            }) e.api.clients
          ) gated;
          # `age.scoped.<principal>` rather than raw `age.secrets`: the scope
          # supplies the `<principal>/` prefix, and for a scope named after a
          # real service it also infers that service's owner, group and
          # restartUnits -- so sonarr restarts when the key it accepts changes.
          age.scoped = lib.mkMerge (
            map (account: {
              ${account} = {
                # `service = null` disables inference, and it has to be off here:
                # inferring would read `services.<name>` while that service's own
                # `environmentFiles` is being computed from a template that
                # depends on this very secret. That is the loop lib/age-scoped.nix
                # documents, and this is the escape hatch it prescribes --
                # explicit `settings` instead of a derived owner and restart.
                service = null;
                secrets.api-key.generator.script = randomKey;
              };
            }) accounts
            ++ map (e: {
              ${e.name} = {
                service = null;
                # Named literally rather than looked up, so nothing is forced.
                settings.restartUnits = [ "${e.name}.service" ];
                secrets.internal.generator.script = randomKey;
              };
            }) (lib.filter (e: e.api.internalKey) gated)
          );

          age.templates =
            # Per-service rewrite table: caller key -> internal key. A key absent
            # from a service's table is a 401 there, so a key minted for one
            # service is useless against another.
            lib.listToAttrs (
              map (
                e:
                lib.nameValuePair "gateway/${e.name}.map" (
                  nginxOwned
                  // {
                    dependencies =
                      lib.listToAttrs (
                        map (client: lib.nameValuePair client config.age.secrets.${accountSecret client}) e.api.clients
                      )
                      // lib.optionalAttrs e.api.internalKey {
                        internal = config.age.secrets.${internalSecret e};
                      };
                    content =
                      { placeholders, ... }:
                      let
                        target = if e.api.internalKey then placeholders.internal else "1";
                      in
                      lib.concatMapStrings (client: ''
                        "${e.api.headerPrefix}${placeholders.${client}}" "${target}";
                      '') e.api.clients
                      # The service's own key maps to itself, so its web UI --
                      # which embeds that key in the page -- keeps working.
                      + lib.optionalString e.api.internalKey ''
                        "${e.api.headerPrefix}${placeholders.internal}" "${target}";
                      '';
                  }
                )
              ) gated
            )
            # One global caller table, so a single log_format can name whoever
            # made the call regardless of which service it was aimed at.
            // lib.optionalAttrs (accounts != [ ]) {
              "gateway/callers.map" = nginxOwned // {
                dependencies = lib.listToAttrs (
                  map (a: lib.nameValuePair a config.age.secrets.${accountSecret a}) accounts
                );
                content =
                  { placeholders, ... }:
                  lib.concatMapStrings (
                    account:
                    lib.concatMapStrings (prefix: ''
                      "${prefix}${placeholders.${account}}" "${account}";
                    '') (prefixesFor account)
                  ) accounts;
              };
            }
            # One env file per account that asked for one, holding every key
            # that account holds.
            // lib.listToAttrs (
              map (
                account:
                lib.nameValuePair "gateway/account-${account}.env" {
                  dependencies.key = config.age.secrets.${accountSecret account};
                  content = { placeholders, ... }: ''
                    ${config.gateway.serviceAccounts.${account}.envPrefix}KEY=${placeholders.key}
                  '';
                }
              ) (lib.attrNames (lib.filterAttrs (_: a: a.envPrefix != null) config.gateway.serviceAccounts))
            )
            # The service's own copy of its internal key, as an env file.
            // lib.listToAttrs (
              map (
                e:
                lib.nameValuePair "gateway/${e.name}.env" {
                  owner = e.api.owner;
                  restartUnits = [ "${e.name}.service" ];
                  dependencies.internal = config.age.secrets.${internalSecret e};
                  content = { placeholders, ... }: ''
                    ${e.api.internalEnv}=${placeholders.internal}
                  '';
                }
              ) (lib.filter (e: e.api.internalEnv != null) gated)
            );

          # Map bodies are globbed in rather than written inline: the key values
          # are secrets, and a glob matching nothing keeps the build-time
          # `nginx -t` happy before agenix has ever run.
          # nginx's own option rather than a hand-written proxy_cache_path: it
          # emits the directive in the right place and the directory comes from
          # the unit's `CacheDirectory`, which a hand-rolled path does not get.
          # Through the module's options rather than raw directives: `lines`
          # concatenates every definition, so a raw `map_hash_bucket_size` is a
          # duplicate the moment anything else emits one, whereas an int option
          # merges. Every key in the tables below is 64 hex characters, and a
          # `Bearer` service's carry a 7-character prefix on top -- past nginx's
          # 64-byte default bucket, reported at startup as "could not build
          # map_hash". Powers of two only.
          services.nginx.mapHashBucketSize = lib.mkDefault 256;
          services.nginx.mapHashMaxSize = lib.mkDefault 4096;

          services.nginx.proxyCachePath.gateway-auth = lib.mkIf (config.gateway.authCacheTtl != null) {
            enable = true;
            keysZoneName = "gateway_auth";
            keysZoneSize = "4m";
            maxSize = "32m";
            inactive = "10m";
          };

          # `commonHttpConfig`, NOT the append variant: that one is emitted after
          # the server blocks, and everything defined here -- the log format, the
          # key maps, the cache zone -- is referenced from inside them. nginx
          # parses in order, so defining them later is simply "unknown log format
          # gw_api" / "unknown variable $gw_key_sonarr" at startup.
          services.nginx.commonHttpConfig = lib.mkMerge [
            (lib.mkIf (gated != [ ]) ''
              log_format gw_api '$remote_addr $gw_caller "$request" $status $body_bytes_sent $request_time';

              map "$http_x_api_key$http_authorization" $gw_caller {
                  default "-";
                  include ${globFile "callers"};
              }

                ${lib.concatMapStrings (e: ''
                  map ${headerVar e.api.header} $gw_key_${nginxName e.name} {
                      default "";
                      include ${globFile e.name};
                  }
                '') gated}
            '')
          ];

          # Fold the auth cache onto the subrequest location the oauth2-proxy
          # module writes. `extraConfig` is `types.lines`, so this appends to
          # what that module already put there rather than replacing it.
          services.nginx.virtualHosts = lib.mkMerge [
            (lib.mapAttrs (_: es: {
              useACMEHost = config.gateway.acmeHost;
              forceSSL = true;
              locations = lib.listToAttrs (
                lib.concatMap (
                  e:
                  [
                    (lib.nameValuePair e.subpath {
                      proxyPass = "${e.upstream}${e.subpath}";
                      proxyWebsockets = e.websockets;
                      # sub_filter cannot match through gzip, so the upstream has
                      # to be asked for plaintext before anything can be injected.
                      # The banner text is XML-escaped, not interpolated raw: an
                      # apostrophe in it ('qBittorrent's settings...') closes
                      # nginx's single-quoted string early, which nginx reports
                      # as an "unexpected" token on the following word. Escaping
                      # also keeps it from injecting markup into the page.
                      #
                      # `sub_filter_types` is deliberately absent: text/html is
                      # already in its default, and naming it again is a
                      # "duplicate MIME type" warning at startup.
                      extraConfig = lib.optionalString (e.banner != null) ''
                        proxy_set_header Accept-Encoding "";
                        sub_filter_once on;
                        sub_filter '</body>' '<div style="position:fixed;bottom:0;left:0;right:0;z-index:99999;padding:6px 12px;font:600 13px/1.4 system-ui,sans-serif;text-align:center;color:#1b1b1b;background:#f5c451;box-shadow:0 -1px 4px rgba(0,0,0,.3)">${lib.escapeXML e.banner}</div></body>';
                      '';
                    })
                  ]
                  ++ lib.optional e.api.enable (
                    lib.nameValuePair "= /gw-auth-${nginxName e.name}" {
                      # Either credential will do: a valid key short-circuits to
                      # 204, anything else falls through to oauth2-proxy. Without
                      # this, an app whose own web UI drives its API from the
                      # browser -- qbittorrent's does, for everything -- 401s on
                      # every request, because a browser carries a session and
                      # not a key.
                      #
                      # `if` here is the safe form: it holds only a `return`, and
                      # proxy_pass sits outside it.
                      proxyPass = "${config.services.oauth2-proxy.nginx.proxy}/oauth2/auth${
                        lib.optionalString (e.groups != [ ])
                          "?allowed_groups=${lib.concatMapStringsSep "," lib.escapeURL e.groups}"
                      }";
                      extraConfig = ''
                        internal;
                        auth_request off;
                        if ($gw_key_${nginxName e.name} != "") { return 204; }
                        proxy_set_header X-Scheme       $scheme;
                        proxy_set_header Content-Length "";
                        proxy_pass_request_body         off;
                      '';
                    }
                  )
                  ++ lib.optional e.api.enable (
                    lib.nameValuePair e.api.subpath {
                      proxyPass = "${e.upstream}${e.api.subpath}";
                      # A longer prefix than the UI location, so it wins.
                      extraConfig = ''
                        auth_request /gw-auth-${nginxName e.name};
                        proxy_set_header ${e.api.header} ${
                          if e.api.internalKey then "\"${e.api.headerPrefix}$gw_key_${nginxName e.name}\"" else "\"\""
                        };
                        access_log /var/log/nginx/api.log gw_api;
                      '';
                    }
                  )
                ) es
              );
            }) (lib.groupBy (e: e.domain) entries))
            (lib.mkIf (config.gateway.authCacheTtl != null) (
              lib.genAttrs (lib.unique (map (e: e.domain) (lib.filter (e: e.sso) entries))) (_: {
                locations."= /oauth2/auth".extraConfig = ''
                  proxy_cache gateway_auth;
                  proxy_cache_key "$host$http_authorization$http_cookie";
                  proxy_cache_valid 200 204 ${config.gateway.authCacheTtl};
                  # Denials are cached far more briefly: a newly granted group
                  # should take effect quickly, a revoked one is bounded above.
                  proxy_cache_valid 401 403 5s;
                  proxy_cache_use_stale error timeout;
                '';
              })
            ))
          ];

          # Anything on this host that calls a gated service has to use the
          # public name so nginx can swap its key -- homepage's widgets are the
          # obvious case. Without this they leave the box and come back through
          # hairpin NAT, which most routers do not do. The wildcard certificate
          # still matches, so TLS verifies.
          networking.hosts."127.0.0.1" = lib.unique (map (e: e.domain) entries);

          # A vhost can carry several services, so it admits the union of their
          # groups; the per-service API locations narrow it again by key.
          services.oauth2-proxy.nginx.virtualHosts = lib.mapAttrs (_: es: {
            allowed_groups = lib.unique (lib.concatMap (e: e.groups) es);
          }) (lib.groupBy (e: e.domain) (lib.filter (e: e.sso) entries));

          # The service accounts themselves. They go in through `extraJsonFile`
          # rather than a typed option because upstream's schema has no entity
          # for them -- that is what patches/kanidm-provision adds. A side
          # effect worth knowing: setting `extraJsonFile` also switches off the
          # module's entity assertions, which is the only reason the group
          # memberships below are accepted at all (the module cannot see that a
          # service account named `prowlarr` exists).
          #
          # No `apiTokens` yet: minting one per account is a single line here,
          # but nothing consumes them until the oauth2-proxy bearer path is
          # confirmed against a live kanidm, and an unused token is a credential
          # sitting on disk for no reason.
          services.kanidm.provision.extraJsonFile = pkgs.writeText "gateway-service-accounts.json" (
            builtins.toJSON {
              serviceAccounts = lib.listToAttrs (
                map (
                  account:
                  lib.nameValuePair account {
                    displayName =
                      let
                        d = config.gateway.serviceAccounts.${account}.description;
                      in
                      if d == "" then account else d;
                    entryManagedBy = "idm_admins";
                  }
                ) accounts
              );
            }
          );

          # One kanidm group per service, holding the machine callers that may
          # reach it. Declared here rather than per-aspect so a service cannot
          # name clients without the group that authorises them existing.
          services.kanidm.provision.groups = lib.listToAttrs (
            map (
              e:
              lib.nameValuePair "svc-${e.name}" {
                members = e.api.clients;
              }
            ) entries
          );
        };
      };
  };
}
