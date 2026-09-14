{ den, ... }:
# Single sign-on for the services that have no usable auth of their own.
#
# kanidm is the identity provider (auth.ivymect.in); oauth2-proxy is the nginx
# forward-auth shim (sso.ivymect.in) that the protected vhosts delegate to.
# Everything about kanidm -- people, groups, OAuth2 clients -- is declared under
# `provision`, which runs as a post-start hook and REMOVES anything it created
# that is no longer declared here (`autoRemove` defaults to true).
#
# Post-deploy, once: `nix run .#rekey && nix run .#gen-secrets`, then log in as
# idm_admin (password in the sso/idm-admin secret) and issue auscyber a credential
# reset link -- kanidm has no way to provision a person's own password, by design.
{
  den.aspects.sso = {
    includes = [
      den.aspects.agenix-rekey
      # Wildcard `ivymect.in` cert + the nginx aspect.
      den.aspects.secondpc-web
      # `provision` and `gated` classes.
      den.aspects.gateway
      # Service accounts are not in upstream kanidm-provision's schema, and
      # stock serde SKIPS unknown fields rather than erroring -- so without this
      # overlay the accounts are silently never created and the `svc-*` group
      # memberships fail referential integrity instead.
      den.aspects.packages.kanidm-provision
      # Temporarily disabled: the patched kanidm_1_11 build (remote, on
      # faggot.sh) is still running and there's no need to block a celler
      # rebuild on it. Re-add once that build is confirmed good.
      den.aspects.packages.kanidm-trace-propagation
    ];

    secrets = {
      # kanidm resets idm_admin to this on every restart when provisioning is
      # on; without it the password is regenerated and printed to the journal.
      idm-admin = {
        owner = "kanidm";
        restartUnits = [ "kanidm.service" ];
        generator.script =
          {
            pkgs,
            lib,
            ...
          }:
          ''
            ${lib.getExe pkgs.openssl} rand -hex 32 | ${pkgs.coreutils}/bin/tr -d '\n'
          '';
      };

      # The OAuth2 client secret. kanidm reads this file directly
      # (`basicSecretFile`); oauth2-proxy gets the same value substituted into
      # its env template below, so the two cannot drift. Hex, and newline
      # stripped, because both sides compare it byte for byte.
      oauth2-client-secret = {
        owner = "kanidm";
        restartUnits = [ "kanidm.service" ];
        generator.script =
          {
            pkgs,
            lib,
            ...
          }:
          ''
            ${lib.getExe pkgs.openssl} rand -hex 32 | ${pkgs.coreutils}/bin/tr -d '\n'
          '';
      };

      # oauth2-proxy's cookie AES key: must be exactly 16, 24 or 32 bytes, so
      # 16 hex bytes rendered as 32 characters.
      oauth2-cookie-secret.generator.script =
        {
          pkgs,
          lib,
          ...
        }:
        ''
          ${lib.getExe pkgs.openssl} rand -hex 16 | ${pkgs.coreutils}/bin/tr -d '\n'
        '';
    };

    # oauth2-proxy takes its credentials as an EnvironmentFile rather than from
    # the command line, which is what keeps them out of the store and out of ps.
    templates."oauth2-proxy.env" = { secrets, ... }: {
      owner = "oauth2-proxy";
      restartUnits = [ "oauth2-proxy.service" ];
      dependencies = {
        inherit (secrets) oauth2-client-secret oauth2-cookie-secret;
      };
      content = { placeholders, ... }: ''
        OAUTH2_PROXY_CLIENT_ID=oauth2-proxy
        OAUTH2_PROXY_CLIENT_SECRET=${placeholders.oauth2-client-secret}
        OAUTH2_PROXY_COOKIE_SECRET=${placeholders.oauth2-cookie-secret}
      '';
    };

    vhosts = {
      # kanidm insists on serving TLS itself even behind a proxy, so this leg
      # is https->https. Verification is off (nginx's default): the backend
      # presents the wildcard cert and we reach it on a loopback address.
      "auth.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;
        locations."/" = {
          proxyPass = "https://127.0.0.1:8443";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_ssl_server_name on;
            proxy_ssl_name auth.ivymect.in;
            # kanidm doesn't extract an incoming traceparent (checked: its
            # own spans always root a fresh trace, so nginx.nix's
            # `otel_trace_context propagate` can't actually join the two)
            # -- but it does echo a per-request X-Kanidm-Opid header that
            # equals the `kopid` attribute on its own root span. Scoped to
            # this location rather than global: nowhere else proxies to
            # kanidm, so `$upstream_http_x_kanidm_opid` would just be a
            # silently-empty attribute everywhere else.
            otel_span_attr "kanidm.opid" "$upstream_http_x_kanidm_opid";
          '';
        };
      };

      # oauth2-proxy's own endpoints. The `/oauth2/` location is added by the
      # oauth2-proxy nginx module; this just gives it a vhost with a cert.
      # Deliberately NOT under auth.ivymect.in -- kanidm serves its OIDC
      # endpoints under /oauth2/ too, and they would collide.
      "sso.ivymect.in" = {
        useACMEHost = "ivymect.in";
        forceSSL = true;

        # The callback sets the session cookie, which is the ID token -- large
        # enough with a groups claim to overrun nginx's default proxy buffers on
        # the way back from oauth2-proxy.
        extraConfig = ''
          proxy_buffer_size 16k;
          proxy_buffers 8 16k;
          proxy_busy_buffers_size 32k;
        '';

        # A bare /oauth2/sign_out loops: the module's /oauth2/ location sets
        # `X-Auth-Request-Redirect` to the current URL, which for sign_out is
        # sign_out itself, so oauth2-proxy redirects there forever. It only
        # terminates with an explicit `rd`, so give it one.
        locations."= /logout".return = "302 /oauth2/sign_out?rd=https%3A%2F%2Fsso.ivymect.in%2Fsigned-out";

        locations."= /signed-out" = {
          extraConfig = ''
            default_type text/html;
            return 200 '<!doctype html><meta charset=utf-8><title>Signed out</title><body style="font:16px/1.5 system-ui,sans-serif;max-width:34em;margin:4em auto;padding:0 1em"><h1>Signed out</h1><p>Your session has been cleared. Open any service again to sign back in.</body>';
          '';
        };
      };
    };

    provision = { scoped, ... }: {
      groups.media-users = { };

      # kanidm's own admin group already exists, so this entry adds a member to
      # it rather than declaring it. `overwriteMembers = false` is not optional:
      # the default REPLACES the member list, which would evict the built-in
      # `idm_admin` account and leave nobody able to recover the instance.
      #
      # `system_admins` is deliberately absent. kanidm splits its admin roles --
      # `idm_admin` owns identity (people, groups, oauth2), `admin` owns the
      # server (domain rename, replication, recovery) -- and provisioning runs as
      # `idm_admin`, so a write to `system_admins` is denied outright:
      # "requested_pres: {Member} !⊆ allowed: {}". Membership there has to be
      # granted by hand as `admin`, and is rarely what you actually want.
      groups.idm_admins = {
        members = [ "auscyber" ];
        overwriteMembers = false;
      };
      # One person, named for the unix account it lines up with -- NSS serves
      # the same name from kanidm, and samba's passdb syncs to it.
      persons.auscyber = {
        displayName = "Ivy Pierlot";
        mailAddresses = [ "ivyp@outlook.com.au" ];
        groups = [
          "media-users"
          "idm_admins"
        ];
      };

      systems.oauth2.oauth2-proxy = {
        displayName = "Media services";
        originUrl = "https://sso.ivymect.in/oauth2/callback";
        # Where kanidm's Apps tile sends you. NOT sso.ivymect.in -- that vhost
        # exists only to host oauth2-proxy's /oauth2/ endpoints and serves
        # nothing at /, so the tile was a dead end. The dashboard is the sensible
        # landing page, and it is behind the same gate.
        originLanding = "https://homepage.ivymect.in/";
        basicSecretFile = scoped.sso.secrets.oauth2-client-secret.path;
        preferShortUsername = true;
        # This scope map is the first gate: kanidm refuses to issue a token to
        # anyone outside `media-users`. The second is oauth2-proxy's per-vhost
        # `allowed_groups`, which needs the groups to actually arrive in the
        # token -- hence the scope below.
        #
        # `groups_name`, not `groups`: kanidm offers `groups` (uuid AND spn),
        # `groups_spn`, and `groups_name` -- only the last yields bare names like
        # `media-users`, which is what the gateway puts in `allowed_groups`. The
        # SPN forms would arrive as `media-users@auth.ivymect.in` and never match.
        scopeMaps.media-users = [
          "openid"
          "profile"
          "email"
          "groups_name"
        ];
      };
    };

    nixos =
      {
        config,
        pkgs,
        lib,
        scoped,
        ...
      }:
      {
        # --- kanidm (identity provider) ---
        #
        # Passkeys: kanidm is WebAuthn-first and needs no switch turned on, but
        # it derives the WebAuthn relying-party ID from `domain` and checks the
        # browser's origin against `origin` exactly -- so those two, the https
        # front, and `http_client_address_info` (nginx terminates TLS, kanidm has
        # to believe it about the client) are the whole server-side story.
        # Changing `domain` later invalidates every enrolled passkey.
        services.kanidm = {
          server.enable = true;
          # Pinned, not left to the stateVersion default, because kanidm only
          # upgrades one minor at a time -- an unpinned jump would strand the
          # database. 1.11 specifically because it is the only branch not past
          # end-of-life: 1.9 EOL'd 2026-05-31 and 1.10 on 2026-08-31, and
          # nixpkgs turns an EOL branch into `knownVulnerabilities`, so the
          # older pins fail to evaluate. Starting here is fine because this is a
          # fresh instance; an existing one would have to walk up minor by minor.
          #
          # `.withSecretProvisioning` is required by `basicSecretFile` below;
          # the plain package asserts against it.
          package = pkgs.kanidm_1_11.withSecretProvisioning;

          serverSettings = {
            domain = "auth.ivymect.in";
            origin = "https://auth.ivymect.in";
            bindaddress = "127.0.0.1:8443";
            # Replaced `trust_x_forward_for` in newer kanidm, and it takes the
            # addresses to trust rather than a bool -- so only nginx on loopback
            # can claim a client IP, not anything that reaches the port.
            http_client_address_info."x-forward-for" = [ "127.0.0.1" ];
            # Traces to tempo, which grafana reads. Needs the scheme: kanidm
            # hands this straight to tonic's OTLP exporter, which parses it as
            # a URI. A bare host:port is accepted at config-parse time (this
            # is what "rejects a scheme" used to be based on) but the
            # exporter then fails to connect with no error surfaced anywhere
            # kanidm logs -- it just never ships a span.
            otel_grpc_endpoint = "http://127.0.0.1:4317";
            tls_chain = "/var/lib/acme/ivymect.in/fullchain.pem";
            tls_key = "/var/lib/acme/ivymect.in/key.pem";
          };

          # People, groups and OAuth2 clients come in through the `provision`
          # class (see ../../services/gateway.nix), so another aspect can add
          # its own client without editing this file.
          provision = {
            enable = true;
            idmAdminPasswordFile = scoped.sso.secrets.idm-admin.path;
          };
        };

        # A person's credentials are held by the person, so enrolling a passkey
        # is interactive by construction and cannot be provisioned. After the
        # first deploy, on the host:
        #
        #   kanidm login -D idm_admin            # password: the idm-admin secret
        #   kanidm person credential create-reset-token auscyber
        #
        # then open the printed link and register the passkey. To make passkeys
        # the only accepted credential rather than one option among several:
        #
        #   kanidm group account-policy credential-type-minimum media-users passkey
        #
        # That is an account-policy call, and kanidm-provision's schema has no
        # account-policy entity, so it stays a one-off rather than living here.
        #
        # POSIX attributes are in the same boat -- the schema has no uid/gid/shell
        # either -- so an account only becomes visible to NSS after:
        #
        #   kanidm group posix set media-users
        #   kanidm person posix set auscyber
        services.kanidm = {
        };

        # kanidm reads the ACME material straight off disk, so it needs to be in
        # the cert's group and to restart when the cert is renewed.
        users.users.kanidm.extraGroups = [ config.security.acme.certs."ivymect.in".group ];
        security.acme.certs."ivymect.in".reloadServices = [ "kanidm.service" ];
        # `server.settings` is a freeform TOML submodule -- it declares almost
        # nothing, so a key upstream renamed, removed or version-gated evaluates
        # cleanly and only fails when kanidmd parses its own config at startup.
        # That is how `trust_x_forward_for` (a v1 key, on a v2 config) reached a
        # deploy.
        #
        # `kanidmd configtest` is the authority on that, so run it at BUILD time
        # over the same settings. It checks file accessibility as well as the
        # schema, hence the sandbox paths and the throwaway certificate: what is
        # under test is the KEY NAMES, not whether /var/lib/acme is populated.
        system.checks = [
          (
            let
              kcfg = config.services.kanidm;
              toml = (pkgs.formats.toml { }).generate "server-configtest.toml" (
                lib.converge (lib.filterAttrsRecursive (_: v: v != null)) (
                  kcfg.server.settings
                  // {
                    tls_chain = "@sandbox@/chain.pem";
                    tls_key = "@sandbox@/key.pem";
                    db_path = "@sandbox@/kanidm.db";
                  }
                )
              );
            in
            pkgs.runCommand "kanidm-configtest" { nativeBuildInputs = [ kcfg.package ]; } ''
              install -m600 ${toml} server.toml
              substituteInPlace server.toml --replace-fail '@sandbox@' "$PWD"

              # kanidm's own throwaway cert, so this needs nothing but kanidm.
              kanidmd cert-generate -c server.toml >/dev/null 2>&1 || true

              # NOT `out=` -- that shadows the output path nix puts in $out, and
              # the `touch` below then tries to create a file named after
              # configtest's output.
              if ! report=$(kanidmd configtest -c server.toml 2>&1); then
              	echo "kanidm rejected the generated server.toml:" >&2
              	echo "$report" >&2
              	exit 1
              fi
              touch "$out"
            ''
          )
        ];

        # --- kanidm as the host's POSIX identity source ---
        #
        # NSS and PAM resolve users and groups out of kanidm, so a share can say
        # `@media-users` and file ownership lines up without a local account
        # being declared anywhere. Note what this does NOT give you: SMB
        # authentication still needs an NT hash, and kanidm's LDAP gateway is
        # read-only with no Samba schema, so `ldapsam` is not an option -- see
        # samba.nix.
        services.kanidm = {
          unix.enable = true;
          clientSettings.uri = "https://auth.${config.gateway.domain}";
          # Nested under `kanidm.` -- the flat spelling is a renamed option and
          # asserts rather than warning.
          unix.settings.kanidm.pam_allowed_login_groups = [ "media-users" ];
        };

        # kanidm-unixd talks to the server on this same box. Resolving the
        # public name to loopback keeps that off the LAN and away from hairpin
        # NAT, while still matching the wildcard certificate kanidm serves.
        networking.hosts."127.0.0.1" = [ "auth.${config.gateway.domain}" ];

        # --- oauth2-proxy (nginx forward-auth) ---
        services.oauth2-proxy = {
          enable = true;
          provider = "oidc";
          oidcIssuerUrl = "https://auth.ivymect.in/oauth2/openid/oauth2-proxy";
          redirectURL = "https://sso.ivymect.in/oauth2/callback";
          # `groups_name` is load bearing: without a groups scope kanidm emits no
          # groups claim, oauth2-proxy sees an empty group set, and every
          # per-vhost `allowed_groups` check fails with a bare 403 -- which nginx
          # renders as a near-blank page, since only 401 is wired to the login
          # redirect.
          scope = "openid profile email groups_name";
          # Authorisation is kanidm's scope map, not an email allowlist here.
          email.domains = [ "*" ];
          setXauthrequest = true;
          reverseProxy = true;
          # One cookie for every *.ivymect.in vhost, which is what makes a
          # single login cover all of them.
          cookie.domain = ".ivymect.in";
          keyFile = scoped.sso.templates."oauth2-proxy.env".path;
          extraConfig = {
            provider-display-name = "kanidm";
            code-challenge-method = "S256";
            whitelist-domain = ".ivymect.in";
            # A machine caller presents a bearer token rather than a session
            # cookie. With this, oauth2-proxy validates that token against
            # kanidm and applies the same per-vhost `allowed_groups` it applies
            # to a browser -- one authorisation rule, both kinds of caller, and
            # the check is a kanidm-issued identity rather than a local secret.
            skip-jwt-bearer-tokens = true;
          };

          nginx = {
            domain = "sso.ivymect.in";
            # Services with no API of their own. The rest arrive from their
            # `gated` declarations in media.nix.
            virtualHosts = lib.genAttrs [
              "aria2.ivymect.in"
              "slsk.ivymect.in"
              "logs.ivymect.in"
            ] (_: { });
          };
        };

        # Neither is implied by `services.oauth2-proxy.enable` on its own.
        # `oidcIssuerUrl` above is the PUBLIC https:// URL, not kanidm's
        # loopback port, so oauth2-proxy's own startup (it fetches the OIDC
        # discovery document immediately) goes through nginx to reach kanidm
        # -- both have to be up, not just kanidm, or that fetch fails and
        # oauth2-proxy comes up with no working provider until its next
        # restart.
        systemd.services.oauth2-proxy = {
          after = [
            "nginx.service"
            "kanidm.service"
          ];
          wants = [
            "nginx.service"
            "kanidm.service"
          ];
          # See kanidm.service below for why. oauth2-proxy's own module
          # already sets Restart=always and its own startLimit*; mkForce
          # the latter two, or eval fails on the conflicting definitions
          # (Restart itself needs no override -- "always" is already at
          # least as aggressive as "on-failure").
          serviceConfig.RestartSec = lib.mkForce "2s";
          startLimitIntervalSec = lib.mkForce 120;
          startLimitBurst = lib.mkForce 30;
        };

        # agenix's own activation script restarts a secret's `restartUnits`
        # BEFORE it chowns the freshly-decrypted files to their configured
        # owner (chown is deliberately deferred until after NixOS's own
        # `users`/`groups` activation, in case the owner is a user being
        # created in this same switch -- see
        # /nix/store/*-agenix-patched/modules/age.nix, `agenixChown.deps`).
        # kanidm's ExecStartPost reads `sso/idm-admin` immediately on start,
        # so every switch that touches it races kanidm's restart against
        # that chown and can lose -- observed taking up to ~2 minutes to
        # resolve on a busy switch. systemd's bare defaults
        # (StartLimitBurst=5 in 10s) don't give it enough attempts to
        # outlast that; this does, so it self-heals instead of dying
        # start-limit-hit and needing a manual `systemctl reset-failed`.
        systemd.services.kanidm.serviceConfig = {
          Restart = "on-failure";
          RestartSec = "2s";
        };
        systemd.services.kanidm.startLimitIntervalSec = 120;
        systemd.services.kanidm.startLimitBurst = 30;

        # Click-through from an nginx span straight to kanidm's own trace
        # for that request, rather than hunting by timestamp -- see the
        # `otel_span_attr "kanidm.opid"` above for the value both sides
        # agree on. `grafana.tempoCorrelations` is a bare extension point
        # (grafana.nix); the correlation is kanidm-specific, so it's
        # declared here, next to the config it depends on.
        grafana.tempoCorrelations = [
          {
            targetUID = "tempo";
            label = "kanidm trace (opid-matched)";
            description = "kanidm roots its own trace per request; this is the same request by kopid, not the same trace.";
            # `type` nests under `config`, not a sibling of it -- confirmed
            # against grafana's own devenv/datasources.yaml example.
            config = {
              type = "query";
              field = "kanidm.opid";
              target = {
                queryType = "traceql";
                # Bare `kopid` is invalid TraceQL ("unknown identifier") --
                # confirmed by hand: attribute references have to be scoped.
                # `.kopid` matches regardless of scope, which is what
                # kanidm's own span actually uses (a plain span attribute,
                # not under resource.*).
                query = "{.kopid=\"\${__value.raw}\"}";
              };
            };
          }
        ];
      };
  };
}
