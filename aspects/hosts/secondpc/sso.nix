{
  den,
  lib,
  ...
}:
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
    # ── TWO NAMES THIS HOST PUBLISHES THAT NOTHING CAN DERIVE ──
    #
    # `auth.ivymect.in` (kanidm) and `sso.ivymect.in` (oauth2-proxy) are RAW
    # `services.nginx.virtualHosts` entries below, not `takes.proxy`, so
    # ../../services/proxy-nginx.nix's `emits` is false for them and
    # `capability.contributions secondpc "proxy"` does not know they exist --
    # measured: 25 published names come out of that fold and neither of these
    # is among them.
    #
    # Declared as SUBDOMAINS, with the zone still coming from the host's zone
    # carrier (../../network/cloudflare.nix), so this stays true if the zone
    # moves. ../../network/roaming-resolver.nix is the reader today; anything
    # else that wants "which names does this box publish" gets them from the
    # same place rather than from a second list.
    alsoPublishes = [
      "sso"
      "auth"
    ];

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
      # Application passwords that can be PROVISIONED rather than only minted,
      # which is what lets ../../programs/zotero.nix hold ivy's WebDAV
      # credential as an ordinary generated secret. See
      # packages/kanidm-set-application-password.nix -- a from-source build.
      den.aspects.packages.kanidm-set-application-password
    ];

    secrets = {
      # kanidm resets idm_admin to this on every restart when provisioning is
      # on; without it the password is regenerated and printed to the journal.
      idm-admin = {
        owner = "kanidm";
        restartUnits = [ "kanidm.service" ];
        generator.script = "random-hex-32";
      };

      # The OAuth2 client secret. kanidm reads this file directly
      # (`basicSecretFile`); oauth2-proxy gets the same value substituted into
      # its env template below, so the two cannot drift. Hex, and newline
      # stripped, because both sides compare it byte for byte.
      oauth2-client-secret = {
        owner = "kanidm";
        restartUnits = [ "kanidm.service" ];
        generator.script = "random-hex-32";
      };

      # oauth2-proxy's cookie AES key: must be exactly 16, 24 or 32 bytes, so
      # 16 hex bytes rendered as 32 characters.
      oauth2-cookie-secret.generator.script = "random-hex-16";
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

    # The client name must be `oauth2-proxy` while this aspect is `sso`. The
    # `provision` class below stays for the things declared exactly once
    # (`enable`, `idmAdminPasswordFile`) and for the groups and the person. The
    # shared OIDC shape is defaulted by `gateway.oidcClients`.
    kanidmClients.oauth2-proxy = {
      displayName = "Media services";
      subdomain = "sso";
      callback = "/oauth2/callback";
      # Where kanidm's Apps tile sends you. NOT the sso vhost -- that exists
      # only to host oauth2-proxy's /oauth2/ endpoints and serves nothing at
      # /, so the tile was a dead end. The dashboard is the sensible landing
      # page, and it is behind the same gate.
      #
      # A subdomain, not a URL: the `homepage.ivymect.in` literal this used
      # to carry is exactly what makes a service unable to move.
      landingSubdomain = "homepage";
      # Not `oauth2-proxy/oidc-client-secret`: this one predates the
      # convention and lives in the `sso` scope under its own name.
      secretScope = "sso";
      secretName = "oauth2-client-secret";
      # This client's scope map is the FIRST gate: kanidm refuses to issue a
      # token to anyone outside `media-users`. The second is oauth2-proxy's
      # per-vhost `allowed_groups`, which needs the group names to actually
      # arrive in the token -- hence `groupsName`.
      groupsName = true;
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
      #
      # NO `members` LIST. Membership comes from the PERSON
      # (`den.persons.<p>.kanidm.groups`, provisioned by
      # ../../services/kanidm.nix): nixpkgs' module folds every person's
      # `groups` into the group's members, so this entry exists only to carry
      # the flag -- which is exactly what it is for.
      groups.idm_admins.overwriteMembers = false;

      # NO `persons` BLOCK EITHER. It listed one person with their display
      # name, mail address and groups written out a second time, while
      # ../../base/person.nix's registry -- which already holds all three --
      # described people kanidm had never heard of. ../../services/kanidm.nix
      # provisions from the registry now, so a person is declared in
      # ../../persons/<name>.nix and nowhere else.
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

            # ── THE LDAP GATEWAY ──
            #
            # Off until something needed it; ../../services/mail.nix does --
            # dovecot authenticates a mailbox by BINDING as that person, which
            # is the only authentication kanidm's gateway can serve (it is
            # read-only and hands out no password material, which is also why
            # samba still needs its own NT hash -- see samba.nix).
            #
            # LOOPBACK, and the name is what makes that work: kanidm serves
            # LDAPS here under the same wildcard certificate as everything
            # else, so a client has to dial a name the certificate covers.
            # `networking.hosts` below already points `auth.ivymect.in` at
            # 127.0.0.1 for kanidm-unixd, so `ldaps://auth.ivymect.in:3636`
            # both resolves locally and verifies -- where a bare `127.0.0.1`
            # URI would fail the hostname check.
            #
            # 3636 is kanidm's own default for LDAPS. The port is declared in
            # the capability layer on `den.aspects.kanidm` (not exposed), so
            # ../../base/firewall.nix leaves it shut.
            ldapbindaddress = "127.0.0.1:3636";
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
            # alloy, not tempo directly: otel-fail2ban.nix taps this stream
            # (same as nginx's) to feed fail2ban, then forwards it to tempo.
            otel_grpc_endpoint = "http://127.0.0.1:4319";
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

          # ── SSH LOGIN FOR SOMEBODY WITH NO LOCAL ACCOUNT ──
          #
          # This is the consuming half of
          # ../../../patches/kanidm-provision/ssh-keys.patch. nixpkgs wires it
          # for us: `unix.sshIntegration` sets
          # `services.openssh.settings.AuthorizedKeysCommand` to
          # `kanidm_ssh_authorizedkeys %u` behind a setuid wrapper, so sshd asks
          # kanidm for a user's keys instead of reading
          # `~/.ssh/authorized_keys`.
          #
          # WHAT IT UNLOCKS, and it is the thing that was asked for: a person
          # who is a kanidm POSIX user but NOT a den user -- no
          # `den.hosts.<sys>.<host>.users.<n>` entry, no home-manager
          # generation, nothing in this repo naming them per host -- can still
          # log in here, because their keys travel with their identity rather
          # than with a per-host account.
          #
          # It does NOT replace ../../security/main-ssh-key.nix, which writes
          # `openssh.authorizedKeys.keys` for accounts this repo DOES declare.
          # sshd consults both: the file for declared accounts, the command for
          # everyone else. Two channels, one source -- `den.persons.<n>.sshKeys`
          # feeds both (../../services/kanidm.nix's `gateway.personSshKeys`).
          unix.sshIntegration = true;
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

        # agenix now restarts a secret's `restartUnits` only after the chown
        # (patches/agenix/templates.patch, `agenixRestart`), so kanidm's
        # ExecStartPost no longer races the chown of `sso/idm-admin`. Kept
        # as a safety net: systemd's bare defaults (StartLimitBurst=5 in
        # 10s) leave it dying start-limit-hit on any other transient
        # failure, needing a manual `systemctl reset-failed`.
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
