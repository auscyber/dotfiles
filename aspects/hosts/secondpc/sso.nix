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
      };
    };

    provision = { scoped, ... }: {
      groups.media-users = { };

      # kanidm's own admin groups already exist, so these entries add a member to
      # them rather than declaring them. `overwriteMembers = false` is not
      # optional here: the default REPLACES the member list, which would evict
      # the built-in `idm_admin` account and leave nobody able to recover the
      # instance.
      groups.idm_admins = {
        members = [ "auscyber" ];
        overwriteMembers = false;
      };
      groups.system_admins = {
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
          "system_admins"
        ];
      };

      systems.oauth2.oauth2-proxy = {
        displayName = "Media services";
        originUrl = "https://sso.ivymect.in/oauth2/callback";
        originLanding = "https://sso.ivymect.in/";
        basicSecretFile = scoped.sso.secrets.oauth2-client-secret.path;
        preferShortUsername = true;
        # This scope map IS the authorisation gate: kanidm refuses to issue a
        # token to anyone outside `media-users`, so oauth2-proxy needs no
        # `allowed_groups` of its own.
        scopeMaps.media-users = [
          "openid"
          "profile"
          "email"
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
        # front, and `trust_x_forward_for` (nginx terminates TLS, kanidm has to
        # believe it) are the whole server-side story. Changing `domain` later
        # invalidates every enrolled passkey.
        services.kanidm = {
          enableServer = true;
          # Pinned, not left to the stateVersion default, because kanidm only
          # upgrades one minor at a time -- an unpinned jump would strand the
          # database. `.withSecretProvisioning` is required by `basicSecretFile`
          # below; the plain package refuses it.
          package = pkgs.kanidm_1_11.withSecretProvisioning;

          serverSettings = {
            domain = "auth.ivymect.in";
            origin = "https://auth.ivymect.in";
            bindaddress = "127.0.0.1:8443";
            trust_x_forward_for = true;
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

        # --- kanidm as the host's POSIX identity source ---
        #
        # NSS and PAM resolve users and groups out of kanidm, so a share can say
        # `@media-users` and file ownership lines up without a local account
        # being declared anywhere. Note what this does NOT give you: SMB
        # authentication still needs an NT hash, and kanidm's LDAP gateway is
        # read-only with no Samba schema, so `ldapsam` is not an option -- see
        # samba.nix.
        services.kanidm = {
          enablePam = true;
          clientSettings.uri = "https://auth.${config.gateway.domain}";
          unixSettings.pam_allowed_login_groups = [ "media-users" ];
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
          scope = "openid profile email";
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
      };
  };
}
