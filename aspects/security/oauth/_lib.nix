{ lib }:
rec {
  # OAuth 2.0 authorization-code tokens, minted by an agenix-rekey generator.
  #
  # The generator is INTERACTIVE. That is not a trick: `agenix generate` is a
  # plain `writeShellScriptBin` run via `nix run`, and apps/generate.nix invokes
  # a generator as
  #
  #   { ( <script> ) || die ...; } | <age master encrypt> -o <file>
  #
  # so only stdout is redirected. stdin and stderr are inherited from the
  # terminal, which is exactly the shape a browser-redirect flow needs: the
  # authorization URL goes to stderr for the operator, the loopback listener
  # takes the redirect, and the token lands on stdout as the secret.
  #
  # Every diagnostic must therefore go to stderr, and NOTHING may print the
  # token. `oauth2c --silent` is what guarantees the second half: without it
  # oauth2c pretty-prints the token response (and the PKCE code_verifier) to
  # stderr via pterm.

  # ---------------------------------------------------------------------------
  # Provider presets.
  #
  # `authorizeParams` carries the non-standard authorization parameters a
  # provider needs, which is why ../../../patches/oauth2c/authorize-params.patch
  # exists: oauth2c builds the authorization request from a fixed allowlist of
  # flags and has no passthrough, so `access_type` was simply unreachable.
  # ---------------------------------------------------------------------------

  providers = {
    # Google issues a refresh token ONLY when `access_type=offline` is on the
    # authorization request; `prompt=consent` additionally re-issues one when
    # the user has already consented, which is what makes a re-mint work rather
    # than silently returning an access token on its own.
    google = {
      name = "google";
      authorizationEndpoint = "https://accounts.google.com/o/oauth2/v2/auth";
      tokenEndpoint = "https://oauth2.googleapis.com/token";
      authMethod = "client_secret_post";
      authorizeParams = {
        access_type = "offline";
        prompt = "consent";
      };
      extraScopes = [ ];
      pkce = true;
    };

    microsoft = microsoftTenant "common";
  };

  # Microsoft wants the refresh token requested as a SCOPE rather than as an
  # authorization parameter, so there is nothing provider-specific to pass.
  microsoftTenant = tenant: {
    name = "microsoft";
    authorizationEndpoint = "https://login.microsoftonline.com/${tenant}/oauth2/v2.0/authorize";
    tokenEndpoint = "https://login.microsoftonline.com/${tenant}/oauth2/v2.0/token";
    authMethod = "client_secret_post";
    authorizeParams = { };
    extraScopes = [ "offline_access" ];
    pkce = true;
  };

  # ---------------------------------------------------------------------------
  # Home Assistant.
  #
  # NOT a third-party provider with a preset: Home Assistant is itself the
  # authorization server, and kanidm sits behind it as the identity provider
  # (`auth_oidc`, `automatic_person_creation`). So the browser leg goes
  # HA -> kanidm -> back, and what comes out of HA's /auth/token is an HA
  # session, not the secret. `haLongLivedToken` below spends that session on the
  # real secret.
  #
  # Three ways this is sharper than the Google case, all load-bearing:
  #
  #  1. IT NEEDS BOTH HA AND KANIDM REACHABLE AT `agenix generate` TIME. Google
  #     and Microsoft only need their public endpoints, which are always up and
  #     reachable from anywhere. This one fails if either service is down, or if
  #     the admin host cannot route to them -- a materially stronger
  #     precondition, and the likeliest reason a generate run breaks.
  #
  #  2. IndieAuth, so `client_id` IS A URL and the redirect URI must share its
  #     host AND port (developers.home-assistant.io/docs/auth_api). That is why
  #     `haClient` derives both from one port rather than letting them be set
  #     separately: they cannot legally disagree.
  #
  #     UNVERIFIED, AND THE MOST LIKELY THING TO BITE: whether HA accepts a
  #     `http://127.0.0.1:<port>/` client_id without trying to FETCH it for
  #     rel="redirect_uri" link tags. HA has localhost handling in
  #     homeassistant/components/auth/indieauth.py, but it was not confirmed
  #     against a running instance here, and nothing in this tree exercises it.
  #     If the authorization request is rejected, this is the first suspect.
  #
  #  3. There is no client secret at all, so tier 1 vanishes -- this is
  #     structurally the public-client shape. `clientSecret` stays null and
  #     `--auth-method none` is correct rather than a weakening.
  haClient = port: {
    clientId = "http://127.0.0.1:${toString port}/";
    inherit port;
  };

  homeassistant = url: {
    name = "homeassistant";
    authorizationEndpoint = "${url}/auth/authorize";
    tokenEndpoint = "${url}/auth/token";
    # Never consulted: `tokenSecret` passes `--auth-method none` whenever
    # `clientSecret` is null, which for HA it always is.
    authMethod = "none";
    authorizeParams = { };
    extraScopes = [ ];
    # HA's authorization-code flow is IndieAuth-derived and its PKCE support was
    # not confirmed here. oauth2c would send `code_verifier` on the exchange
    # regardless of whether HA stored a challenge, so this is off: an ignored
    # challenge with a sent verifier is the kind of mismatch that produces an
    # opaque `invalid_request`. Turn it on once HA is known to honour it --
    # which it should, and which would be a strict improvement.
    pkce = false;
  };

  # The HA-specific half: trade the session for a long-lived access token.
  #
  # Home Assistant's frontend does this over the WebSocket API, and there is no
  # REST equivalent -- `auth/long_lived_access_token` is a websocket_api command
  # (homeassistant/components/auth/__init__.py) gated by `ws_require_user`, so
  # the connection must be authenticated first. The handshake is
  #
  #   server: {"type":"auth_required","ha_version":...}
  #   client: {"type":"auth","access_token":...}         <- the OAuth token
  #   server: {"type":"auth_ok"} | {"type":"auth_invalid","message":...}
  #   client: {"id":1,"type":"auth/long_lived_access_token",
  #            "client_name":...,"lifespan":<days>}
  #   server: {"id":1,"type":"result","success":true,"result":"<THE SECRET>"}
  #
  # `lifespan` is in DAYS and HA's own UI offers ten years. Which brings up the
  # thing to read before turning any of this on:
  #
  #   RE-MINTING DOES NOT REPLACE THE OLD TOKEN. HA keeps a server-side
  #   refresh-token record per long-lived token -- that is how it validates one
  #   -- and issuing a new one does not revoke the previous one, which stays
  #   valid for the rest of its ten years. So every `derivedFrom`-triggered
  #   regeneration ACCUMULATES a still-valid credential that nothing in this
  #   repo tracks and that must be revoked by hand, via POST /auth/revoke or
  #   the user's profile page.
  #
  #   That is materially worse than Google and Microsoft, where re-consent
  #   supersedes the previous grant. Treat a scope or lifespan edit here as
  #   "mint a new credential and go revoke the old one", not as a rotation.
  haLongLivedToken =
    {
      url,
      clientName,
      lifespan,
    }:
    { pkgs, lib }:
    let
      wsUrl = "${lib.replaceStrings [ "https://" "http://" ] [ "wss://" "ws://" ] url}/api/websocket";

      # `first(...)` short-circuits, so jq prints the one frame that matters and
      # exits rather than waiting for a socket that is never closed from the
      # other end. `auth_invalid` is matched too, so a rejected session fails
      # here instead of hanging until the browser timeout.
      #
      # Not `select(...) | halt`: `halt` exits BEFORE the value is written, so
      # that reads naturally and emits nothing at all.
      pick = ''first(inputs | select(.type == "result" or .type == "auth_invalid"))'';

      unwrap = ''
        if .type == "auth_invalid" then
          "home assistant rejected the oauth session for the websocket api"
            | halt_error(5)
        elif (.success // false) | not then
          "home assistant refused auth/long_lived_access_token"
            | halt_error(6)
        elif (.result // "") == "" then
          "home assistant returned an empty long-lived token"
            | halt_error(7)
        else
          .result
        end
      '';
    in
    ''
      # stdin is HA's own token response. The access token is a session, not the
      # secret, so it goes into the environment rather than onto a command line
      # where `ps` would show it.
      HA_SESSION="$(${lib.getExe pkgs.jq} -r '.access_token // ""')"
      export HA_SESSION
      if [[ -z "$HA_SESSION" ]]; then
        echo "oauth-token: home assistant returned no access_token" >&2
        exit 4
      fi

      echo "oauth-token: asking home assistant for a long-lived token (${toString lifespan}d)" >&2

      # Both frames are written up front. HA reads the auth frame first and only
      # then the command, so pipelining is safe and saves having to synchronise
      # on auth_ok. --no-close keeps the socket open after stdin ends, which is
      # what gives HA a chance to answer at all.
      #
      # `--max-messages-rev 3` is what makes this TERMINATE, and the 3 is the
      # exact number of frames HA sends on the happy path: auth_required,
      # auth_ok, result. Without it websocat sits forever -- HA never closes the
      # socket, and jq exiting does not disturb a websocat that has nothing left
      # to write, so no SIGPIPE ever arrives and the generator hangs until the
      # operator kills it. (Measured: 0.14s with the bound, indefinite without.)
      #
      # The failure mode if HA ever interposes an extra frame is safe rather
      # than wrong: three frames arrive without a `result`, jq emits nothing,
      # and the empty-reply check below fails the generator. It cannot mint a
      # bogus secret. On the auth_invalid path HA closes the socket itself after
      # two frames, so the bound is never reached and websocat exits on EOF.
      ha_reply="$(
        {
          ${lib.getExe pkgs.jq} -nc '{ type: "auth", access_token: $ENV.HA_SESSION }'
          ${lib.getExe pkgs.jq} -nc \
            --arg name ${lib.escapeShellArg clientName} \
            --argjson lifespan ${toString lifespan} \
            '{ id: 1, type: "auth/long_lived_access_token", client_name: $name, lifespan: $lifespan }'
        } \
          | ${lib.getExe pkgs.websocat} --no-close --max-messages-rev 3 \
              -t ${lib.escapeShellArg wsUrl} \
          | ${lib.getExe pkgs.jq} -n -c --unbuffered ${lib.escapeShellArg pick}
      )" || true
      # `|| true` because jq exiting first leaves websocat writing into a closed
      # pipe, and `pipefail` would turn that SIGPIPE into a failure of the whole
      # generator. Whether this worked is decided by the reply, below -- not by
      # the exit status of a pipeline that is expected to end this way.

      if [[ -z "$ha_reply" ]]; then
        echo "oauth-token: no reply from home assistant -- it may be unreachable," >&2
        echo "oauth-token: or it closed the websocket without answering." >&2
        exit 8
      fi

      printf '%s' "$ha_reply" \
        | ${lib.getExe pkgs.jq} -r ${lib.escapeShellArg unwrap} \
        | ${pkgs.coreutils}/bin/tr -d '\n'
    '';

  # ---------------------------------------------------------------------------
  # Tier 1: the client credentials, per (provider, consuming service).
  #
  # `client_id` is NOT a secret -- it travels in the authorization URL that the
  # operator's browser visits -- so it lives in Nix as a plain string, which is
  # also what lets `derivedFrom` track its VALUE rather than merely which secret
  # it is.
  #
  # The client SECRET is a static, operator-entered age file declared
  # `intermediary = true`, so agenix-rekey never encrypts it to a host key and a
  # dummy is deployed instead. It reaches the generator only as a
  # `dependencies` edge, decrypted with the master identity while `agenix
  # generate` runs -- the same arrangement ../../base/celler/_lib.nix uses for a
  # server's RS256 signing key, and for the same reason.
  # ---------------------------------------------------------------------------
  clientSecret =
    { provider, service }:
    {
      rekeyFile = ../../../secrets/static/oauth + "/${provider}-${service}-client-secret.age";
      intermediary = true;
    };

  # ---------------------------------------------------------------------------
  # Tier 2: one refresh token per (provider, service, person).
  #
  # The path is person-keyed and written out explicitly, because
  # `age.rekey.generatedSecretsDir` is keyed by HOST: left to default, the same
  # person's token would resolve to a different file on every host they have an
  # account on, and each would demand its own browser login. A fixed path makes
  # every host's declaration name one file, which agenix-rekey dedups.
  #
  # Setting `rekeyFile` opts out of the `derivedFrom` name suffix that
  # ../../../patches/agenix-rekey/derivedFrom.patch appends, so staleness rides
  # entirely on the `<file>.derived-from` stamp -- regeneration IN PLACE rather
  # than under a new name, which is what a person-keyed path wants anyway.
  # ---------------------------------------------------------------------------
  tokenSecret =
    {
      provider,
      service,
      sub,
      clientId,
      clientSecret ? null,
      scopes,
      # What turns oauth2c's token response into the secret. Defaults to the
      # x/oauth2 shape rclone wants; `haLongLivedToken` is the other one in
      # tree. See `postProcess` on `tokenScript`.
      postProcess ? refreshTokenJSON,
      # Anything else this secret is a function of, folded into `derivedFrom`.
      # A `postProcess` with parameters of its own (Home Assistant's client name
      # and lifespan) belongs here, or changing them would not re-mint.
      extraDerivedFrom ? { },
      # Fixed, because both providers match a registered redirect URI exactly.
      # Google's loopback option and Microsoft's localhost exception both relax
      # the PORT specifically, but neither relaxes the rest of the URI, and a
      # port that moves between runs is a redirect_uri_mismatch waiting to
      # happen. Pin it, register it once, and keep it out of `derivedFrom` --
      # moving the listener does not invalidate a token.
      port ? 49152,
    }:
    let
      allScopes = lib.unique (scopes ++ provider.extraScopes);

      # WHAT THIS TOKEN IS A FUNCTION OF. A scope set that grows needs fresh
      # consent, a different client issues a token the old one cannot refresh,
      # and a different person is a different token outright -- so each of these
      # changing must re-run the flow.
      #
      # Endpoints and `port` are deliberately absent: they are how the token is
      # obtained, not what it authorises.
      derivedFrom = {
        provider = provider.name;
        scopes = allScopes;
        inherit service sub clientId;
      }
      // extraDerivedFrom;
    in
    {
      rekeyFile =
        ../../../secrets/generated/oauth + "/${provider.name}-${service}-${sub}-refresh-token.age";
      generator = {
        tags = [ "oauth_token" ];
        inherit derivedFrom;
      }
      // lib.optionalAttrs (clientSecret != null) {
        dependencies.client_secret = clientSecret;
      }
      // {
        script = tokenScript {
          inherit
            provider
            clientId
            port
            postProcess
            ;
          scopes = allScopes;
          confidential = clientSecret != null;
        };
      };
    };

  # The generator proper.
  tokenScript =
    {
      provider,
      clientId,
      scopes,
      port,
      confidential,
      postProcess,
    }:
    {
      pkgs,
      lib,
      decrypt,
      deps,
      ...
    }:
    let
      redirectUrl = "http://127.0.0.1:${toString port}/callback";

      # Joined as a list rather than written as a backslash-continued block:
      # a conditional or an empty attrset in the middle of a continued command
      # leaves a line holding nothing but whitespace, which ENDS the command and
      # turns everything after it into a separate one.
      invocation = lib.concatStringsSep " \\\n        " (
        # args[0] is a client-config FILE, so the client secret is never an
        # argv entry visible to `ps` on the admin host. `oauth2c` only reads
        # client_id / client_secret / openid_discovery_endpoint from it, and
        # flags take precedence over the file, so the file carries the secret
        # and nothing else of consequence.
        [ ''${lib.getExe pkgs.oauth2c} "$cfg"'' ]
        ++ [
          # Both endpoints explicit: `ServerConfig.IsConfigured` is an OR over
          # them, so naming either one skips OpenID discovery entirely.
          "--authorization-endpoint ${lib.escapeShellArg provider.authorizationEndpoint}"
          "--token-endpoint ${lib.escapeShellArg provider.tokenEndpoint}"
          "--grant-type authorization_code"
          "--response-types code"
          "--auth-method ${lib.escapeShellArg (if confidential then provider.authMethod else "none")}"
          "--client-id ${lib.escapeShellArg clientId}"
          "--redirect-url ${lib.escapeShellArg redirectUrl}"
          "--callback-addr ${lib.escapeShellArg "127.0.0.1:${toString port}"}"
          "--no-browser"
          "--no-prompt"
          # --silent is load-bearing, not cosmetic: without it oauth2c
          # pretty-prints the whole token response AND the PKCE code_verifier to
          # stderr. With it, the only thing on stderr is the authorization URL
          # (which --no-browser asks for) and errors.
          "--silent"
        ]
        # Omitted entirely when empty rather than passed as "": oauth2c treats a
        # single empty element as a scope and would send `scope=`. Home
        # Assistant takes no scopes at all, so it must send no parameter.
        ++ lib.optional (scopes != [ ]) "--scopes ${lib.escapeShellArg (lib.concatStringsSep "," scopes)}"
        # `or true`: PKCE on unless a provider says otherwise, so a provider
        # record that forgets the field gets the safe answer rather than an
        # eval error. Only Home Assistant opts out, and it says why.
        ++ lib.optional (provider.pkce or true) "--pkce"
        ++ lib.mapAttrsToList (
          k: v: "--authorize-param ${lib.escapeShellArg "${k}=${v}"}"
        ) provider.authorizeParams
      );
    in
    ''
      # A browser flow needs an operator. With no terminal there is nobody to
      # open the URL, so fail now with instructions rather than blocking until
      # oauth2c's callback timeout.
      if [[ ! -t 2 ]]; then
        echo "oauth-token: needs an interactive terminal (stderr is not a tty)." >&2
        echo "oauth-token: run \`nix run .#gen-secrets -- -t oauth_token\` from a shell." >&2
        exit 1
      fi

      cfg="$(${pkgs.coreutils}/bin/mktemp)"
      ${pkgs.coreutils}/bin/chmod 600 "$cfg"
      trap '${pkgs.coreutils}/bin/rm -f "$cfg"' EXIT

      ${
        if confidential then
          ''
            ${decrypt} ${lib.escapeShellArg deps.client_secret.file} \
              | ${lib.getExe pkgs.jq} -Rs \
                  --arg issuer ${lib.escapeShellArg provider.authorizationEndpoint} \
                  '{ openid_discovery_endpoint: $issuer, client_secret: rtrimstr("\n") }' \
              > "$cfg"
          ''
        else
          ''
            ${lib.getExe pkgs.jq} -n \
              --arg issuer ${lib.escapeShellArg provider.authorizationEndpoint} \
              '{ openid_discovery_endpoint: $issuer }' > "$cfg"
          ''
      }

      echo "oauth-token: ${provider.name} -- client ${clientId}" >&2
      echo "oauth-token: open the URL below; the redirect lands on ${redirectUrl}" >&2

      ${invocation} \
        | (
      ${postProcess { inherit pkgs lib; }}
        )
    '';

  # ---------------------------------------------------------------------------
  # `postProcess`: stdin is oauth2c's token response, stdout is the secret.
  #
  # Splitting this out is what makes the generator provider-shaped rather than
  # rclone-shaped. For Google and Microsoft the thing wanted IS the token, so
  # the step is a reshape. For Home Assistant the OAuth token is only a session
  # with which to ask for the real secret, so the step is a round trip.
  #
  # Both run with `set -euo pipefail` inherited from `agenix generate`, so a
  # non-zero exit anywhere aborts before anything is encrypted and leaves the
  # previous secret on disk untouched.
  # ---------------------------------------------------------------------------

  # The default: the shape rclone stores in its config `token` key, which is a
  # json.Marshal of golang.org/x/oauth2.Token -- note `expiry`, an RFC3339
  # instant, NOT the `expires_in` duration a token endpoint returns, so the
  # duration is resolved against the clock here.
  #
  # A response with no refresh_token is a HARD failure. It is the one outcome
  # that looks like success and is not: the secret would hold an access token
  # that dies within the hour with nothing able to renew it.
  refreshTokenJSON =
    { pkgs, lib }:
    let
      filter = ''
        if (.refresh_token // "") == "" then
          "no refresh_token in the token response -- the provider did not grant offline access"
            | halt_error(3)
        else
          {
            access_token,
            token_type: (.token_type // "Bearer"),
            refresh_token,
            expiry: ((now + (.expires_in // 3600)) | todate),
          }
        end
      '';
    in
    ''
      ${lib.getExe pkgs.jq} -c ${lib.escapeShellArg filter} \
        | ${pkgs.coreutils}/bin/tr -d '\n'
    '';
}
