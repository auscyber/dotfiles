# Tailscale, as a peer to ./vpn.nix rather than a replacement for it.
#
# The wireguard tunnel in ./vpn.nix is a hub-and-spoke through `secondpc` at a
# fixed endpoint: every client dials that one box, so when it is down or its
# address moves, nothing reaches anything. Tailscale is here for the cases where
# that is not good enough -- reaching the laptop from a phone on cellular, or
# from behind a NAT that neither end controls -- because it negotiates directly
# and falls back to a DERP relay when it cannot.
#
# Credentials are an OAuth client, not a pre-auth key, which is why nothing here
# stores a key per device. An OAuth client does not expire the way an auth key
# does (90 days at the outside), and keys minted from it are made on demand and
# thrown away -- so a phone is enrolled by minting one at that moment rather
# than by decrypting something that has been sitting in the repo going stale.
#
# BOOTSTRAP:
#
#   1. Make an OAuth client at
#      https://login.tailscale.com/admin/settings/oauth
#      with the `auth_keys` write scope, granted over ${nodeTag}.
#   2. Put this in the tailnet ACL, or step 3 is rejected:
#        "tagOwners": { "${nodeTag}": ["autogroup:admin"] }
#   3. nix run .#secret-edit -- secrets/tailscale_auth.age
#      Two lines, no quotes, no `export`:
#        CLIENT_ID=...
#        CLIENT_SECRET=tskey-client-...
#   4. Rebuild. `tailscale-autoconnect` enrols this machine at boot.
#
# Enrol a phone or anything else this flake does not build:
#
#   nix run .#tailscale-client-key            # asks which device
#   nix run .#tailscale-client-key -- mobile  # or name it
#
# which mints a single-use, preauthorized key from the OAuth client and prints
# it, to paste into the Tailscale app's "Custom auth key" field. If the stored
# client cannot be decrypted (no Yubikey in this checkout), it asks for a
# CLIENT_ID/CLIENT_SECRET instead, so step 3 above is a convenience rather than
# a prerequisite.
#
# Once the phone is on the tailnet, t3code is reachable from it over the
# tailnet's own HTTPS -- see `nix run .#t3code-pair` in
# ../dev/llms/t3code.nix for the pairing link.
{
  den,
  lib,
  ...
}:
let
  # OAuth-minted nodes MUST be tagged -- an OAuth client secret cannot enrol an
  # untagged node, and a tag it does not own is refused. One tag for everything
  # this flake builds; devices enrolled by hand stay untagged and keep their own
  # ACL treatment.
  nodeTag = "tag:nix";

  # Devices that are part of the tailnet but that this flake does not build, so
  # there is nothing to configure for them -- only a key to hand over. Declared
  # rather than free-form so `.#tailscale-client-key` can offer a list and so a
  # typo mints a key described as "mobil" instead of failing.
  #
  # Read out of ../hosts/_registry.nix rather than written here, so a device is
  # declared in the one file that already is the fleet's identity, next to the
  # machines it sits alongside. The registry is a plain data file with no
  # partition scoping (that is the whole point of it), so this stays readable
  # from wherever this aspect is evaluated.
  #
  # Nothing is stored per client: the key is minted at the moment you enrol the
  # device and is single-use, so the list costs nothing to extend -- add a
  # `device` entry to the registry and it appears here.
  registry = import ../hosts/_registry.nix;
  clients = lib.mapAttrs (_: meta: meta.device.description) (
    lib.filterAttrs (_: meta: meta ? device) registry
  );
  clientNames = lib.attrNames clients;

  # Shared by the autoconnect daemon and the key-minting app. Tailscale's OAuth
  # is plain client_credentials, so this is a token exchange followed by one
  # authenticated POST -- no SDK, and nothing that has to be kept in step with a
  # library version.
  #
  # Contract with the caller, kept deliberately small because this is inlined
  # into two different scripts: set `ts_env_file` to the KEY=VALUE file and
  # `ts_description` to whatever should show against the key in the admin
  # console. Sets `authkey` on success and exits non-zero on every failure.
  #
  # Both are shell variables rather than Nix-interpolated strings so the app can
  # point this at a temp file it made at runtime, and so the description can
  # carry the real hostname instead of whatever the module system thinks it is
  # at eval time.
  mintKeyScript = ''
    if [ ! -r "$ts_env_file" ]; then
      echo "tailscale: $ts_env_file is not readable" >&2
      exit 1
    fi

    # Sourced in a subshell-free way but scrubbed immediately after: CLIENT_ID
    # and CLIENT_SECRET are the long-lived credential and have no business
    # staying in the environment of anything this later execs.
    # The source has to sit on its own line with the directive immediately
    # above it. As `set -a; . "$f"; set +a` on one line, shellcheck attaches the
    # disable to `set -a` and still fails the build on SC1090 -- which it does
    # at *build* time, so it passes eval and only bites on switch.
    set -a
    # shellcheck disable=SC1090
    . "$ts_env_file"
    set +a

    if [ -z "''${CLIENT_ID:-}" ] || [ -z "''${CLIENT_SECRET:-}" ]; then
      echo "tailscale: $ts_env_file must set CLIENT_ID and CLIENT_SECRET" >&2
      exit 1
    fi

    # NOT `curl -f`. writeShellApplication sets `errexit`, so a `-f` curl inside
    # a command substitution kills the script AT THE ASSIGNMENT -- which made
    # every diagnostic below this point dead code and turned a real API error
    # into 199 log lines reading only "curl: (22)". Capture the status and the
    # body instead, and say what the API actually objected to.
    token_body="$(mktemp)"
    token_code="$(curl -sS -o "$token_body" -w '%{http_code}' \
      -d "client_id=$CLIENT_ID" \
      -d "client_secret=$CLIENT_SECRET" \
      https://api.tailscale.com/api/v2/oauth/token || echo 000)"

    if [ "$token_code" != "200" ]; then
      echo "tailscale: OAuth token exchange failed (HTTP $token_code)" >&2
      echo "  response: $(head -c 500 "$token_body")" >&2
      echo "  check CLIENT_ID/CLIENT_SECRET in $ts_env_file" >&2
      rm -f "$token_body"
      exit 1
    fi

    access_token="$(jq -r '.access_token // empty' <"$token_body")"
    rm -f "$token_body"

    if [ -z "$access_token" ]; then
      echo "tailscale: token endpoint returned 200 but no access_token" >&2
      exit 1
    fi

    # The key description is validated control-side against a set narrower than
    # JSON's -- parentheses are rejected -- and all you get back is HTTP 400
    # "description had invalid characters", which reads like an auth problem and
    # is not one. Fold anything outside the safe set to a hyphen and cap the
    # length, so a hostname can never wedge the daemon into a retry loop.
    ts_description="$(printf '%s' "$ts_description" | tr -c 'A-Za-z0-9 ._-' '-' | cut -c1-50)"

    # Built with jq rather than a Nix-side toJSON so `ts_description` can be a
    # runtime value, and so the description is escaped by something that
    # actually knows JSON.
    payload="$(jq -n \
      --arg description "$ts_description" \
      --arg tag ${lib.escapeShellArg nodeTag} \
      '{description: $description,
        capabilities: {devices: {create: {
          reusable: false,
          ephemeral: false,
          preauthorized: true,
          tags: [$tag]}}}}')"

    # `-` means "the tailnet these credentials belong to", so this never has to
    # name the tailnet.
    key_body="$(mktemp)"
    key_code="$(curl -sS -o "$key_body" -w '%{http_code}' \
      -H "Authorization: Bearer $access_token" \
      -H "Content-Type: application/json" \
      -d "$payload" \
      https://api.tailscale.com/api/v2/tailnet/-/keys || echo 000)"

    unset CLIENT_ID CLIENT_SECRET access_token

    if [ "$key_code" != "200" ]; then
      echo "tailscale: minting an auth key failed (HTTP $key_code)" >&2
      echo "  response: $(head -c 500 "$key_body")" >&2
      echo "  request:  $payload" >&2
      echo "  check ${nodeTag} is in the ACL's tagOwners and the client has auth_keys write" >&2
      rm -f "$key_body"
      exit 1
    fi

    authkey="$(jq -r '.key // empty' <"$key_body")"
    rm -f "$key_body"

    if [ -z "$authkey" ]; then
      echo "tailscale: key endpoint returned 200 but no key field" >&2
      exit 1
    fi
  '';
in
{
  den.aspects.tailscale = {
    includes = [ den.aspects.agenix-rekey ];

    # The escape hatch lib/age-scoped.nix documents for exactly this shape of
    # wedge, and it is not optional here.
    #
    # A scope's `service` defaults to the aspect name, and the darwin backend
    # infers `restartUnits` from it with `config.launchd.daemons ? tailscale`.
    # That membership test forces the ATTRIBUTE NAMES of `launchd.daemons`,
    # which forces every definition of it -- including agenix's own, whose
    # entire config block is `mkIf (cfg.secrets != { } || ...)`. Evaluating that
    # condition forces `age.secrets`, which is what the backend was computing:
    # age.secrets -> launchd.daemons -> age.secrets, and the host dies with
    # `infinite recursion` from inside agenix, pointing nowhere near here.
    #
    # It bites this aspect and not ./vpn.nix because a scope only reaches for
    # launchd at all on darwin, and `vpn-secrets` is not a name any of this
    # resolves against. Nothing is lost by switching inference off: the only
    # consumer is the `tailscale-autoconnect` daemon below, which reads the
    # credentials at boot rather than needing a reload when they change.
    secretSettings.service = null;

    # Hand-written rather than generated: an OAuth client is minted in the admin
    # console and has no offline derivation, so there is nothing for a generator
    # to compute.
    secrets.auth.rekeyFile = ../../secrets/tailscale_auth.age;

    darwin =
      {
        config,
        pkgs,
        scoped,
        ...
      }:
      let
        envFile = scoped.tailscale.secrets.auth.path;
      in
      {
        services.tailscale.enable = true;

        # nix-darwin's module only runs `tailscaled`; it has no equivalent of
        # NixOS's `services.tailscale.authKeyFile`, so enrolling the node is
        # this daemon's job.
        #
        # It is not `RunAtLoad`-and-exit-once by accident: tailscaled takes a
        # moment to create its socket, and a node that has been logged out (or
        # whose key expired) needs enrolling again without a rebuild. So it
        # waits for the socket, acts only when the backend actually says
        # NeedsLogin, and is a no-op on every other boot.
        launchd.daemons.tailscale-autoconnect = {
          command = "${
            pkgs.writeShellApplication {
              name = "tailscale-autoconnect";
              # writeShellApplication prepends these to PATH, so naming every
              # external command here is what stops the daemon depending on
              # whatever launchd happens to hand it.
              runtimeInputs = [
                config.services.tailscale.package
                pkgs.coreutils
                pkgs.curl
                pkgs.jq
              ];
              text = ''
                for _ in $(seq 1 60); do
                  if tailscale status --json >/dev/null 2>&1; then break; fi
                  sleep 1
                done

                state="$(tailscale status --json 2>/dev/null | jq -r '.BackendState // "NoState"' || echo NoState)"
                if [ "$state" != "NeedsLogin" ] && [ "$state" != "NoState" ]; then
                  echo "tailscale: backend is ''${state:-unknown}, nothing to do"
                  exit 0
                fi

                ts_env_file=${lib.escapeShellArg envFile}
                # `uname -n`, not `hostname`: coreutils is already pinned onto
                # PATH here and ships the former, while `hostname` on darwin
                # lives outside the closure in /usr/bin.
                ts_description="$(uname -n) nix autoconnect"
                ${mintKeyScript}

                # The key reaches tailscaled as an argument, which is visible in
                # the process table -- but it is single-use and preauthorized,
                # so it is spent by the time this returns. The long-lived
                # credential, the OAuth client, never leaves the env file.
                # Not `exec`, because what happens on failure matters. launchd
                # restarts this after ~10s (KeepAlive.SuccessfulExit), and every
                # attempt mints a NEW key -- so a persistent failure, an ACL
                # missing ${nodeTag} being the likely one, would otherwise spray
                # a fresh unused key into the tailnet six times a minute until
                # noticed. Backing off to a minute bounds that, and says plainly
                # what to go and look at.
                if tailscale up \
                  --auth-key "$authkey" \
                  --advertise-tags ${lib.escapeShellArg nodeTag} \
                  --accept-routes
                then
                  echo "tailscale: up as ${nodeTag}"
                  exit 0
                fi

                echo "tailscale: 'tailscale up' failed -- check that ${nodeTag} is in the tailnet ACL's tagOwners and that the OAuth client owns it" >&2
                sleep 60
                exit 1
              '';
            }
          }/bin/tailscale-autoconnect";
          serviceConfig = {
            Label = "com.ivy.tailscale-autoconnect";
            RunAtLoad = true;
            KeepAlive.SuccessfulExit = false;
            StandardErrorPath = "/var/log/tailscale-autoconnect.log";
            StandardOutPath = "/var/log/tailscale-autoconnect.log";
          };
        };
      };
  };

  # Mint a key for a device this flake does not build -- a phone, a tablet, a
  # borrowed machine. Single-use and preauthorized, so it is spent the moment
  # the device enrols and is worth nothing afterwards if it leaks.
  #
  # Reads the OAuth client through the *master* identity rather than the
  # laptop's deployed copy, so this works from any checkout the Yubikey is
  # plugged into, not only from the machine the secret was rekeyed onto.
  perSystem =
    { pkgs, config, ... }:
    {
      # A package as well as an app, deliberately. `apps.<x>.program` is a
      # string, so there is no way to `nix build` it -- and writeShellApplication
      # runs shellcheck in its BUILDER, which means an app-only script is never
      # linted until something realises it. That is how an SC1090 failure in
      # here survived a green `nix eval` of the whole host: eval does not build.
      # `nix build .#tailscale-client-key` now exercises it.
      packages.tailscale-client-key = pkgs.writeShellApplication {
        name = "tailscale-client-key";
        runtimeInputs = [
          config.packages.secret-view
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
        ];
        text = ''
          known=(${lib.concatStringsSep " " (map lib.escapeShellArg clientNames)})

          name="''${1:-}"

          # No argument: ask, rather than print a usage line and quit. This
          # is a command you run once per device and rarely enough to have
          # forgotten the names.
          if [ -z "$name" ]; then
            # Opening it, not `[ -r /dev/tty ]`: the device node tests
            # readable even with no controlling terminal attached, so that
            # check passes and the `read` then dies with "Device not
            # configured". Actually opening it is the only honest test.
            if ! { : </dev/tty; } 2>/dev/null; then
              echo "usage: nix run .#tailscale-client-key -- <${lib.concatStringsSep "|" clientNames}>" >&2
              exit 2
            fi
            echo "Which device is this key for?" >&2
            ${lib.concatStringsSep "\n" (
              lib.mapAttrsToList (n: desc: ''echo "  ${n} -- ${desc}" >&2'') clients
            )}
            printf 'device: ' >&2
            IFS= read -r name </dev/tty
          fi

          matched=0
          for k in "''${known[@]}"; do
            [ "$k" = "$name" ] && matched=1
          done
          if [ "$matched" -ne 1 ]; then
            echo "unknown device '$name' -- known: ''${known[*]}" >&2
            # The list is derived from the registry now, so that is where a new
            # device goes; there is no list in this file to edit any more.
            echo "(add a \`device\` entry to aspects/hosts/_registry.nix)" >&2
            exit 2
          fi

          # Decrypt to a private temp file rather than a variable: the
          # shared minting code sources a KEY=VALUE file, and a
          # process-substitution FD would not survive `set -a; . <file>`
          # identically across shells.
          tmp="$(mktemp -d)"
          trap 'rm -rf "$tmp"' EXIT

          src="secrets/tailscale_auth.age"
          if [ -e "$src" ] && (umask 077; secret-view "$src" > "$tmp/env" 2>/dev/null); then
            :
          else
            # Fall back to asking. The stored client needs the Yubikey to
            # decrypt, so this is what makes the command usable from a
            # checkout that does not have it -- and what lets you mint a key
            # with a *different* OAuth client without editing the secret.
            if ! { : </dev/tty; } 2>/dev/null; then
              echo "Could not decrypt $src, and there is no terminal to ask on." >&2
              echo "Run it from your own shell, where gpg can prompt for the Yubikey:" >&2
              echo "  nix run .#tailscale-client-key -- mobile" >&2
              exit 1
            fi
            echo "Could not decrypt $src -- falling back to asking." >&2
            echo "From https://login.tailscale.com/admin/settings/oauth (needs auth_keys write over ${nodeTag}):" >&2
            printf 'CLIENT_ID: ' >&2
            IFS= read -r oauth_id </dev/tty
            printf 'CLIENT_SECRET: ' >&2
            # -s: the secret is not echoed into a shared terminal's scrollback.
            IFS= read -rs oauth_secret </dev/tty
            printf '\n' >&2
            (
              umask 077
              printf 'CLIENT_ID=%s\nCLIENT_SECRET=%s\n' "$oauth_id" "$oauth_secret" > "$tmp/env"
            )
            unset oauth_id oauth_secret
          fi

          ts_env_file="$tmp/env"
          ts_description="$name"
          ${mintKeyScript}

          echo "" >&2
          echo "Single-use ${nodeTag} key for '$name' -- paste into the Tailscale app's" >&2
          echo "\"Custom auth key\" field. It is spent once the device enrols." >&2
          echo "" >&2
          printf '%s\n' "$authkey"
        '';
      };

      apps.tailscale-client-key = {
        type = "app";
        program = lib.getExe config.packages.tailscale-client-key;
      };
    };
}
