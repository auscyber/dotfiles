# Cloudflare tunnels, minted the way ./tailscale.nix mints auth keys: one
# long-lived account credential that only the admin host ever reads, and a
# connector token derived from it. Nothing writes a tunnel token by hand, and
# the account token never reaches a target.
#
# One tunnel per host, in front of that host's one nginx -- so the ingress is
# every vhost the box serves, and adding a service needs no second list.
#
# ONE TOKEN, and it is the whole secret: secrets/cloudflare_api.age holds the
# token and nothing else -- no `KEY=`, no trailing newline required. Everything
# that wants it in some other shape derives that shape with a template, which
# is why `acme_env` below exists rather than a second credential for the same
# account. (The file used to live next to ../hosts/secondpc/web.nix as
# `acme_cloudflare.age`, holding lego's environment file; it moved here when
# the tunnels needed the same token.)
#
# It is an ACCOUNT-scoped token, which is what makes the sub-tokens below
# possible -- a user token cannot mint account-owned ones, and R2's S3
# credentials are account-owned API tokens. It needs the union of:
#
#   Account / Cloudflare Tunnel  / Edit    creating the tunnels
#   Account / API Tokens         / Edit    minting the scoped sub-tokens
#   Account / Workers R2 Storage / Edit    creating the bucket
#   Account / Account Settings   / Read    finding which account this is
#   Zone    / DNS                / Edit    the tunnels' CNAMEs
#   Zone    / Zone               / Read    finding the zone for a hostname
#
# over the zones published into. A token missing one of these fails with the
# API's own message rather than something inscrutable -- `cf` prints the
# response body and names the scopes. Widen it at
# https://dash.cloudflare.com/profile/api-tokens.
#
# NOTHING BUT THIS TOKEN IS WRITTEN BY HAND. Everything else is minted from it
# and scoped down to one job: lego gets a DNS-only token for one zone, each
# host gets a connector token for its own tunnel, and celler gets S3
# credentials for one bucket. This one never reaches a host.
#
# BOOTSTRAP:
#
#   1. nix run .#secret-edit -- secrets/cloudflare_api.age
#      The bare token, on its own, with no `KEY=` prefix.
#   2. nix run .#gen-secrets     # tunnels, bucket, every sub-token
#   3. nix run .#rekey
#   4. nix run .#sync-tunnels    # a CNAME per vhost
#
#   then commit secrets/generated/<host>/r2.json, which step 2 writes, and
#   rebuild: that is what moves celler's storage onto the bucket.
#
# WHY STEP 4 IS NOT PART OF STEP 2. The route list is `attrNames
# config.services.nginx.virtualHosts`, and a generated secret is part of
# `config.age.secrets` -- so computing it inside the generator puts
# `age.secrets -> nginx.virtualHosts -> age.secrets` in the fixpoint (plenty of
# those vhosts read a secret path), which is the infinite recursion
# ../../lib/age-scoped.nix warns about. `sync-tunnels` reads the SAME evaluated
# option from outside the fixpoint, where that read is just a read. The list is
# still derived, not maintained: the only thing split off is when it is pushed.
{
  den,
  lib,
  ...
}:
let
  api = "https://api.cloudflare.com/client/v4";

  # Every API call in this file goes through `cf`, which writes the response's
  # `result` to STDOUT -- which for a generator script is the secret itself, so
  # every caller captures or discards it.
  #
  # NOT `curl -f`: these run under `errexit`, so a `-f` curl in a command
  # substitution dies at the assignment and takes every diagnostic with it.
  # Capture the status and say what the API objected to. `success` is checked as
  # well as the status, because Cloudflare answers some failures with 200.
  cfHelpers = ''
    cf() {
      cf_method="$1"
      cf_path="$2"
      cf_body="''${3:-}"
      cf_out="$(mktemp)"
      if [ -n "$cf_body" ]; then
        set -- -d "$cf_body"
      else
        set --
      fi
      cf_code="$(curl -sS -o "$cf_out" -w '%{http_code}' \
        -X "$cf_method" \
        -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" \
        -H "Content-Type: application/json" \
        "$@" \
        "${api}$cf_path" || echo 000)"

      if [ "$cf_code" != "200" ] || [ "$(jq -r '.success' <"$cf_out")" != "true" ]; then
        echo "cloudflare: $cf_method $cf_path failed (HTTP $cf_code)" >&2
        echo "  response: $(head -c 500 "$cf_out")" >&2
        echo "  check the token has Tunnel:Edit, DNS:Edit and Zone:Read" >&2
        rm -f "$cf_out"
        exit 1
      fi

      jq -c '.result' <"$cf_out"
      rm -f "$cf_out"
    }

    # Every zone the token can see, fetched once. `cf_zone_for` then resolves
    # locally, so a host with twenty vhosts does not make twenty identical
    # calls.
    cf_zones_file=""
    cf_load_zones() {
      if [ -z "$cf_zones_file" ]; then
        cf_zones_file="$(mktemp)"
        cf GET '/zones?per_page=1000' > "$cf_zones_file"
      fi
    }

    # The account these credentials belong to. An account token is scoped to
    # exactly one, so the first entry is it. Sets cf_account_id.
    #
    # Separate from `cf_zone_for` because most of what is done here is
    # account-level -- tunnels, buckets, tokens -- and has no hostname to
    # resolve a zone from. Asking `cf_zone_for` for the zone of a tunnel named
    # `celler2` is how this went wrong: no zone is a suffix of it.
    cf_account() {
      if [ -z "''${cf_account_id:-}" ]; then
        cf_account_id="$(cf GET '/accounts?per_page=50' | jq -r '.[0].id // empty')"
        if [ -z "$cf_account_id" ]; then
          echo "cloudflare: the token can see no account" >&2
          echo "  an account-scoped token needs Account Settings:Read to list it" >&2
          exit 1
        fi
      fi
    }

    # The longest zone whose name is a suffix of the hostname -- `ivymect.in`
    # for cache.ivymect.in, `pierlot.com.au` for logs.pierlot.com.au. Resolved
    # against the account rather than by splitting on dots, which gets the
    # second one wrong, and PER HOSTNAME rather than once per host: one box
    # serves names in both of those zones, and a record written into the wrong
    # zone is either a 403 or a record on the wrong domain.
    # Sets cf_zone_id and cf_account_id.
    cf_zone_for() {
      cf_load_zones
      cf_zone="$(jq -c --arg h "$1" '
        map(select(.name as $n | $h | endswith($n)))
        | sort_by(.name | length) | last // empty' "$cf_zones_file")"
      if [ -z "$cf_zone" ]; then
        echo "cloudflare: no zone in this account covers $1" >&2
        exit 1
      fi
      cf_zone_id="$(printf '%s' "$cf_zone" | jq -r '.id')"
      cf_account_id="$(printf '%s' "$cf_zone" | jq -r '.account.id')"
    }

    # A permission group's id, by NAME. The ids are account-independent but
    # undocumented, and a stale one fails as an opaque 400 on token creation,
    # so they are looked up rather than pasted. Needs Account API Tokens:Read.
    cf_permission_group() {
      cf_pg="$(cf GET "/accounts/$cf_account_id/tokens/permission_groups" \
        | jq -r --arg n "$1" 'map(select(.name == $n)) | .[0].id // empty')"
      if [ -z "$cf_pg" ]; then
        echo "cloudflare: no permission group named '$1'" >&2
        echo "  an account token needs Account API Tokens:Read to list the groups" >&2
        exit 1
      fi
    }

    # An account API token, scoped by the caller. $1 name, $2 policies JSON.
    # Its `value` comes back ONCE, here, and never again -- which is why every
    # caller's output IS that value, or something derived from it.
    cf_mint_token() {
      cf_stale="$(cf GET "/accounts/$cf_account_id/tokens" \
        | jq -r --arg n "$1" 'map(select(.name == $n)) | .[0].id // empty')"
      if [ -n "$cf_stale" ]; then
        # A token of this name already exists, and its value is unrecoverable,
        # so it is revoked and replaced rather than reused -- the opposite of
        # the tunnel, where the id is the useful part and the secret is
        # re-fetchable.
        cf DELETE "/accounts/$cf_account_id/tokens/$cf_stale" >/dev/null
        echo "cloudflare: revoked previous '$1' token" >&2
      fi
      cf POST "/accounts/$cf_account_id/tokens" \
        "$(jq -n --arg name "$1" --argjson policies "$2" \
          '{name: $name, policies: $policies}')"
    }

    # Reuse before create, so regenerating a token does not strand a tunnel.
    # `is_deleted=false` matters: a deleted tunnel keeps its name and would
    # otherwise be adopted and then refuse to run. Sets cf_tunnel_id.
    cf_tunnel_for() {
      cf_tunnel_id="$(cf GET \
        "/accounts/$cf_account_id/cfd_tunnel?is_deleted=false&name=$1" \
        | jq -r '.[0].id // empty')"
      if [ -z "$cf_tunnel_id" ]; then
        # `config_src: "local"` -- the ingress is a NixOS option rendered to
        # /etc/cloudflared/config.yml, so the account must not hold a copy of
        # it that would win over the file. The connector still authenticates
        # with the token, which already encodes the account and the tunnel id;
        # that is what keeps the id out of the Nix config, where it could only
        # arrive by being written back into the repo.
        cf_tunnel_id="$(cf POST "/accounts/$cf_account_id/cfd_tunnel" \
          "$(jq -n --arg name "$1" '{name: $name, config_src: "local"}')" \
          | jq -r '.id')"
        echo "cloudflare: created tunnel $1 ($cf_tunnel_id)" >&2
      fi
    }
  '';

  # The secret is the token itself, so this is a read and not a `.` of an
  # environment file -- which also means no SC1090 to disable and no guessing
  # which of lego's four names it was written under.
  #
  # `$cf_token_file` in, CLOUDFLARE_API_TOKEN exported out, scrubbed by the
  # caller once it is done so the credential is not in the environment of
  # anything this later execs.
  loadToken = ''
    if [ ! -r "$cf_token_file" ]; then
      echo "cloudflare: $cf_token_file is not readable" >&2
      exit 1
    fi
    CLOUDFLARE_API_TOKEN="$(tr -d '[:space:]' < "$cf_token_file")"
    export CLOUDFLARE_API_TOKEN
    if [ -z "$CLOUDFLARE_API_TOKEN" ]; then
      echo "cloudflare: $cf_token_file is empty -- it must hold the API token" >&2
      exit 1
    fi
    case "$CLOUDFLARE_API_TOKEN" in
      *=*)
        # Caught rather than sent: the API would answer a 400 that reads like a
        # permissions problem. This file used to be lego's environment file.
        echo "cloudflare: $cf_token_file looks like a KEY=VALUE line, not a bare token" >&2
        echo "  drop everything up to and including the '=' -- aspects/network/cloudflare.nix" >&2
        exit 1
        ;;
    esac
  '';

  apiFile = ../../secrets/cloudflare_api.age;

  # A connector token for one tunnel, generated once and kept
  # master-encrypted at secrets/generated/<host>/<key>.age.
  #
  # `dir` is the host's `age.rekey.generatedSecretsDir` -- the one thing a
  # `secrets` body can see that says which host it is for. Deliberately NOT
  # given the route list; see the note at the top of this file.
  # A PER-ENTRY module function, which is what lets the dependency be found
  # rather than passed: `age.secrets.<n>` is a submodule, so lib/age-scoped.nix
  # binds `secrets` to the enclosing scope keyed by short name
  # (`entryType`'s `_module.args`). So the generator picks the scope's own
  # `api` entry up itself, and that entry is a real `age.secrets` value --
  # which it has to be, because apps/generate.nix reads `dep.id`,
  # `dep.generator` and `dep.rekeyFile` off a dependency and a literal
  # `{ rekeyFile = ...; }` fails with `attribute 'generator' missing`.
  #
  # The contract is just the name: whatever scope declares this must also
  # declare the account token as `api`, the way `den.aspects.cloudflare` does.
  # ONE script for every tunnel token, registered as `age.generators` and
  # named by `generator.script = "cloudflared-token"`. Its per-secret
  # parameters come off `secret.settings`, which is upstream's channel for
  # exactly this -- so nothing here closes over a call-site argument and the
  # script is genuinely shared rather than re-instantiated per host.
  #
  #   secrets.token = {
  #     settings.tunnel = "secondpc";
  #     generator = {
  #       script = "cloudflared-token";
  #       dependencies.api = secrets.api;
  #       derivedFrom = ...;
  #     };
  #   };
  tunnelTokenScript =
    {
      secret,
      pkgs,
      decrypt,
      deps,
      ...
    }:
    ''
      export PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
        ]
      }:$PATH

      cf_token_file="$(mktemp)"
      trap 'rm -f "$cf_token_file"' EXIT
      ${decrypt} ${lib.escapeShellArg deps.api.file} > "$cf_token_file"
      ${loadToken}
      ${cfHelpers}

      # A tunnel is account-level and its name is not a hostname, so there is
      # no zone to look up here -- the CNAMEs that point at it are
      # `sync-tunnels`' job, and that resolves a zone per hostname.
      tunnel_name=${lib.escapeShellArg secret.settings.tunnel}
      cf_account
      cf_tunnel_for "$tunnel_name"

      token="$(cf GET "/accounts/$cf_account_id/cfd_tunnel/$cf_tunnel_id/token" | jq -r '.')"
      unset CLOUDFLARE_API_TOKEN

      if [ -z "$token" ]; then
        echo "cloudflare: tunnel $cf_tunnel_id has no connector token" >&2
        exit 1
      fi

      printf '%s\n' "$token"
    '';

  # The zone ACME's DNS-01 challenges are solved in. One literal, because it
  # is the only thing here that is a choice rather than something the API can
  # be asked.
  acmeZone = "ivymect.in";

  # lego's environment file, holding a DNS-ONLY sub-token rather than the
  # account token it was minted with. The account token can edit every tunnel,
  # bucket and record in the account; what goes on a host that only renews
  # certificates is a token that can do nothing else.
  #
  # `Zone Read` as well as `DNS Write` because lego finds the zone for a name
  # rather than being told it -- the two variables are the same token, which is
  # what `CLOUDFLARE_ZONE_API_TOKEN` is for.
  dnsTokenScript =
    {
      secret,
      pkgs,
      decrypt,
      deps,
      ...
    }:
    ''
      export PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
        ]
      }:$PATH

      cf_token_file="$(mktemp)"
      trap 'rm -f "$cf_token_file"' EXIT
      ${decrypt} ${lib.escapeShellArg deps.api.file} > "$cf_token_file"
      ${loadToken}
      ${cfHelpers}

      cf_zone_for ${lib.escapeShellArg secret.settings.zone}

      cf_permission_group "DNS Write"
      dns_pg="$cf_pg"
      cf_permission_group "Zone Read"
      zone_pg="$cf_pg"

      policies="$(jq -n \
        --arg dns "$dns_pg" \
        --arg zone "$zone_pg" \
        --arg res "com.cloudflare.api.account.zone.$cf_zone_id" \
        '[{
           effect: "allow",
           permission_groups: [{ id: $dns }, { id: $zone }],
           resources: { ($res): "*" }
         }]')"

      value="$(cf_mint_token "acme-dns-${"\${cf_zone_id}"}" "$policies" | jq -r '.value')"
      unset CLOUDFLARE_API_TOKEN

      if [ -z "$value" ]; then
        echo "cloudflare: token endpoint returned no value" >&2
        exit 1
      fi

      printf 'CLOUDFLARE_DNS_API_TOKEN=%s\nCLOUDFLARE_ZONE_API_TOKEN=%s\n' "$value" "$value"
    '';

  # ---------------------------------------------------------------- R2
  #
  # S3 credentials for a bucket, minted from the same account token. R2's S3
  # credentials ARE an account API token, derived like this:
  #
  #   Access Key ID      the token's `id`
  #   Secret Access Key  the SHA-256 of the token's `value`
  #
  # The token's `value` is returned once, at creation, and never again -- so
  # the raw token is kept as its own `intermediary` secret and the two usable
  # halves are generated FROM it. That is also why they are three secrets and
  # not one env fragment: each half stays a bare value, so the env file is
  # shaped by `templates.env` in ../base/celler/server.nix like everything
  # else, and a rotation is one file to delete.
  #
  # Needs `Account API Tokens: Edit` on top of the tunnel scopes (this is an
  # ACCOUNT-scoped token, so the user-level equivalent does not apply), plus
  # `Workers R2 Storage: Edit` to make the bucket.
  r2TokenScript =
    {
      secret,
      pkgs,
      file,
      decrypt,
      deps,
      ...
    }:
    ''
      export PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
        ]
      }:$PATH

      cf_token_file="$(mktemp)"
      trap 'rm -f "$cf_token_file"' EXIT
      ${decrypt} ${lib.escapeShellArg deps.api.file} > "$cf_token_file"
      ${loadToken}
      ${cfHelpers}

      bucket=${lib.escapeShellArg secret.settings.bucket}

      # R2 is account-level: no zone involved.
      cf_account

      # Reuse before create, as everywhere else here: a second run must not
      # fail on a bucket that is already there.
      #
      # Asked as a LIST, not `GET /r2/buckets/<name>`. `cf` exits the script on
      # any non-200, and a missing bucket is a 404 -- so probing for one with
      # `cf` killed the generator instead of falling through to create it.
      # `exit` inside a function called in an `if` condition ends the shell,
      # not the condition. A list is a 200 either way and the answer is in the
      # body. (R2 wraps it: `result.buckets`, not a bare array.)
      if cf GET "/accounts/$cf_account_id/r2/buckets?per_page=1000" \
        | jq -e --arg b "$bucket" '(.buckets // .) | any(.name == $b)' >/dev/null
      then
        echo "cloudflare: bucket $bucket exists" >&2
      else
        cf POST "/accounts/$cf_account_id/r2/buckets" \
          "$(jq -n --arg name "$bucket" '{name: $name}')" >/dev/null
        echo "cloudflare: created bucket $bucket" >&2
      fi

      # The permission group is looked up by NAME rather than hardcoded: the
      # ids are account-independent but undocumented, and a stale one fails as
      # an opaque 400 on token creation.
      cf_permission_group "Workers R2 Storage Bucket Item Write"

      # Scoped to this ONE bucket, not the account: the resource key is
      # Cloudflare's own spelling for "items in this bucket".
      policies="$(jq -n \
        --arg pg "$cf_pg" \
        --arg res "com.cloudflare.edge.r2.bucket.''${cf_account_id}_default_''${bucket}" \
        '[{
           effect: "allow",
           permission_groups: [{ id: $pg }],
           resources: { ($res): "*" }
         }]')"

      created="$(cf_mint_token "celler-$bucket" "$policies")"

      # The account id and the endpoint built from it, written PLAINTEXT next
      # to the secret for Nix to read back (`lib.importJSON`). Neither is
      # secret -- the account id is in every R2 URL -- but both are only known
      # once the API has been asked, and celler's `[storage].endpoint` is
      # needed at EVALUATION time. This is the same trick
      # ../../patches/agenix-rekey/derivedFrom.patch uses for its stamp.
      #
      # A fixed sibling name, not the secret's own: that one carries the
      # `derivedFrom` hash, which the reader would have to recompute.
      r2_meta="$(dirname ${lib.escapeShellArg file})/r2.json"
      jq -n \
        --arg account "$cf_account_id" \
        --arg bucket "$bucket" \
        '{
           accountId: $account,
           bucket: $bucket,
           endpoint: ("https://" + $account + ".r2.cloudflarestorage.com")
         }' > "$r2_meta"
      echo "cloudflare: wrote $r2_meta -- commit it, the storage config reads it" >&2

      unset CLOUDFLARE_API_TOKEN

      # `value` is returned ONCE. Both halves are derived from this file, so
      # losing it means rotating rather than recovering.
      printf '%s' "$created" | jq -c '{id, value}'
    '';

  # The two usable halves. Both read the token secret rather than the API --
  # `dependencies` orders them after it, so the file is there.
  r2KeyIdScript =
    {
      pkgs,
      decrypt,
      deps,
      ...
    }:
    ''
      export PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.jq
        ]
      }:$PATH
      ${decrypt} ${lib.escapeShellArg deps.token.file} | jq -r '.id'
    '';

  r2SecretKeyScript =
    {
      pkgs,
      decrypt,
      deps,
      ...
    }:
    ''
      export PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.jq
        ]
      }:$PATH
      # Exactly what R2 documents: the SHA-256 of the token value, hex, and
      # nothing else on the line.
      ${decrypt} ${lib.escapeShellArg deps.token.file} \
        | jq -r '.value' \
        | tr -d '\n' \
        | sha256sum \
        | cut -d' ' -f1
    '';

  # The three declarations a celler server adds to its `celler` scope. The two
  # halves are named for `templates.env` in ../base/celler/server.nix, which
  # picks them up because a template depends on every secret in its scope.
  r2Secrets = settings: { secrets, ... }: {
    # The account token, in THIS scope. An entry module function only ever
    # sees its own scope (`entryType`'s `_module.args` in
    # ../../lib/age-scoped.nix), so `den.aspects.cloudflare`'s `api` is not
    # reachable from the `celler` scope these land in. Declared again here
    # instead, which is the SAME secret because agenix-rekey keys on the file
    # -- and it makes this set self-contained, droppable into any scope.
    api = {
      rekeyFile = apiFile;
      intermediary = true;
    };

    # Never deployed: the host needs the two halves, not the token they came
    # from.
    r2_token = {
      inherit settings;
      intermediary = true;
      generator = {
        tags = [ "r2_token" ];
        script = "r2-token";
        dependencies.api = secrets.api;
        derivedFrom = settings;
      };
    };

    r2_access_key_id = {
      inherit settings;
      generator = {
        tags = [ "r2_token" ];
        script = "r2-access-key-id";
        dependencies.token = secrets.r2_token;
        derivedFrom = settings // {
          token = secrets.r2_token;
        };
      };
    };

    r2_secret_access_key = {
      inherit settings;
      generator = {
        tags = [ "r2_token" ];
        script = "r2-secret-access-key";
        dependencies.token = secrets.r2_token;
        derivedFrom = settings // {
          token = secrets.r2_token;
        };
      };
    };
  };

  # R2's S3 endpoint. Account-scoped, and `force_path_style` comes with it --
  # celler only sets that when `endpoint` is in the TOML, so this cannot be
  # left to `AWS_ENDPOINT_URL`.
  r2Endpoint = accountId: "https://${accountId}.r2.cloudflarestorage.com";

  # What a call site writes. `settings` is both the generator's parameters and
  # what the secret is a function of, so `derivedFrom` is just `settings` --
  # one place to add a parameter, and adding one moves the filename.
  tunnelSecret =
    settings:
    # Still a per-entry module function, for `secrets.api`: a generator
    # dependency has to be a real `age.secrets` entry, and this is where the
    # enclosing scope's entries are bound.
    { secrets, ... }: {
      inherit settings;
      generator = {
        tags = [ "cloudflared_token" ];
        script = "cloudflared-token";
        dependencies.api = secrets.api;
        derivedFrom = settings;
      };
    };
in
{
  # For a host that fronts a tunnel with something other than nginx -- see
  # ../hosts/celler2.nix, a container with cellerd and nothing else.
  den.lib.cloudflare = {
    inherit tunnelSecret r2Secrets r2Endpoint;
  };

  # The account token, and the shapes other things want it in. Included by
  # whatever needs one; the token is deployed because a template is rendered on
  # the host from its dependencies, which is no more exposure than the
  # environment file it replaces.
  # The shared generator, registered once for the whole fleet rather than on
  # `den.aspects.cloudflare`: ../hosts/celler2.nix mints a connector token
  # without including that aspect (it has no nginx to put behind a tunnel),
  # and a named generator has to be registered wherever a secret names it. It
  # costs nothing to carry -- an unapplied function that is never run unless
  # some secret's `generator.script` asks for it.
  den.default.age.generators.cloudflared-token = tunnelTokenScript;
  den.default.age.generators.cloudflare-dns-token = dnsTokenScript;
  den.default.age.generators.r2-token = r2TokenScript;
  den.default.age.generators.r2-access-key-id = r2KeyIdScript;
  den.default.age.generators.r2-secret-access-key = r2SecretKeyScript;

  den.aspects.cloudflare = {
    includes = [ den.aspects.agenix-rekey ];

    # The scope is named after the aspect, not a service -- there is no
    # `services.cloudflare` for inference to find.
    secretSettings.service = null;

    secrets =
      {
        secrets,
        age,
        ...
      }:
      {
        # The account token. `intermediary` because nothing on any host reads
        # it: `agenix generate` decrypts it on the admin host to mint the
        # tunnel tokens and to write lego's environment file, and only those
        # travel.
        api = {
          rekeyFile = apiFile;
          intermediary = true;
        };

        # lego's environment file: the token under the names it reads,
        # GENERATED from the token rather than templated from it.
        #
        # A template is rendered on the host, so its dependencies have to be
        # deployed -- which would put an account-wide DNS-and-tunnel credential
        # on every box that wants a certificate. A generator runs on the admin
        # host instead, so what ships is this file and the token stays put.
        #
        # `ZONE` as well as `DNS` because the same token carries Zone:Read,
        # which is what lets lego find the zone for a name rather than being
        # told -- `defaults.dnsProvider = "cloudflare"` in
        # ../services/nginx.nix is what consumes this.
        acme_env = {
          settings.zone = acmeZone;
          generator = {
            tags = [ "cloudflare_acme_env" ];
            script = "cloudflare-dns-token";
            dependencies.api = secrets.api;
            derivedFrom.zone = acmeZone;
          };
        };
      };
  };

  # One tunnel in front of this host's nginx. Public ingress without a port
  # forward or a static address, so it is also how a box behind CGNAT publishes.
  den.aspects.cloudflare-tunnel = {
    includes = [
      den.aspects.agenix-rekey
      den.aspects.cloudflare
    ];

    # Shares `cloudflare`'s scope, so `secrets.api` below is that aspect's
    # declared token entry and the generator can depend on it directly. Two
    # scopes would mean two declarations of one rekeyFile, which agenix-rekey
    # warns about precisely because it makes generator dependencies ambiguous.
    secretScope = "cloudflare";

    # The scope is not a service, and the unit that reads the token is
    # `cloudflared-tunnel` -- so inference has nothing to find and is pointed
    # at the unit by hand. Same escape hatch ../services/gateway.nix uses.
    #
    # Scope-wide, so it also lands on `api` and `acme_env`. Harmless: the
    # tunnel is the only unit in this scope, `api` is never deployed, and a
    # changed token is a reason to restart the connector anyway.
    secretSettings = {
      service = null;
      settings.restartUnits = [ "cloudflared-tunnel.service" ];
    };

    # Named after the host by where agenix puts its generated secrets, which is
    # the one thing a `secrets` body can see.
    secrets = { age, ... }: {
      token = tunnelSecret { tunnel = baseNameOf age.rekey.generatedSecretsDir; };
    };

    templates.env = { secrets, ... }: {
      dependencies.token = secrets.token;
      content = { placeholders, ... }: ''
        TUNNEL_TOKEN=${placeholders.token}
      '';
    };

    nixos =
      {
        config,
        pkgs,
        scoped,
        ...
      }:
      let
        cfg = config.services.cloudflared-tunnel;

        # The ingress the connector reads, in the documented shape: one rule
        # per hostname and `http_status:404` last, so a request matching
        # nothing gets a 404 rather than whatever the catch-all proxied.
        configFile = (pkgs.formats.yaml { }).generate "cloudflared.yml" {
          ingress = lib.mapAttrsToList (hostname: service: { inherit hostname service; }) cfg.ingress ++ [
            { service = "http_status:404"; }
          ];
        };
      in
      {
        options.services.cloudflared-tunnel = {
          frontDoorPort = lib.mkOption {
            type = lib.types.port;
            default = 8080;
            description = "Loopback port the connector forwards to.";
          };

          origin = lib.mkOption {
            type = lib.types.str;
            default = "http://localhost:${toString cfg.frontDoorPort}";
            defaultText = "http://localhost:‹frontDoorPort›";
            description = "What every ingress rule forwards to.";
          };

          exclude = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = ''
              Hostnames to keep off the tunnel, for anything that has to stay a
              direct A record.
            '';
          };

          ingress = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = lib.genAttrs (
              # `hasInfix "."` is the filter that matters: not every nginx
              # server block is a public name. searchix declares one called
              # `searchix-sources`, which as an ingress rule would have asked
              # Cloudflare for a CNAME by that name.
              builtins.filter (n: lib.hasInfix "." n) (
                lib.subtractLists cfg.exclude (builtins.attrNames config.services.nginx.virtualHosts)
              )
            ) (_: cfg.origin);
            defaultText = "every nginx vhost on this host that is a domain name, less `exclude`";
            description = ''
              Public hostname -> local service. Derived from nginx rather than
              from the `vhosts` den class, which would miss every vhost
              ../services/gateway.nix builds (it writes
              `services.nginx.virtualHosts` directly). Safe to read here
              because nothing in `age.secrets` reads this back -- see the note
              at the top of this file.
            '';
          };
        };

        config = lib.mkIf (cfg.ingress != { }) {
          # `--token`, not a credentials file: the token already carries the
          # account and the tunnel id, so the id never has to be recorded in
          # the repo to be named here. The ingress still comes from the file,
          # because the tunnel is `config_src: "local"`.
          systemd.services.cloudflared-tunnel = {
            wantedBy = [ "multi-user.target" ];
            after = [
              "network-online.target"
              "nginx.service"
            ];
            wants = [ "network-online.target" ];
            serviceConfig = {
              ExecStart = "${lib.getExe pkgs.cloudflared} tunnel --no-autoupdate --config ${configFile} run";
              # `scoped.cloudflare`, not `scoped.cloudflare-tunnel`: this
              # aspect sets `secretScope = "cloudflare"` so it can reach that
              # scope's `api` entry, which puts its own secrets and templates
              # there too.
              EnvironmentFile = scoped.cloudflare.templates.env.path;
              DynamicUser = true;
              Restart = "always";
              RestartSec = 5;
            };
          };

          # The connector's front door: plain HTTP on loopback, so nothing has
          # to terminate TLS twice at the edge and no vhost needs touching.
          #
          # It proxies to :443 rather than :80 because every vhost here is
          # `forceSSL` -- nixpkgs answers :80 with a 301 to https, which would
          # come straight back through the tunnel and loop. One loopback TLS
          # hop is the cost of leaving twenty-odd vhosts alone.
          services.nginx.appendHttpConfig = ''
            server {
              listen 127.0.0.1:${toString cfg.frontDoorPort};
              server_name _;

              # Only the local connector can reach this port, so it is the only
              # peer whose forwarded address is worth trusting -- which is why
              # Cloudflare's own ranges are not listed here. They would have to
              # be, and kept current, if this listened on anything routable.
              set_real_ip_from 127.0.0.1;
              real_ip_header CF-Connecting-IP;

              location / {
                proxy_pass https://127.0.0.1:443;
                proxy_ssl_server_name on;
                proxy_ssl_name $host;

                proxy_set_header Host              $host;
                proxy_set_header X-Real-IP         $remote_addr;
                proxy_set_header X-Forwarded-For   $proxy_add_x_forwarded_for;
                # The request arrived at the edge over https; without this every
                # app behind here builds http:// links and self-redirects.
                proxy_set_header X-Forwarded-Proto https;
                proxy_set_header Upgrade           $http_upgrade;
                proxy_set_header Connection        $connection_upgrade;
              }
            }
          '';
        };
      };
  };

  # `nix run .#sync-tunnels [host...]` -- point a proxied CNAME at each host's
  # tunnel, one per hostname in its `ingress`. With no arguments, every host
  # that has an ingress. Idempotent: each record is upserted.
  #
  # DNS only: the ingress itself ships in the closure
  # (/etc/cloudflared/config.yml), so a `nixos-rebuild` is what publishes a
  # route change and this is only needed when a hostname is ADDED or removed.
  #
  # `cloudflared tunnel route dns <tunnel> <hostname>` is the CLI equivalent
  # and is not used here: it authenticates with the cert.pem that
  # `cloudflared tunnel login` writes from an interactive browser flow, which
  # the account API token this already holds makes unnecessary.
  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    let
      syncTunnels = pkgs.writeShellApplication {
        name = "sync-tunnels";
        runtimeInputs = [
          config.packages.secret-view
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
          pkgs.nix
        ];
        text = ''
          if [ ! -e flake.nix ]; then
            echo "sync-tunnels: run from the repo root (flake.nix not found)" >&2
            exit 1
          fi

          src="secrets/cloudflare_api.age"
          cf_token_file="$(mktemp)"
          trap 'rm -f "$cf_token_file" ''${plan:-}' EXIT
          if ! (umask 077; secret-view "$src" > "$cf_token_file" 2>/dev/null); then
            echo "sync-tunnels: could not decrypt $src (is the Yubikey plugged in?)" >&2
            exit 1
          fi
          ${loadToken}
          ${cfHelpers}

          # One evaluation for the whole fleet: `ingress` per host, with the
          # hosts that have none dropped. Reading it out here rather than in the
          # generator is the whole point -- see aspects/network/cloudflare.nix.
          plan="$(mktemp)"
          nix eval --json --impure --expr '
            let f = builtins.getFlake (toString ./.);
            in builtins.mapAttrs
              (_: c: c.config.services.cloudflared-tunnel.ingress or {})
              f.nixosConfigurations
          ' > "$plan"

          hosts="$*"
          if [ -z "$hosts" ]; then
            hosts="$(jq -r 'to_entries | map(select(.value != {})) | .[].key' "$plan")"
          fi

          if [ -z "$hosts" ]; then
            echo "sync-tunnels: no host has a cloudflared ingress" >&2
            exit 1
          fi

          for host in $hosts; do
            ingress="$(jq -c --arg h "$host" '.[$h] // {}' "$plan")"
            if [ "$ingress" = "{}" ]; then
              echo "sync-tunnels: $host has no ingress, skipping" >&2
              continue
            fi

            # Any one hostname finds the zone and the account; they all sit in
            # the same zone in practice, and a stray one is caught per-record
            # below.
            # The tunnel lives in an account, so any one of its hostnames finds
            # it; the records below each resolve their own zone.
            cf_zone_for "$(printf '%s' "$ingress" | jq -r 'keys[0]')"
            cf_tunnel_for "$host"
            tunnel_id="$cf_tunnel_id"

            for name in $(printf '%s' "$ingress" | jq -r 'keys[]'); do
              cf_zone_for "$name"
              dns="$(jq -n \
                --arg name "$name" \
                --arg content "$tunnel_id.cfargotunnel.com" \
                '{type: "CNAME", proxied: true, name: $name, content: $content}')"
              record_id="$(cf GET \
                "/zones/$cf_zone_id/dns_records?type=CNAME&name=$name" \
                | jq -r '.[0].id // empty')"
              if [ -n "$record_id" ]; then
                cf PATCH "/zones/$cf_zone_id/dns_records/$record_id" "$dns" >/dev/null
              else
                cf POST "/zones/$cf_zone_id/dns_records" "$dns" >/dev/null
              fi
              echo "  $name -> $host" >&2
            done

            echo "sync-tunnels: $host done ($(printf '%s' "$ingress" | jq -r 'keys | length') hostnames)" >&2
          done

          unset CLOUDFLARE_API_TOKEN
        '';
      };
    in
    {
      packages.sync-tunnels = syncTunnels;
      devshells.default.packages = [ syncTunnels ];
      apps.sync-tunnels = {
        type = "app";
        program = lib.getExe syncTunnels;
      };
    };
}
