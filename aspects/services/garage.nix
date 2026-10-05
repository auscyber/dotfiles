{ den, lib, ... }:
# Garage (self-hosted S3), one node, bound to loopback.
#
# Garage's model is "a config file for the NODE, an RPC API for the CLUSTER",
# and the split is not negotiable:
#
#   DECLARATIVE   everything in `services.garage.settings` -- the replication
#                 factor, the three listen addresses, the directories, the
#                 database engine. Rendered to /etc/garage.toml, and the unit
#                 restarts when it changes.
#   RECONCILED    the cluster LAYOUT (which node holds which share of the
#                 keyspace), buckets, access keys and bucket permissions. These
#                 live in garage's own metadata tables, reachable only over RPC
#                 against a RUNNING node -- there is no config-file form and no
#                 nixpkgs option. `garage-provision` below is the oneshot that
#                 reconciles them, in the shape ./nextcloud.nix's
#                 `nextcloud-shares` already uses: check before create, let an
#                 upsert be an upsert, and never guess at a value it can read.
#
# A fresh garage node with no layout is not a degraded cluster, it is a
# NON-FUNCTIONING one: every S3 request fails until some node has been assigned
# a role and the layout version applied. So the layout half of that oneshot is
# load bearing on first boot, not a nicety.
#
# --- why loopback, and why nothing is published ---
#
# All three listeners are on 127.0.0.1, the same reasoning as ./grafana.nix's
# `http_addr` and ./audiobookshelf.nix's `host`. For garage two of them are
# stronger than the house rule:
#
#   * the RPC port is FULL CLUSTER CONTROL to anybody holding `rpc_secret` --
#     it is the transport the `garage` CLI itself uses -- so it has no business
#     on the LAN, let alone through the tunnel;
#   * the admin API is the same class of thing for everything behind a token.
#
# There is deliberately no `gated.garage` block. Nothing off-box consumes this,
# and publishing the S3 API would additionally want `root_domain` plus a
# wildcard record so clients could use virtual-hosted-style addressing -- which
# ../network/cloudflare.nix does not provide and path-style addressing does not
# need. An entry here would also hand ./gateway.nix a service whose `upstream`
# has no default; see the `access` note in ../households/ivymectin.nix.
#
# --- SPLITPRO DOES NOT USE THIS, and cannot ---
#
# This aspect was written to back ./splitpro.nix's receipt uploads with S3. It
# cannot: SplitPro v2 REMOVED object-storage support. The pinned image is
# v2.1.5 (../../packages/splitpro.nix, digest-pinned by nvfetcher) and in that
# tree
#
#   * `src/env.ts` is a `@t3-oss/env-nextjs` schema listing every variable the
#     app reads, and there is no S3/R2/bucket/endpoint/credential entry in it;
#   * `src/pages/api/upload.ts` writes to `path.join(process.cwd(), 'uploads')`
#     with `node:fs/promises`, and `src/pages/api/files/[...path].ts` serves it
#     back -- which is what the `/var/lib/splitpro/uploads:/app/uploads` bind
#     mount in ./splitpro.nix already is;
#   * `docs/MIGRATING_FROM_V1.md` says it outright: "Receipts are now stored
#     locally instead of S3/R2/Minio."
#
# So ./splitpro.nix needs NO change, and adding `S3_*`/`R2_*` to its env
# template would be inert -- t3-env ignores variables its schema does not name.
# The `splitpro` bucket and key below are provisioned anyway: they cost one
# generated secret, they are what any S3 client (rclone, s3cmd, a later
# upstream that brings the feature back) has to be handed, and they are what
# exercises the bucket/key half of the oneshot at all.
#
# --- how a consumer gets credentials, and the trap in the obvious way ---
#
# Garage MINTS an access key at runtime (`garage key create`), so the credential
# does not exist at eval time and no template can interpolate it. `garage key
# import` inverts that: the key id and the secret are chosen by us and installed
# into garage, so both sides know them before the node has ever run. v2.4.1's
# `Key::import` (src/model/key_table.rs) validates only "key id >= 8 chars of
# [A-Za-z0-9._-]" and "secret >= 16 graphic ASCII", so a readable `GK<name>` id
# and a `random-hex-32` secret both pass -- the `GK` + 24 hex shape is what
# `Key::new` happens to generate, not a requirement.
#
# WHICH SCOPE THE SECRET LIVES IN IS THE WHOLE DESIGN. A template may not depend
# on another scope's secret: `age.templates` -> `age.scoped.<other>.access` ->
# `age.templates` is a cycle, and it only surfaces at `gen-secrets`, not at eval
# (./postgres.nix's `rolePasswords` note spells it out). So the material is NOT
# held here and handed to a consumer. It is held in the CONSUMER's own scope,
# where its env template can interpolate it freely, and garage's oneshot reads
# the file by PATH at activation -- a plain path read, which is exactly what
# ./postgres.nix's `passwordFile` already does across scopes and is safe because
# it never enters `age.templates`.
#
#   # in the consuming aspect:
#   secrets.s3-secret-key.generator.script = "random-hex-32";
#   templates.env = { secrets, ... }: {
#     dependencies = { inherit (secrets) s3-secret-key; };
#     content = { placeholders, ... }: ''
#       S3_ACCESS_KEY_ID=GKmyservice
#       S3_SECRET_ACCESS_KEY=${placeholders.s3-secret-key}
#       S3_ENDPOINT=http://127.0.0.1:3900
#     '';
#   };
#   nixos = { scoped, ... }: {
#     garage.buckets.myservice.secretKeyFile =
#       scoped.myservice.secrets.s3-secret-key.path;
#   };
#
# `garage.buckets` is a NixOS option, not a den class, on purpose: den class
# content merges ONE LEVEL DEEP, so a second aspect writing a `buckets` class
# would replace this file's whole subtree. NixOS merges by attribute.
let
  # THE THREE LISTEN ADDRESSES, ONCE. Each is both a line in garage.toml and a
  # port this aspect `gives`, and the second is what ../base/firewall.nix and
  # every resolver read -- so a literal in one place and a give in the other
  # would be the same fact twice, able to disagree. ../households/ivymectin.nix
  # used to carry a third copy.
  ports = {
    # The S3 API. Path-style addressing, no `root_domain`.
    s3 = 3900;
    # Cluster RPC -- the transport the `garage` CLI uses, so holding
    # `rpc_secret` and reaching this port is administrative access.
    rpc = 3901;
    # /health and /metrics; everything else there needs a token nothing here
    # provisions.
    admin = 3903;
  };

  portDescriptions = {
    s3 = "The S3 data API, path-style, on loopback: every bucket consumer on this box reads and writes through it.";
    rpc = "Cluster RPC between garage nodes, and what the `garage` CLI speaks -- `rpc_secret` plus this port is administrative access.";
    admin = "The admin API: /health and /metrics unauthenticated, everything else behind a token nothing here provisions.";
  };
in
{
  den.aspects.garage = {
    includes = [ den.aspects.agenix-rekey ];

    # ── WHAT IT LISTENS ON, as capabilities it gives ──
    #
    # NONE CARRIES `expose`, which is the statement rather than an omission:
    # ../base/firewall.nix opens exactly the ports a service declares exposed,
    # so declaring these keeps them visible to resolution while leaving the
    # firewall shut. The RPC one in particular is full cluster control to
    # anything holding `rpc_secret` (header).
    #
    # `reach` is left at its default `host`, which is the fact the household
    # file used to restate: this is stateful storage whose contents are the
    # whole point, so another machine borrowing this one's is exactly what
    # `reach` exists to prevent -- and all three listeners are on 127.0.0.1, so
    # anything wider would be a promise the binding cannot keep.
    gives =
      { self, ... }:
      {
        # A SERVICE THE HOUSE OFFERS. No `access`: garage's permission model
        # is per (bucket, access key) in its own metadata tables, so there is
        # nothing a kanidm group could mean.
        service = _: { };

        tcp = _: {
          ports = lib.mapAttrs (n: port: {
            inherit port;
            meta.description = portDescriptions.${n};
          }) ports;
          carrier = self.name;
        };
      };

    secrets = {
      # Garage refuses to start without this and every `garage` CLI invocation
      # presents it. 32 bytes as hex is what garage documents.
      rpc-secret.generator.script = "random-hex-32";

      # The `splitpro` bucket's key material -- see the header for why it lives
      # in THIS scope rather than splitpro's (nothing over there reads it) and
      # why it would live in the consumer's scope for a service that did.
      #
      # ROTATING THIS DOES NOT ROTATE THE KEY. The import below is guarded on
      # the key not existing, because `garage key import` on a live key id is an
      # error rather than an update. Rotation is `garage key delete GKsplitpro
      # --yes` and letting the oneshot re-import.
      splitpro-key.generator.script = "random-hex-32";
    };

    # Why an env file and not `rpc_secret_file` in the TOML, which garage also
    # supports: the nixpkgs module runs the daemon with `DynamicUser = true`, so
    # its uid is transient and no agenix `owner` can name it. systemd reads
    # `EnvironmentFile` as root before dropping privileges, so this reaches the
    # daemon where a 0400 root-owned file would not. It also sidesteps garage's
    # own refusal to read a group/world-readable secret FILE -- an environment
    # variable has no mode to check.
    templates.env = { secrets, ... }: {
      dependencies = { inherit (secrets) rpc-secret; };
      content =
        { placeholders, ... }:
        ''
          GARAGE_RPC_SECRET=${placeholders.rpc-secret}
        '';
    };

    # The `splitpro` key in the form an S3 client wants it. No unit reads this
    # today (see the header); it exists so the credential is one `cat` away for
    # rclone/s3cmd, and so a future consumer in this scope has something to
    # mount rather than a note telling it to go and look.
    templates.s3-env = { secrets, ... }: {
      dependencies = { inherit (secrets) splitpro-key; };
      content =
        { placeholders, ... }:
        ''
          AWS_ACCESS_KEY_ID=GKsplitpro
          AWS_SECRET_ACCESS_KEY=${placeholders.splitpro-key}
          AWS_ENDPOINT_URL_S3=http://127.0.0.1:${toString ports.s3}
          AWS_REGION=garage
        '';
    };

    nixos =
      {
        config,
        pkgs,
        lib,
        scoped,
        ...
      }:
      let
        garage = lib.getExe config.services.garage.package;

        # One node, so one failure domain, and naming it after the machine is
        # the only thing that stays true if a second node is ever added.
        zone = config.networking.hostName;

        # ADVISORY. Garage weights the keyspace by capacity and does not enforce
        # it as a quota; with one node the weighting is a no-op. It still has to
        # be given -- `layout assign` wants `-c` or `-g` -- so this is a number
        # that says "a reasonable slice of the pool", not a limit.
        capacity = "100G";

        buckets = lib.attrValues config.garage.buckets;
      in
      {
        imports = [
          {
            options.garage.buckets = lib.mkOption {
              type = lib.types.attrsOf (
                lib.types.submodule (
                  { name, ... }:
                  {
                    options = {
                      bucket = lib.mkOption {
                        type = lib.types.str;
                        default = name;
                        description = "S3 bucket name. Defaults to the attribute name.";
                      };

                      keyId = lib.mkOption {
                        type = lib.types.str;
                        default = "GK${name}";
                        description = ''
                          Access key id to install for this bucket. Not a secret
                          -- it is the username half -- so it is a plain string
                          here and the consumer spells the same one.
                        '';
                      };

                      secretKeyFile = lib.mkOption {
                        type = lib.types.path;
                        description = ''
                          File holding the secret half, read at activation by
                          `garage-provision`. It belongs to the CONSUMER's agenix
                          scope, not garage's -- see this file's header.
                        '';
                      };

                      permissions = lib.mkOption {
                        type = lib.types.listOf (
                          lib.types.enum [
                            "read"
                            "write"
                            "owner"
                          ]
                        );
                        default = [
                          "read"
                          "write"
                        ];
                        description = ''
                          What the key may do in the bucket. `owner` additionally
                          allows deleting the bucket and changing its website
                          config, which an uploader does not need.
                        '';
                      };
                    };
                  }
                )
              );
              default = { };
              description = ''
                Buckets and their access keys, reconciled against the running
                node by `garage-provision`. Not declarative in garage itself;
                this option is the declaration, the oneshot is the enforcement.
              '';
            };
          }
        ];

        services.garage = {
          enable = true;

          # `package` has NO default in the nixpkgs module -- it says "needs to
          # be set explicitly" -- and the choice is STATEFUL: garage will not
          # move more than one major at a time, so a bump is a stop plus
          # `garage offline-repair`, not an activation. `garage_2` (2.4.1) and
          # not `pkgs.garage` (= `garage_1`, 1.3.1) because this store is empty:
          # starting on 1.x would buy a migration for nothing. 2.x is also what
          # makes `garage json-api` available, which the oneshot relies on.
          package = pkgs.garage_2;

          environmentFile = scoped.garage.templates.env.path;

          settings = {
            # Garage refuses to start unless exactly one of these is set.
            # `replication_factor` is the 1.0+ spelling; `replication_mode =
            # "none"` is the pre-0.10 name, still accepted as a deprecated alias
            # (both fields are present in src/util/config.rs at v2.4.1), so use
            # the current one. 1 means one copy of every block: no redundancy,
            # which is the only honest value for a single node.
            replication_factor = 1;

            # The module's own defaults, restated because the `/var/lib/garage`
            # PREFIX is load bearing: the unit sets `StateDirectory = "garage"`
            # only when every path has it, and that is what creates and chowns
            # the directories for the DynamicUser.
            metadata_dir = "/var/lib/garage/meta";
            data_dir = "/var/lib/garage/data";

            # `db_engine` deliberately unset. The default is lmdb and changing
            # it later is `garage convert-db` on a stopped node, not a redeploy.

            # Single node with no `bootstrap_peers`, so the address other nodes
            # would dial is the loopback one it binds.
            rpc_bind_addr = "127.0.0.1:${toString ports.rpc}";
            rpc_public_addr = "127.0.0.1:${toString ports.rpc}";

            s3_api = {
              api_bind_addr = "127.0.0.1:${toString ports.s3}";
              # Signed into every request's SigV4 credential scope, so a client
              # has to be told the same string. `garage` is garage's own
              # conventional value for a store that is not on AWS.
              s3_region = "garage";
              # No `root_domain`: virtual-hosted-style addressing
              # (<bucket>.s3.<domain>) needs a wildcard record and a certificate
              # for it. Path-style (127.0.0.1:3900/<bucket>) needs neither.
            };

            admin.api_bind_addr = "127.0.0.1:${toString ports.admin}";
            # No `admin_token`. Without one garage serves only the
            # unauthenticated admin endpoints -- /health and /metrics, since
            # `metrics_require_token` defaults false -- which is all anything on
            # this box wants. Administration goes over RPC with the `garage`
            # CLI, which authenticates with `rpc_secret` and not with this.
          };
        };

        # The administrative CLI, because the one the nixpkgs module installs
        # cannot be executed. That one is a `pkgs.writeScriptBin "garage"` whose
        # text begins with a comment, and `writeScriptBin` does not prepend a
        # shebang -- so /run/current-system/sw/bin/garage is a mode-0755 file
        # with no `#!` line, which bash will fall back to interpreting but
        # `execve` (so sudo, and fish) rejects with ENOEXEC. Every "by hand" step
        # in this file's comments goes through this one instead:
        #
        #   sudo garage-manage status
        #   sudo garage-manage key info GKsplitpro --show-secret
        #
        # A different NAME on purpose: two packages both providing /bin/garage
        # would be a `environment.systemPackages` collision.
        environment.systemPackages = [
          (pkgs.writeShellScriptBin "garage-manage" ''
            export GARAGE_RPC_SECRET_FILE=${lib.escapeShellArg scoped.garage.secrets.rpc-secret.path}
            exec ${garage} "$@"
          '')
        ];

        # Everything garage cannot be told in a config file.
        #
        # BY HAND, the things this deliberately does not reconcile:
        #
        #   * the LAYOUT, once assigned. The guard is "no node has a role", so
        #     changing `zone` or `capacity` afterwards is not picked up --
        #     re-running `layout assign` on a live cluster triggers a data
        #     rebalance, which is not something an activation should start
        #     behind your back. Do it deliberately:
        #       sudo garage layout assign -z <z> -c <c> <node-id>
        #       sudo garage layout apply --version <n+1>
        #   * DELETING a `garage.buckets` entry leaves the bucket, its objects
        #     and its key. Same reasoning as ./nextcloud.nix's shares: a config
        #     change must not destroy data. `garage bucket delete <n> --yes`.
        #   * a key's SECRET, once imported -- see `secrets.splitpro-key`.
        #
        # Note the `garage` binary in `environment.systemPackages` is a wrapper
        # the nixpkgs module generates, and it is NOT used here: it sources
        # `environmentFile`, which is fine for a human with sudo but means the
        # oneshot would depend on that wrapper's shape. `GARAGE_RPC_SECRET_FILE`
        # against the secret directly is the same thing with one fewer layer,
        # and this unit runs as root so it can read a 0400 file.
        systemd.services.garage-provision = {
          description = "Reconcile Garage's cluster layout, buckets and access keys";
          after = [ "garage.service" ];
          requires = [ "garage.service" ];
          wantedBy = [ "multi-user.target" ];
          path = [ pkgs.jq ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            export GARAGE_RPC_SECRET_FILE=${lib.escapeShellArg scoped.garage.secrets.rpc-secret.path}

            # `garage.service` is `Type = simple`, so `After=` only means the
            # process was forked -- the RPC listener is not up yet, and on a
            # cold lmdb open it is not up for a second or two. Poll rather than
            # sleep a guessed interval.
            ready=
            for _ in $(seq 60); do
              if ${garage} status >/dev/null 2>&1; then
                ready=1
                break
              fi
              sleep 1
            done
            if [ -z "$ready" ]; then
              echo "garage did not answer RPC within 60s; not provisioning" >&2
              exit 1
            fi

            # --- cluster layout ---
            #
            # `json-api` is the admin API over the same RPC the CLI already
            # uses, which is the only form of this that can be parsed: `layout
            # show` prints the version to apply as prose for a human to read.
            # `apply --version` must be exactly current + 1, by design, as a
            # guard against two administrators staging changes at once.
            layout="$(${garage} json-api GetClusterLayout)"
            if [ "$(jq '.roles | length' <<<"$layout")" -eq 0 ]; then
              node="$(${garage} json-api GetClusterStatus | jq -r 'first(.nodes[] | select(.isUp) | .id)')"
              ${garage} layout assign -z ${lib.escapeShellArg zone} -c ${lib.escapeShellArg capacity} "$node"
              ${garage} layout apply --version "$(( $(jq '.version' <<<"$layout") + 1 ))"
            fi

            # --- buckets, keys, permissions ---
            provision() {
              bucket="$1"
              keyid="$2"
              keyfile="$3"
              shift 3

              # `bucket create` FAILS on an existing bucket rather than being an
              # upsert, so look first.
              ${garage} bucket info "$bucket" >/dev/null 2>&1 \
                || ${garage} bucket create "$bucket"

              # NOT `garage key import <id> <secret>`: that puts the secret in a
              # command-line WORD, i.e. in `ps`, for the length of the call --
              # the same thing ./nextcloud.nix avoids with `--clientsecret-file`.
              # The admin API's ImportKey takes it in a JSON body and `json-api
              # <endpoint> -` reads that body from stdin, so it never becomes an
              # argv. jq gets the material through `--rawfile` for the same
              # reason, and strips the trailing newline agenix's generator does
              # not write but a hand-edited file would.
              if ! ${garage} key info "$keyid" >/dev/null 2>&1; then
                jq -n --arg id "$keyid" --arg name "$bucket" --rawfile secret "$keyfile" \
                  '{ accessKeyId: $id, secretAccessKey: ($secret | rtrimstr("\n")), name: $name }' \
                  | ${garage} json-api ImportKey -
              fi

              # Unguarded: `bucket allow` SETS the flags, so it is an upsert and
              # re-running it is how a changed `permissions` takes effect.
              ${garage} bucket allow --key "$keyid" "$@" "$bucket"
            }

            ${lib.concatMapStringsSep "\n" (b: ''
              provision ${
                lib.escapeShellArgs (
                  [
                    b.bucket
                    b.keyId
                    b.secretKeyFile
                  ]
                  ++ map (p: "--${p}") b.permissions
                )
              }
            '') buckets}
          '';
        };

        # The bucket ./splitpro.nix would have used. It does not -- the header
        # says why in detail -- so this is provisioned and unconsumed, which
        # costs one generated secret and keeps the credential ready.
        garage.buckets.splitpro.secretKeyFile = scoped.garage.secrets.splitpro-key.path;
      };
  };
}
