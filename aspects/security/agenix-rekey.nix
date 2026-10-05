{
  inputs,
  realInputs,
  lib,
  den,
  ...
}:
let
  # The enclave master as a TAG recipient: the same key, but age >= 1.3 and
  # rage encrypt to it natively, so a machine without `age-plugin-se` (a Linux
  # runner) can still write a secret it will be able to read.
  enclaveMaster = "age1tag1qtu7ycpgpl79gzdflwuywxc6qttaqldmyvv3w8yqhq5m7alk2fqd25safn7";

  # The CI master (.github/workflows/secrets.yml), present once
  # `nix run .#ci-master-init` has written its public half.
  ciMasterPub = ./ci-master.pub;
  ciMaster =
    if builtins.pathExists ciMasterPub then lib.trim (builtins.readFile ciMasterPub) else null;
  ciMasterIdentity = "/tmp/agenix-ci/master.txt";

  # ── THE SESSION MASTER (`nix run .#unlock`) ──
  # A software key whose private half is kept in the repo encrypted to the
  # enclave and the backup. Unlocking decrypts it once, with one Touch ID, for
  # the life of one shell; inside it the wrappers decrypt with it instead of
  # asking the enclave for every file.
  sessionMasterPub = ./session-master.pub;
  sessionMaster =
    if builtins.pathExists sessionMasterPub then
      lib.trim (builtins.readFile sessionMasterPub)
    else
      null;
  sessionDir = "/Users/ivypierlot/Library/agenix/session";
  sessionIdentity = "${sessionDir}/master.txt";

  pF =
    p:
    if p.kind == "age" then
      [ p.kind ]
    else
      [
        "age"
        p.kind
      ];
  rename = [
    #    (lib.mkAliasOptionModule ([
    #      "age"
    #      "warnings"
    #    ]) [ "warnings" ])
    #    (lib.mkAliasOptionModule ([
    #      "age"
    #      "warnings"
    #    ]) [ "warnings" ])
  ];

  # `age.scoped` -- per-scope groups of secrets/templates flattened back into
  # `age.secrets`/`age.templates`, plus the `secrets`/`templates`/`scoped` module
  # args. Imported next to the agenix modules in each class so the option exists
  # exactly where `age.*` does. See ./age-scope.nix for the per-aspect nesting.
  scopedModule = class: import ../../lib/age-scoped.nix { inherit lib class; };

  # homeManager rekey content as a function of the resolved identity (`anyUser`).
  mkHmRekey = anyUser: {
    imports = [
      (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
      inputs.agenix.homeManagerModules.default
      (scopedModule "homeManager")
    ]
    ++ rename;
    age.rekey.storageMode = "local";
    age.rekey.hostPubkey = lib.mkIf (anyUser.hostPublicKey != null) anyUser.hostPublicKey;
    # ONE directory for the fleet; the entity is the default `scope` of each
    # secret instead (../../lib/age-scoped.nix), which is what lets the two
    # ends of a credential name one file.
    age.rekey.generatedSecretsDir = ../../secrets/generated;
    age.scopedEntity = "${anyUser.host.name}-${anyUser.name}";
    age.rekey.localStorageDir = ../../secrets/rekeyed + "/${anyUser.host.name}-${anyUser.name}";
  };
  # Register your custom classes
  # Create routing policies for each kind → system combination
  makeRoute =
    kind: system:
    den.lib.policy.route {
      fromClass = kind;
      intoClass = system;
      adaptArgs =
        args@{ config, ... }:
        # `args`, not `args // config`. Splatting the target config put every
        # top-level option into the module-argument namespace, so a routed body
        # could bind `networking`, `services`, `launchd`, `users`... as if they
        # were module args -- shadowing real ones and making the arg set differ
        # per class. Routed bodies now get ordinary module args plus the four
        # aliases below.
        #
        # Note what is NOT here, and never was: den's entity context. A routed
        # class body becomes a plain module in the target config, so `host` /
        # `user` / `anyUser` are not bound and asking for one fails eval with
        # "attribute 'host' missing" (which is what `den.aspects.celler-user`
        # did). Take the identity off `config` instead.
        args
        // rec {
          age = config.age;
          secrets = age.secrets;
          templates = age.templates;
          scoped = age.scoped;
        };
      path = pF { inherit kind system; }; # Your existing intoPath function
    };

  # Generate all routes via cartesian product.
  #
  # `secrets` and `templates` are deliberately absent: they are still registered
  # classes (aspects keep writing them, and den's key classification needs them),
  # but aspects/security/age-scope.nix nests each aspect's content under
  # `scoped.<aspect>` instead, and the `scoped` route below is what carries it to
  # `age.scoped`. Routing them here as well would double-deliver, once flat and
  # once scoped.
  allRoutes = lib.flatten (
    lib.mapCartesianProduct
      (
        {
          kind,
          system,
        }:
        makeRoute kind system
      )
      {
        kind = [
          "scoped"
          "rekey"
          "age"
        ];
        system = [
          "nixos"
          "darwin"
          "homeManager"
        ];
      }
  );
in
{
  den.classes.secrets = { };
  den.classes.templates = { };
  den.classes.rekey = { };
  den.policies.kind-system-routes = _: allRoutes;

  imports = lib.optionals (inputs ? "agenix-rekey") [
    inputs.agenix-rekey.flakeModule
  ];

  den.aspects.agenix-rekey = {
    inputs = {
      agenix.patch.enable = true;
      agenix.patch.patches = [
        ../../patches/agenix/templates.patch
        ../../patches/agenix/edit.patch
      ];

      agenix-rekey = {
        url = "github:oddlama/agenix-rekey";
        patch.enable = true;
        patch.patches = [
          # ⚠ KEEP THIS PATCH FREE OF FORMATTING-ONLY HUNKS. It carried a third
          # hunk for `modules/agenix-rekey.nix` that reindented the
          # `assertions = [ ... ] ++ flatten (...)` block and changed nothing
          # else -- 21 removed lines against 20 added, byte-identical once
          # whitespace is stripped, which is a nixfmt reflow captured by
          # `git format-patch`. Upstream then reformatted the same block, the
          # hunk stopped applying (`Hunk #2 FAILED at 203`), and that took
          # `nix run .#update` out entirely: ../tooling/update.nix builds its
          # script with the `patched-inputs` hook's store paths interpolated
          # INTO the text, so an unappliable patch means the app cannot be
          # built and `nix flake update` never runs at all. The failure arrives
          # as a build error about `update.drv`, naming neither this file nor
          # the patch.
          #
          # So a cosmetic hunk here is not free: it buys nothing and breaks on
          # every upstream reflow, with a diagnostic pointing somewhere else.
          ../../patches/agenix-rekey/template.patch
          # macOS ships BSD `stat`, which rejects the GNU `-c %Y` the generate
          # script uses for its mtime freshness check. On failure both lookups fall
          # back to their defaults (dep→1, this→0), so `1 -gt 0` is always true and
          # every generated secret regenerates on every run. Pin the check to GNU
          # coreutils' stat (same style as the file's existing ${pkgs.coreutils}/bin/realpath).
          ../../patches/agenix-rekey/stat-portable.patch
          # `storageMode = "local"` writes each rekeyed secret to
          # <localStorageDir>/<identHash>-<secret.name>.age but only creates
          # localStorageDir. Scoped secrets put a `/` in the name (`celler/cache_key`),
          # so the target directory does not exist and reencrypt fails. (Already a
          # latent bug for aspects/services/rclone.nix, which has never been rekeyed.)
          ../../patches/agenix-rekey/rekey-mkdir-p.patch
          # `generator.derivedFrom`: regenerate a secret when the things it is a
          # function of change, not only when its file is missing. Upstream can
          # express "after this other secret" (`dependencies`, ordering plus an
          # mtime compare) but not "because this value changed".
          ../../patches/agenix-rekey/derivedFrom.patch
          # A master identity absent from this machine is skipped on decrypt.
          ../../patches/agenix-rekey/skip-absent-identities.patch
          # A source no master can read is skipped and listed, not fatal.
          ../../patches/agenix-rekey/update-masterkeys-skip-unreadable.patch
        ];
        inputs.nixpkgs.follows = "nixpkgs";
        inputs.devshell.follows = "devshell";
        inputs.treefmt-nix.follows = "treefmt-nix";
        inputs.flake-parts.follows = "flake-parts";
      };
      agenix.url = "github:ryantm/agenix";
      agenix.inputs.nixpkgs.follows = "nixpkgs";
      age-plugin-gpg = {
        url = "github:certainlach/age-plugin-gpg";
        inputs.nixpkgs.follows = "nixpkgs";
        inputs.rust-overlay.follows = "rust-overlay";
        inputs.crane.follows = "crane";

        patch.patches = [ ../../patches/age-plugin-gpg/age-plugin-gpg.patch ];
        patch.enable = true;
      };
    };

    #    meta.collisionPolicy = "den-wins";

    hmDarwin = { config, ... }: {
      age.secretsDir = "${config.home.homeDirectory}/Library/agenix/secrets";
      age.ageMountPoint = "${config.home.homeDirectory}/Library/agenix.d";
      age.templateDir = "${config.home.homeDirectory}/Library/agenix/templates";
    };

    # homeManager rekey config is keyed on `anyUser` — a context arg carrying the
    # resolved identity (name + host{name,system} + hostPublicKey), bound for both a
    # standalone home and a host-managed user (see aspects/base/anyuser.nix).
    homeManager = { anyUser, ... }: mkHmRekey anyUser;
    # Module FUNCTIONS, not bare attrsets.
    #
    # den's effect trampoline deepSeqs its handler state at every step (fx
    # src/trampoline.nix, for stack safety), so class content written as an
    # attrset is fully forced during fleet resolution -- and `agenix-rekey` is a
    # PATCHED input, so that forced its patched-tree IFD unconditionally, for
    # every scope, before anything decided whether a host wanted it. `deepSeq`
    # on a function forces the closure and not its body, so this defers both the
    # import and the IFD to the host that actually builds.
    nixos = _: {
      imports = [
        inputs.agenix.nixosModules.default
        (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
        (scopedModule "nixos")
      ];
    };
    darwin = _: {
      imports = [
        #      inputs.agenix-rekey.nixosModules.default
        (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
        inputs.agenix.darwinModules.default
        (scopedModule "darwin")
      ];
    };
    os =
      {
        host,
        inputs',
        ...
      }:
      {
        imports = rename;
        age.scopedEntity = host.hostName;
        age.rekey = {
          hostPubkey = lib.mkIf (host.hostPublicKey != null) host.hostPublicKey;
          # ONE directory for the fleet; see the home-manager half above.
          generatedSecretsDir = ../../secrets/generated;
          localStorageDir = ../../secrets/rekeyed + "/${host.hostName}/";
        };
      };

    includes = [
      den.policies.kind-system-routes
    ];

    rekey = {
      # TWO masters, and the second one is the lesson from the first.
      #
      # The gpg-yubikey identity that used to be the only entry here became
      # unusable, and every `secrets/static/*.age` was encrypted to it alone --
      # so nothing could be decrypted and nothing could be rekeyed (rekeying
      # reads the static sources). What saved the recoverable half was that the
      # per-host `secrets/rekeyed/` copies are encrypted to each host's own ssh
      # key, which we still had. See ./recover-master.sh.
      masterIdentities = [
        {
          # Apple Secure Enclave, gated by Touch ID (any-biometry). The private
          # key cannot leave this Mac's enclave, so the identity file is only a
          # reference to it and is kept OUT of the repo. The explicit `pubkey`
          # keeps evaluation independent of that file existing -- host builds
          # never read it -- and avoids a biometry prompt when merely
          # ENCRYPTING; the file is read only by the rekey CLI on this Mac.
          identity = "/Users/ivypierlot/Library/agenix/se-identity.txt";
          pubkey = enclaveMaster;
        }
      ]
      # ── THE CI MASTER ──
      # A software key held only as a GitHub environment secret, so the
      # `secrets` workflow can generate and rekey. Its file exists only on the
      # runner; every local wrapper pins the enclave as the one identity to
      # try, so nothing here ever looks for it.
      ++ lib.optional (ciMaster != null) {
        identity = ciMasterIdentity;
        pubkey = ciMaster;
      }
      ++ lib.optional (sessionMaster != null) {
        identity = sessionIdentity;
        pubkey = sessionMaster;
      };

      # The portable half, and the whole point of having two: an enclave key
      # dies with the machine. `extraEncryptionPubkeys` rather than a second
      # `masterIdentities` entry, because that is the option built for exactly
      # this -- upstream's words: "a backup identity that must be able to
      # decrypt all secrets but should not be used when attempting regular
      # decryption". Everything is encrypted to it, but rage is never handed it
      # as an identity to try (the private half is in 1Password and is
      # deliberately not on this disk, so an attempt could only fail).
      extraEncryptionPubkeys = [ ./backup-master.pub ];
      storageMode = "local";
      # Emptied deliberately: upstream defaults this to `age-plugin-yubikey`,
      # which is not the plugin this fleet uses and would be a dead dependency
      # in the rekey derivation. The plugins that are actually needed
      # (`age-plugin-se` for the enclave master) are put on PATH by the
      # `rekey`/`secret-edit`/`secret-view` wrappers below, which is where
      # decryption actually happens -- see `agePlugins` in the perSystem block.
      agePlugins = [ ];
    };
  };
  den.default.includes = [ den.aspects.agenix-rekey ];
  perSystem =
    {
      inputs',
      pkgs,
      system,
      config,
      ...
    }:
    let
      # Use the agenix-rekey package directly from inputs to avoid forcing
      # full fleet evaluation through config.agenix-rekey.package
      agenixRekeyPkg = inputs'.agenix-rekey.packages.default;
      agePlugins = [
        (inputs.age-plugin-gpg.packages.${system}.age-plugin-gpg.overrideAttrs (attrs: {
          postInstall = (attrs.postInstall or "") + ''
            ln -s $out/bin/age-plugin-gpg $out/bin/age-plugin-gpg-1
          '';
        }))
        pkgs.rage
      ]
      ++ lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
        pkgs.age-plugin-se
      ];

      withoutCeller = import ../../lib/without-celler.nix { inherit lib; };

      # Every agenix wrapper goes through here rather than calling
      # `writeShellApplication` itself, so none of them can be added later
      # without the celler guard.
      #
      # It matters for all five: `agenix` shells out to `nix` to evaluate the
      # flake, so when cache.ivymect.in is unreachable, editing a secret dies
      # after ~25s of narinfo retries against a cache that has nothing to do
      # with the operation. See lib/without-celler.nix.
      mkAgenix =
        name: argv:
        pkgs.writeShellApplication {
          inherit name;
          runtimeInputs = [ agenixRekeyPkg ] ++ agePlugins ++ withoutCeller.deps pkgs;
          text = ''
            ${withoutCeller.snippet}
            # One identity per run: the enclave here, unless `unlock`'s shell or
            # the CI runner has already chosen one.
            export AGENIX_REKEY_PRIMARY_IDENTITY="''${AGENIX_REKEY_PRIMARY_IDENTITY:-${enclaveMaster}}"
            export AGENIX_REKEY_PRIMARY_IDENTITY_ONLY="''${AGENIX_REKEY_PRIMARY_IDENTITY_ONLY:-true}"
            exec agenix ${argv} "$@"
          '';
        };
    in
    {
      packages.rekey = mkAgenix "rekey" "rekey -a";
      devshells.default = {
        packages = [ agenixRekeyPkg ] ++ agePlugins;
      };
      packages.secret-edit = mkAgenix "secret-edit" "edit";
      # `edit` opens $EDITOR; `view` decrypts to stdout. Needed by anything that
      # reads a secret back out non-interactively -- see
      # `.#tailscale-client-key`.
      packages.secret-view = mkAgenix "secret-view" "view";
      packages.gen-secrets = mkAgenix "gen-secrets" "generate -a";
      packages.update-masterkeys = mkAgenix "update-masterkeys" "update-masterkeys";

      # ── MINT THE CI MASTER ──
      # The private half goes straight from age-keygen into the `secrets`
      # environment's GitHub secret and is shredded; only the public half is
      # written to the repo. Run once, then `update-masterkeys` so the existing
      # sources are readable by it.
      # ── ONE TOUCH ID, FOR ONE SHELL ──
      # `nix run .#unlock` decrypts the session master once and opens a shell
      # that decrypts with it; leaving that shell deletes it. `lock` clears a
      # copy left by a shell that was killed outright.
      packages.unlock = pkgs.writeShellApplication {
        name = "unlock";
        runtimeInputs = agePlugins ++ [ pkgs.coreutils ];
        text = ''
          if [ -n "''${AGENIX_REKEY_PRIMARY_IDENTITY:-}" ]; then
            echo "unlock: already inside an unlocked shell" >&2
            exit 1
          fi
          if [ ! -e aspects/security/session-master.age ] || [ ! -e aspects/security/session-master.pub ]; then
            echo "unlock: run from the repo root, after nix run .#session-master-init" >&2
            exit 1
          fi
          install -d -m 700 ${sessionDir}
          trap 'rm -f ${sessionIdentity}' EXIT HUP INT TERM
          (
            umask 077
            rage -d -i ${lib.escapeShellArg "/Users/ivypierlot/Library/agenix/se-identity.txt"} \
              -o ${sessionIdentity} aspects/security/session-master.age
          )
          echo "unlocked; exit this shell to lock" >&2
          AGENIX_REKEY_PRIMARY_IDENTITY="$(tr -d '\n' < aspects/security/session-master.pub)" \
            AGENIX_REKEY_PRIMARY_IDENTITY_ONLY=true \
            AGENIX_UNLOCKED=1 \
            "''${SHELL:-${pkgs.bashInteractive}/bin/bash}" || true
          echo "locked" >&2
        '';
      };
      packages.lock = pkgs.writeShellApplication {
        name = "lock";
        runtimeInputs = [ pkgs.coreutils ];
        text = ''
          rm -f ${sessionIdentity}
          echo "locked" >&2
        '';
      };

      # Mint the session master: its private half goes straight into
      # `session-master.age`, encrypted to the enclave and the backup, never
      # anywhere else. Then `update-masterkeys` once so everything is readable
      # by it -- the last run that prompts per file.
      packages.session-master-init = pkgs.writeShellApplication {
        name = "session-master-init";
        runtimeInputs = [
          pkgs.age
          pkgs.rage
          pkgs.coreutils
        ];
        text = ''
          if [ ! -e flake.nix ]; then
            echo "session-master-init: run this from the repo root" >&2
            exit 1
          fi
          if [ -e aspects/security/session-master.pub ]; then
            echo "session-master-init: aspects/security/session-master.pub exists; delete it and session-master.age to rotate" >&2
            exit 1
          fi
          dir="$(mktemp -d)"
          trap 'rm -rf "$dir"' EXIT
          age-keygen -o "$dir/key" 2>/dev/null
          pub="$(age-keygen -y "$dir/key")"
          rage -e -r ${enclaveMaster} -R aspects/security/backup-master.pub \
            -o aspects/security/session-master.age "$dir/key"
          shred -u "$dir/key" 2>/dev/null || rm -f "$dir/key"
          printf '%s\n' "$pub" > aspects/security/session-master.pub
          echo "wrote session-master.{age,pub}; now: jj file track them, then nix run .#update-masterkeys" >&2
        '';
      };

      packages.ci-master-init = pkgs.writeShellApplication {
        name = "ci-master-init";
        runtimeInputs = [
          pkgs.age
          pkgs.gh
          pkgs.coreutils
        ];
        text = ''
          if [ ! -e flake.nix ]; then
            echo "ci-master-init: run this from the repo root" >&2
            exit 1
          fi
          if [ -e aspects/security/ci-master.pub ]; then
            echo "ci-master-init: aspects/security/ci-master.pub exists; delete it to rotate" >&2
            exit 1
          fi
          repo="$(gh repo view --json nameWithOwner -q .nameWithOwner)"

          # The environment, deployable from `master` alone: that rule is what
          # keeps the key from any other ref.
          gh api -X PUT "repos/$repo/environments/secrets" --input - >/dev/null <<'JSON'
          {"deployment_branch_policy": {"protected_branches": false, "custom_branch_policies": true}}
          JSON
          if ! gh api "repos/$repo/environments/secrets/deployment-branch-policies" \
            -q '.branch_policies[].name' | grep -qx master; then
            gh api -X POST "repos/$repo/environments/secrets/deployment-branch-policies" \
              -f name=master -f type=branch >/dev/null
          fi

          dir="$(mktemp -d)"
          trap 'rm -rf "$dir"' EXIT
          age-keygen -o "$dir/key" 2>/dev/null
          # The public half is written only once the private half is stored, so
          # a failed upload leaves no recipient whose key is gone.
          pub="$(age-keygen -y "$dir/key")"
          gh secret set AGENIX_CI_MASTER --env secrets --repo "$repo" < "$dir/key"
          shred -u "$dir/key" 2>/dev/null || rm -f "$dir/key"
          printf '%s\n' "$pub" > aspects/security/ci-master.pub
          echo "wrote aspects/security/ci-master.pub; now: jj file track it, then nix run .#update-masterkeys" >&2
        '';
      };

      # Mobile devices join the node set. Merged into `homeConfigurations`
      # because select-nodes.nix prefixes the platform sets (`darwin:<name>`)
      # and leaves home ones unprefixed, so the key stays `iphone` rather than
      # claiming a platform the device does not run. `config ? age` is agenix's
      # own filter, so this is inert until a device declares secrets.
      agenix-rekey.homeConfigurations =
        lib.filterAttrs (_: x: x.config ? age) (inputs.self.homeConfigurations or { })
        // lib.filterAttrs (_: x: x.config ? age) (inputs.self.mobileConfigurations or { });
    };
}
