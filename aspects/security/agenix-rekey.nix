{
  inputs,
  realInputs,
  lib,
  den,
  ...
}:
let
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
    age.rekey.generatedSecretsDir = ../../secrets/generated + "/${anyUser.host.name}-${anyUser.name}";
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
        # "attribute 'host' missing" (which is what `den.aspects.celler-push`
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

  ff = {
    agenix.patch.enable = true;
    agenix.patch.patches = [
      ../../patches/agenix/templates.patch
      ../../patches/agenix/edit.patch
    ];

    agenix-rekey = {
      url = "github:oddlama/agenix-rekey";
      patch.enable = true;
      patch.patches = [
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
      ];
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = "darwin";
    agenix.inputs.home-manager.follows = "home-manager";
    age-plugin-gpg = {
      url = "github:certainlach/age-plugin-gpg";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust-overlay.follows = "rust-overlay";
      inputs.crane.follows = "crane";

      patch.patches = [ ../../patches/age-plugin-gpg/age-plugin-gpg.patch ];
      patch.enable = true;
    };
  };
  patchedInputs = {
  };

  imports = lib.optionals (inputs ? "agenix-rekey") [
    inputs.agenix-rekey.flakeModule
  ];

  den.aspects.agenix-rekey = {
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
    nixos = {
      imports = [
        inputs.agenix.nixosModules.default
        (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
        (scopedModule "nixos")
      ];
    };
    darwin.imports = [
      #      inputs.agenix-rekey.nixosModules.default
      (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
      inputs.agenix.darwinModules.default
      (scopedModule "darwin")
    ];
    os =
      {
        host,
        inputs',
        ...
      }:
      {
        imports = rename;
        age.rekey = {
          hostPubkey = lib.mkIf (host.hostPublicKey != null) host.hostPublicKey;
          generatedSecretsDir = ../../secrets/generated + "/${host.name}/";
          localStorageDir = ../../secrets/rekeyed + "/${host.name}/";
        };
      };

    includes = [
      den.policies.kind-system-routes
    ];

    rekey = {
      masterIdentities = [
        { identity = ./gpg-yubikey.pub; }
        #        {
        #          # Apple Secure Enclave master key — usable only on this Mac, and
        #          # decryption is gated by Touch ID (any-biometry). The identity is the
        #          # enclave-bound private-key reference, so it is kept OUT of the repo
        #          # (at an absolute path) and only the recipient below is committed.
        #          # An explicit pubkey keeps eval independent of the file's presence
        #          # (host builds never read it) and avoids an extra biometry prompt when
        #          # encrypting; the file is read only by the rekey CLI on this Mac.
        #          identity = "/Users/ivypierlot/Library/agenix/se-identity.txt";
        #          pubkey = "age1se1qgnzav6c967adnfme32lr827v0vp8ddus96l78s4h4yqtnc0tuydyqee780";
        #        }
      ];
      storageMode = "local";
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
      ++ lib.optionals pkgs.stdenv.isDarwin [
        pkgs.age-plugin-se
      ];
    in
    {
      packages.rekey = pkgs.writeShellApplication {
        name = "rekey";
        runtimeInputs = [ agenixRekeyPkg ] ++ agePlugins;
        text = ''exec agenix rekey -a "$@"'';
      };
      devshells.default = {
        packages = [ agenixRekeyPkg ] ++ agePlugins;
      };
      packages.secret-edit = pkgs.writeShellApplication {
        name = "secret-edit";
        runtimeInputs = [ agenixRekeyPkg ] ++ agePlugins;
        text = ''exec agenix edit "$@"'';
      };
      packages.gen-secrets = pkgs.writeShellApplication {
        name = "gen-secrets";
        runtimeInputs = [ agenixRekeyPkg ] ++ agePlugins;
        text = ''exec agenix generate -a "$@"'';
      };
      packages.update-masterkeys = pkgs.writeShellApplication {
        name = "update-masterkeys";
        runtimeInputs = [ agenixRekeyPkg ] ++ agePlugins;
        text = ''exec agenix update-masterkeys "$@"'';
      };
    };
}
