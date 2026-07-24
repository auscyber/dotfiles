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
  # Register your custom classes
  # Create routing policies for each kind → system combination
  makeRoute =
    kind: system:
    den.lib.policy.route {
      fromClass = kind;
      intoClass = system;
      adaptArgs =
        { config, ... }:
        config
        // rec {
          age = config.age;
          secrets = age.secrets;
        };
      path = pF { inherit kind system; }; # Your existing intoPath function
    };

  # Generate all routes via cartesian product
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
          "secrets"
          "templates"
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
    meta.collisionPolicy = "den-wins";
    provides.to-users =
      {
        host,
        user,
        ...
      }:
      {
        hmDarwin = { config, ... }: {
          age.secretsDir = "${config.home.homeDirectory}/Library/agenix/secrets";
          age.ageMountPoint = "${config.home.homeDirectory}/Library/agenix.d";
          age.templateDir = "${config.home.homeDirectory}/Library/agenix/templates";
        };

        homeManager = {
          imports = [
            (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
            inputs.agenix.homeManagerModules.default
          ]
          ++ rename;
          age.rekey.storageMode = "local";
          age.rekey.hostPubkey = lib.mkIf (user.hostPublicKey != null) user.hostPublicKey;
          age.rekey.generatedSecretsDir = ../../secrets/generated + "/${host.name}-${user.name}";
          age.rekey.localStorageDir = ../../secrets/rekeyed + "/${host.name}-${user.name}";
        };
      };
    nixos = {
      imports = [
        inputs.agenix.nixosModules.default
        (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
      ];
    };
    darwin.imports = [
      #      inputs.agenix-rekey.nixosModules.default
      (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
      inputs.agenix.darwinModules.default
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
