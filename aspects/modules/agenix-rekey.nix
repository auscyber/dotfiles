{
  inputs,
  lib,
  den,
  ...
}:
let

  agenix-class =
    {
      class,
      aspect-chain,
      ...
    }:
    den.batteries.forward {
      each = lib.cartesianProduct {
        kind = [
          "secrets"
          "templates"
          "rekey"
          "age"
        ];
        system = [
          "nixos"
          "homeManager"
          "darwin"
        ];
      };
      fromClass = p: p.kind; # source class: secrets / templates
      intoClass = p: p.system; # target class
      intoPath =
        p:
        if p.kind == "age" then
          [ p.kind ]
        else
          [
            "age"
            p.kind
          ]; # age.secrets / age.templates
      adaptArgs = lib.id;
      fromAspect = _item: lib.head aspect-chain;
    };
in
{
  flake-file.inputs = {
    agenix-rekey.url = "github:auscyber/agenix-rekey?ref=add-template-support";
    agenix.url = "github:auscyber/agenix?ref=add-templates";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    age-plugin-gpg = {
      type = "git";
      url = "./.";
      ref = "inputs/age-plugin-gpg";

      inputs.rust-overlay.follows = "rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  imports = lib.optionals (inputs ? "agenix-rekey") [
    inputs.agenix-rekey.flakeModule
  ];

  den.aspects.agenix-rekey = {

    provides.to-users =
      {
        host,
        user,
        ...
      }:
      {
        homeManager = {
          imports = [

            (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
            inputs.agenix.homeManagerModules.default
            (lib.mkAliasOptionModule [ "age" "rekey" "warnings" ] [ "warnings" ])
          ];
          age.rekey.hostPubkey = lib.mkIf (user.hostPublicKey != null) user.hostPublicKey;
          age.rekey.generatedSecretsDir = ../../secrets/generated + "/${host.name}/${user.name}";
          age.rekey.localStorageDir = ../../secrets/rekeyed + "/${host.name}/${user.name}";

        };
      };
    nixos = {
      imports = [
        inputs.agenix.nixosModules.default

        (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
        (lib.mkAliasOptionModule [ "age" "rekey" "warnings" ] [ "warnings" ])
      ];
    };
    darwin.imports = [
      #      inputs.agenix-rekey.nixosModules.default

      (import "${inputs.agenix-rekey}/modules/agenix-rekey.nix" inputs.nixpkgs)
      inputs.agenix.darwinModules.default
      (lib.mkAliasOptionModule [ "age" "rekey" "warnings" ] [ "warnings" ])
    ];
    os =
      { host, inputs', ... }:
      {

        age.rekey.hostPubkey = lib.mkIf (host.hostPublicKey != null) host.hostPublicKey;
        age.rekey.generatedSecretsDir = ../../secrets/generated + "/${host.name}/";
        age.rekey.localStorageDir = ../../secrets/rekeyed + "/${host.name}/";
      };

    includes = [
      agenix-class

    ];

    rekey = { inputs', host, ... }: {

      masterIdentities = [ { identity = ./gpg-yubikey.pub; } ];
      storageMode = "local";
    };

  };
  den.schema.host.includes = [ den.aspects.agenix-rekey ];
  perSystem =
    {
      inputs',
      pkgs,
      config,
      ...
    }:
    let
      agePlugins = [
        (inputs'.age-plugin-gpg.packages.default.overrideAttrs (attrs: {
          postInstall = (attrs.postInstall or "") + ''
            ln -s $out/bin/age-plugin-gpg $out/bin/age-plugin-gpg-1
          '';
        }))
      ];
    in
    {

      packages.rekey = config.agenix-rekey.package;
      apps.rekey.program = lib.getExe (
        pkgs.writeShellApplication {
          name = "rekey";
          runtimeInputs = [ config.agenix-rekey.package ] ++ agePlugins;
          text = ''exec agenix rekey -a "$@"'';
        }
      );
      apps.gen-secrets.program = lib.getExe (
        pkgs.writeShellApplication {
          name = "gen-secrets";
          runtimeInputs = [ config.agenix-rekey.package ] ++ agePlugins;
          text = ''exec agenix generate -a "$@"'';
        }
      );
    };

}
