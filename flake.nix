{
  nixConfig = {
    abort-on-warn = true;
    allow-import-from-derivation = false;
  };

  inputs = {
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
    };
    nixpkgs-lib.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { lib, ... }:
      {
        systems = [ ];

        partitionedAttrs = lib.genAttrs [ "checks" "devShells" "formatter" ] (_: "dev");
        partitions.dev = {
          extraInputsFlake = ./dev;
          module = ./dev/imports.nix;
        };

        imports = [
          ./modules/flake-module.nix
          ./modules/nixos-module.nix
          inputs.flake-parts.flakeModules.flakeModules
          inputs.flake-parts.flakeModules.modules
          inputs.flake-parts.flakeModules.partitions
        ];
      }
    );
}
