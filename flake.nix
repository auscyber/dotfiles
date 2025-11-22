{
  description = "Age plugin utilizing gpg-agent";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-25.05";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
    shelly.url = "github:CertainLach/shelly";
  };
  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake
      {
        inherit inputs;
      }
      {
        imports = [ inputs.shelly.flakeModule ];
        systems = inputs.nixpkgs.lib.systems.flakeExposed;

        perSystem =
          {
            config,
            system,
            pkgs,
            self,
            inputs',
            ...
          }:
          let
            rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
            craneLib = (inputs.crane.mkLib pkgs).overrideToolchain rust;

            sharedDependencies = with pkgs; [ nettle ];
            sharedBuildDependencies = with pkgs; [
              pkg-config
              rustPlatform.bindgenHook
            ];
          in
          {
            _module.args.pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = [ inputs.rust-overlay.overlays.default ];
            };

            packages =
              let
                age-plugin-gpg = craneLib.buildPackage {
                  src = ./.;
                  buildInputs = sharedDependencies;
                  nativeBuildInputs = sharedBuildDependencies;
                };
              in
              {
                inherit age-plugin-gpg;
                default = age-plugin-gpg;
              };

            shelly.shells.default = {
              factory = craneLib.devShell;
              packages =
                with pkgs;
                [
                  nettle
                  rust
                  rage
                ]
                ++ sharedDependencies
                ++ sharedBuildDependencies;
              shellHook = ''
                export PATH=$PATH:$PWD/target/debug
              '';
            };
          };
      };
}
