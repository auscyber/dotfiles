{
  lib,
  inputs,
  flake-parts-lib,
  ...
}:
{
  options.perSystem = flake-parts-lib.mkPerSystemOption {
    options.testCases = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            title = lib.mkOption {
              type = lib.types.singleLineStr;
            };
            module = lib.mkOption {
              type = lib.types.package;
            };
            script = lib.mkOption {
              type = lib.types.package;
            };
          };
        }
      );
    };
  };
  config = {
    _module.args.baseDir = "inputs";

    perSystem =
      psArgs@{ pkgs, ... }:
      {
        checks =
          lib.pipe
            {
              testCase = psArgs.config.testCases;
              # TODO document
              nix = lib.pipe pkgs.nixVersions [
                (lib.filterAttrs (name: _: lib.hasPrefix "nix_" name))
                lib.attrValues
                (lib.filter (nix: (builtins.tryEval nix).success))
                # Support for `inputs.self.submodules` added in 2.27.0
                # https://nix.dev/manual/nix/2.27/release-notes/rl-2.27.html
                (lib.filter (nix: lib.versionAtLeast nix.version "2.27"))
              ];
            }
            [

              lib.cartesianProduct
              (lib.map (
                { testCase, nix }:
                let
                  inherit (testCase) title module script;

                  input-branches-src = lib.fileset.toSource {
                    root = ../..;
                    fileset = lib.fileset.unions [
                      ../../flake.lock
                      ../../flake.nix
                      ../../modules
                    ];
                  };

                  flake =
                    pkgs.writeText "test-case-${title}-flake.nix"
                      # nix
                      ''
                        {
                          inputs = {
                            self.submodules = true;

                            input-branches = {
                              url = "${input-branches-src}";
                              inputs.flake-parts.follows = "flake-parts";
                            };
                            flake-parts = {
                              url = "${inputs.flake-parts}";
                              inputs.nixpkgs-lib.follows = "nixpkgs";
                            };
                            nixpkgs.url = "${inputs.nixpkgs}";
                            systems.url = "${inputs.systems}";
                            dummy.url = "git+file:///build/dummy-input";
                          };
                          outputs =
                            inputs:
                            inputs.flake-parts.lib.mkFlake { inherit inputs; } {
                              systems = import inputs.systems;
                              imports = [
                                inputs.input-branches.flakeModules.default
                                ./module.nix
                              ];
                            };
                        }
                      '';
                in
                {
                  name = "integration/${title}/${nix.name}";
                  value =
                    pkgs.runCommand title
                      {
                        nativeBuildInputs = [
                          nix
                          pkgs.git
                        ];
                        requiredSystemFeatures = [ "recursive-nix" ];
                        env.NIX_CONFIG = ''
                          extra-experimental-features = nix-command flakes
                        '';
                      }
                      ''
                        set -o errexit
                        set -o nounset
                        set -o pipefail
                        set -o xtrace

                        export HOME="$(pwd)"

                        git config --global user.name "test runner"
                        git config --global user.email "test@example.com"
                        git config --global protocol.file.allow always
                        git config --global init.defaultBranch main
                        (
                          mkdir dummy-input
                          cd dummy-input
                          git init --initial-branch=master
                          echo '{outputs=_:{};}' > flake.nix
                          echo -n original > content
                          git add .
                          git commit --message "initial commit"
                        )
                        (
                          mkdir origin
                          cd origin
                          cp ${flake} flake.nix
                          cp ${module} module.nix
                          git init .
                          git add .
                          git commit --message "initial commit"
                          git checkout --detach HEAD
                        )
                        git clone ./origin ./test-case
                        cd test-case
                        ${lib.getExe script}
                      '';
                }
              ))
              lib.listToAttrs
            ];
      };
  };
}
