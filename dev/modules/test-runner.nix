{
  lib,
  inputs,
  flake-parts-lib,
  ...
}:
{
  options.perSystem = flake-parts-lib.mkPerSystemOption {
    options.testCases = lib.mkOption {
      type = lib.types.lazyAttrsOf (
        lib.types.submodule {
          options = {
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
          psArgs.config.testCases
          |> lib.mapAttrs' (
            name:
            { module, script }:
            let
              flake =
                pkgs.writeText "test-case-${name}-flake.nix"
                  # nix
                  ''
                    {
                      inputs = {
                        input-branches.url = "${inputs.input-branches}";
                        flake-parts = {
                          url = "${inputs.flake-parts}";
                          inputs.nixpkgs-lib.follows = "nixpkgs";
                        };
                        nixpkgs.url = "${inputs.nixpkgs}";
                        systems.url = "${inputs.systems}";
                        dummy = {
                          url = "git+file:///build/dummy-input";
                          flake = false;
                        };
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
              name = "integration/${name}";
              value =
                pkgs.runCommand name
                  {
                    nativeBuildInputs = [
                      (lib.pipe pkgs.nixVersions [
                        lib.attrNames
                        (lib.filter (lib.hasPrefix "nix_"))
                        lib.naturalSort
                        lib.last
                        (lib.flip lib.getAttr pkgs.nixVersions)
                      ])
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

                    export HOME="$(pwd)"

                    git config --global user.name "test runner"
                    git config --global user.email "test@example.com"
                    git config --global protocol.file.allow always
                    git config --global init.defaultBranch main
                    (
                      mkdir dummy-input
                      cd dummy-input
                      git init --initial-branch=master
                      echo -n original > content
                      git add content
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
                    git daemon --verbose --informative-errors --base-path=. --export-all --enable=receive-pack &
                    git clone git://localhost/origin ./test-case
                    cd test-case
                    ${lib.getExe script}
                  '';
            }
          );
      };
  };
}
