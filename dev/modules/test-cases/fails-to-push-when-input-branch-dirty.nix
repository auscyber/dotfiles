{ baseDir, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      testCases = [
        {
          title = "fails-to-push-when-input-branch-dirty";
          module =
            pkgs.writeText "module.nix"
              # nix
              ''
                { lib, inputs, ... }:
                {
                  input-branches.inputs.dummy.upstream = {
                    url = "/build/dummy-input";
                    ref = "master";
                  };
                  perSystem =
                    psArgs@{ pkgs, ... }:
                    {
                      packages.default = pkgs.symlinkJoin {
                        name = "commands";
                        paths = psArgs.config.input-branches.commands.all;
                      };
                    };
                }
              '';

          script = pkgs.writeShellApplication {
            name = "script";
            runtimeInputs = [ pkgs.jq ];
            text = ''
              set -o xtrace

              result=$(nix build --no-link --print-out-paths)
              "$result/bin/input-branch-init-dummy"
              sed --in-place 's#"git+file:///build/dummy-input"#"./${baseDir}/dummy"#' flake.nix
              git add .
              git commit -m'inputs/dummy'
              "$result/bin/input-branch-push-force-dummy"

              (
                cd inputs/dummy
                touch something
                git add something
                git commit --message "something"
              )

              if "$result/bin/input-branch-push-force-dummy"; then
                echo "must fail to push when input-branch submodule is not at tracked rev"
                exit 1
              fi

              git add inputs/dummy
              git commit -m'inputs/dummy something'
              "$result/bin/input-branch-push-force-dummy"

              (
                cd inputs/dummy
                touch dirt
                git add --intent-to-add dirt
              )

              if "$result/bin/input-branch-push-force-dummy"; then
                echo "must fail to push when input-branch submodule dirty"
                exit 1
              fi

              declare out
              touch "$out"
            '';
          };
        }
      ];
    };
}
