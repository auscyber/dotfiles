{ baseDir, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      testCases = [
        {
          title = "cancel-rebase-when-not-clean";
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
              git commit -m'input-branch'
              "$result/bin/input-branch-push-force-dummy"
              git push

              (
                cd "${baseDir}/dummy"
                touch dirt
                git add --intent-to-add dirt
              )

              result=$(nix build --no-link --print-out-paths)

              set +o errexit
              "$result/bin/input-branch-rebase-dummy"
              EXIT_CODE=$?
              set -o errexit

              if [ $EXIT_CODE -ne 70 ]; then
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
