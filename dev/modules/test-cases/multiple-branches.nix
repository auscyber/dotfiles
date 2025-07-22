{ baseDir, ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      inputName = "dummy";
    in
    {
      testCases = [
        {
          title = "multiple-branches";
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

              sed --in-place 's#"git+file:///build/dummy-input"#"./${baseDir}/${inputName}"#' flake.nix

              git add .
              git commit -m'input-branch'
              "$result/bin/input-branch-push-force-dummy"
              git push

              actual_refs=$( (
                cd ${baseDir}/${inputName}
                git show-ref --abbrev=4 | cut -d' ' -f2
              ))
              expect_refs="\
              refs/heads/inputs/main/dummy
              refs/heads/main
              refs/remotes/origin/HEAD
              refs/remotes/origin/inputs/main/dummy
              refs/remotes/origin/main"

              if [ "$actual_refs" != "$expect_refs" ]; then
                declare -p actual_refs
                declare -p expect_refs
                exit 1
              fi

              actual_checked_out_branch=$( (
                cd ${baseDir}/${inputName}
                git branch --show-current
              ))
              expect_checked_out_branch="inputs/main/dummy"

              if [ "$actual_checked_out_branch" != "$expect_checked_out_branch" ]; then
                declare -p actual_checked_out_branch
                declare -p expect_checked_out_branch
                exit 1
              fi

              git switch -c feature
              "$result/bin/input-branch-push-force-dummy"

              actual_refs=$( (
                cd ${baseDir}/${inputName}
                git show-ref --abbrev=4 | cut -d' ' -f2
              ))
              expect_refs="\
              refs/heads/inputs/feature/dummy
              refs/heads/inputs/main/dummy
              refs/heads/main
              refs/remotes/origin/HEAD
              refs/remotes/origin/inputs/feature/dummy
              refs/remotes/origin/inputs/main/dummy
              refs/remotes/origin/main"

              if [ "$actual_refs" != "$expect_refs" ]; then
                declare -p actual_refs
                declare -p expect_refs
                exit 1
              fi

              actual_checked_out_branch=$( (
                cd ${baseDir}/${inputName}
                git branch --show-current
              ))
              # Automatically switching submodule branches not supported yet (if ever)
              expect_checked_out_branch="inputs/main/dummy"

              if [ "$actual_checked_out_branch" != "$expect_checked_out_branch" ]; then
                declare -p actual_checked_out_branch
                declare -p expect_checked_out_branch
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
