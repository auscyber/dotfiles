{ baseDir, ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      inputName = "dummy";

      expectedGitmodules = pkgs.writeText ".gitmodules" ''
        [submodule "${baseDir}/${inputName}"]
        ''\tpath = ${baseDir}/${inputName}
        ''\turl = ./.
      '';
    in
    {
      testCases.happy-path = {
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
                  let
                    psCfg = psArgs.config.input-branches;
                  in
                  {
                    packages.default = pkgs.symlinkJoin {
                      name = "commands";
                      paths = psCfg.commands.all;
                    };
                  };

                flake.dummy = lib.readFile (inputs.dummy + "/content");
              }
            '';

        script = pkgs.writeShellApplication {
          name = "script";
          runtimeInputs = [ pkgs.jq ];
          text = ''
            actual_content=$(nix eval --raw .#dummy)
            expected_content="original"
            if [ "$actual_content" != "$expected_content" ]; then
              declare -p actual_content
              declare -p expected_content
              exit 1
            fi

            nix build
            ./result/bin/input-branch-init-dummy

            if ! diff_output=$(diff --unified ${expectedGitmodules} .gitmodules); then
                echo "$diff_output"
                exit 1
            fi

            actual_remotes=$( (
              cd ${baseDir}/${inputName}
              git remote --verbose
            ))
            expected_remotes="\
            origin''\t/build/./origin/. (fetch)
            origin''\t/build/./origin/. (push)
            upstream''\t/build/dummy-input (fetch)
            upstream''\t/build/dummy-input (push)"

            if [ "$actual_remotes" != "$expected_remotes" ]; then
              declare -p actual_remotes
              declare -p expected_remotes
              exit 1
            fi

            actual_refs=$( (
              cd ${baseDir}/${inputName}
              git show-ref --abbrev=4 | cut -d' ' -f2
            ))
            expected_refs="\
            refs/heads/inputs/dummy
            refs/heads/main
            refs/remotes/origin/HEAD
            refs/remotes/origin/inputs/dummy
            refs/remotes/origin/main"

            if [ "$actual_refs" != "$expected_refs" ]; then
              declare -p actual_refs
              declare -p expected_refs
              exit 1
            fi

            actual_fetch_refspec=$( (
              cd ${baseDir}/${inputName}
              git config --get remote.origin.fetch
            ))
            expected_fetch_refspec='+refs/heads/*:refs/remotes/origin/*'

            if [ "$actual_fetch_refspec" != "$expected_fetch_refspec" ]; then
              declare -p actual_fetch_refspec
              declare -p expected_fetch_refspec
              exit 1
            fi

            actual_checked_out_branch=$( (
              cd ${baseDir}/${inputName}
              git branch --show-current
            ))
            expected_checked_out_branch="inputs/dummy"

            if [ "$actual_checked_out_branch" != "$expected_checked_out_branch" ]; then
              declare -p actual_checked_out_branch
              declare -p expected_checked_out_branch
              exit 1
            fi

            flake_locked_rev=$(nix flake metadata --json | jq --raw-output .locks.nodes.${inputName}.locked.rev)

            submodule_rev=$( (
              cd ${baseDir}/${inputName}
              git rev-parse HEAD
            ))

            if [ "$flake_locked_rev" != "$submodule_rev" ]; then
              declare -p flake_locked_rev
              declare -p submodule_rev
              exit 1
            fi

            new_submodule_content="altered"

            (
              cd ${baseDir}/${inputName}
              echo -n "$new_submodule_content" > content
              git add content
              git commit --quiet --message "change"
              git rev-parse HEAD
            )

            # https://github.com/NixOS/nix/issues/13324
            touch dirt
            git add --intent-to-add dirt

            actual_submodule_content=$(nix eval --raw .#dummy --override-input ${inputName} ./${baseDir}/${inputName})

            if [ "$actual_submodule_content" != "$new_submodule_content" ]; then
              declare -p actual_submodule_content
              declare -p new_submodule_content
              exit 1
            fi

            new_upstream_rev=$( (
              cd ../dummy-input
              git commit --quiet --allow-empty --message "empty"
              git rev-parse HEAD
            ))

            nix build
            ./result/bin/input-branch-rebase-dummy

            submodule_parent_rev=$( (
              cd ${baseDir}/${inputName}
              git rev-parse HEAD~
            ))

            if [ "$submodule_parent_rev" != "$new_upstream_rev" ]; then
              declare -p submodule_parent_rev
              declare -p new_upstream_rev
              exit 1
            fi

            new_submodule_rev=$( (
              cd ${baseDir}/${inputName}
              git rev-parse HEAD
            ))

            nix build
            ./result/bin/input-branch-push-force-dummy

            git fetch origin
            new_origin_rev=$(git rev-parse origin/inputs/${inputName})

            if [ "$new_submodule_rev" != "$new_origin_rev" ]; then
              declare -p new_upstream_rev
              declare -p new_submodule_rev
              exit 1
            fi

            declare out
            touch "$out" 
          '';
        };
      };
    };
}
