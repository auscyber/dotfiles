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
            expect_content="original"
            if [ "$actual_content" != "$expect_content" ]; then
              declare -p actual_content
              declare -p expect_content
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
            expect_remotes="\
            origin''\t/build/./origin (fetch)
            origin''\t/build/./origin (push)
            upstream''\t/build/dummy-input (fetch)
            upstream''\t/build/dummy-input (push)"

            if [ "$actual_remotes" != "$expect_remotes" ]; then
              declare -p actual_remotes
              declare -p expect_remotes
              exit 1
            fi

            actual_refs=$( (
              cd ${baseDir}/${inputName}
              git show-ref --abbrev=4 | cut -d' ' -f2
            ))
            expect_refs="\
            refs/heads/inputs/dummy
            refs/remotes/origin/inputs/dummy"

            if [ "$actual_refs" != "$expect_refs" ]; then
              declare -p actual_refs
              declare -p expect_refs
              exit 1
            fi

            actual_origin_fetch_refspec=$( (
              cd ${baseDir}/${inputName}
              git config --get remote.origin.fetch
            ))
            expect_origin_fetch_refspec='+refs/heads/*:refs/remotes/origin/*'

            if [ "$actual_origin_fetch_refspec" != "$expect_origin_fetch_refspec" ]; then
              declare -p actual_origin_fetch_refspec
              declare -p expect_origin_fetch_refspec
              exit 1
            fi

            actual_upstream_fetch_refspec=$( (
              cd ${baseDir}/${inputName}
              git config --get remote.upstream.fetch
            ))
            expect_upstream_fetch_refspec='+refs/heads/*:refs/remotes/upstream/*'

            if [ "$actual_upstream_fetch_refspec" != "$expect_upstream_fetch_refspec" ]; then
              declare -p actual_upstream_fetch_refspec
              declare -p expect_upstream_fetch_refspec
              exit 1
            fi

            actual_checked_out_branch=$( (
              cd ${baseDir}/${inputName}
              git branch --show-current
            ))
            expect_checked_out_branch="inputs/dummy"

            if [ "$actual_checked_out_branch" != "$expect_checked_out_branch" ]; then
              declare -p actual_checked_out_branch
              declare -p expect_checked_out_branch
              exit 1
            fi

            expect_submodule_rev=$(nix flake metadata --json | jq --raw-output .locks.nodes.${inputName}.locked.rev)

            actual_submodule_rev=$( (
              cd ${baseDir}/${inputName}
              git rev-parse HEAD
            ))

            if [ "$actual_submodule_rev" != "$expect_submodule_rev" ]; then
              declare -p actual_submodule_rev
              declare -p expect_submodule_rev
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
