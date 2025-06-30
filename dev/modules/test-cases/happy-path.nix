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
                    packages = {
                      init = lib.head psCfg.commands.init;
                      rebase = lib.head psCfg.commands.rebase;
                      push = lib.head psCfg.commands.push-force;
                    };
                  };

                flake.dummy = lib.readFile (inputs.dummy + "/content");
              }
            '';

        script = pkgs.writeShellApplication {
          name = "script";
          runtimeInputs = [ pkgs.jq ];
          text = ''
            content=$(nix eval --raw .#dummy)
            expected_content="original"
            if [ "$content" != "$expected_content" ]; then
              declare -p content
              declare -p expected_content
              exit 1
            fi

            nix run .#init

            if ! diff_output=$(diff --unified ${expectedGitmodules} .gitmodules); then
                echo "$diff_output"
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

            nix run .#rebase

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

            nix run .#push

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
