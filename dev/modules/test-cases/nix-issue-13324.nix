# https://github.com/NixOS/nix/issues/13324
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
          title = "nix-issue-13324";
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

                  flake.dummy = lib.readFile (inputs.dummy + "/content");
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

              new_submodule_content="altered"

              (
                cd ${baseDir}/${inputName}
                echo -n "$new_submodule_content" > content
              )

              actual_content=$(nix eval --raw .#dummy)

              expect_content="original"
              if [ "$actual_content" != "$expect_content" ]; then
                declare -p actual_content
                declare -p expect_content
                exit 1
              fi

              touch dirt
              git add --intent-to-add dirt

              actual_submodule_content=$(nix eval --raw .#dummy)
              expect_submodule_content="$new_submodule_content"

              if [ "$actual_submodule_content" != "$expect_submodule_content" ]; then
                declare -p actual_submodule_content
                declare -p expect_submodule_content
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
