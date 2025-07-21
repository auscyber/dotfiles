{ baseDir, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      testCases = [
        {
          title = "fails-when-no-inputs-self-submodules-true";
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

              sed --in-place 's#"git+file:///build/dummy-input"#"./${baseDir}/dummy"#' flake.nix

              git add .
              git commit -m'input-branch'
              "$result/bin/input-branch-push-force-dummy"
              git push

              actual_content=$(nix eval --raw .#dummy)

              expect_content="original"
              if [ "$actual_content" != "$expect_content" ]; then
                declare -p actual_content
                declare -p expect_content
                exit 1
              fi

              sed --in-place 's/self\.submodules = true;//' flake.nix

              # sanity check
              nix flake show

              if nix eval --raw .#dummy; then
                echo "should have failed"
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
