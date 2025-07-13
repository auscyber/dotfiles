{ baseDir, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      testCases = [
        {
          title = "fail-non-flake-input";
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

              sed --in-place 's/flake = true;/flake = false;/' flake.nix
              if nix eval .#dummy; then
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
