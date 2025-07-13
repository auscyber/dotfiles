{
  perSystem =
    { pkgs, system, ... }:
    {
      testCases = [
        {
          title = "no-such-input";
          module =
            pkgs.writeText "module.nix"
              # nix
              ''
                {
                  input-branches.inputs.bogus.upstream = {
                    url = "https://example.com/bogus.git";
                    ref = "master";
                  };
                  perSystem = psArgs: {
                    flake.commands = psArgs.config.input-branches.commands.all;
                  };
                }
              '';

          script = pkgs.writeShellApplication {
            name = "script";
            text = ''
              set -o xtrace

              log=$(nix eval '.#commands.${system}' --print-build-logs 2>&1 || true)
              substring="error: attribute 'bogus' missing"
              if [[ "$log" != *"$substring"* ]]; then
                echo "Substring \`$substring\` not found in log:"
                echo
                echo '```'
                echo "$log"
                echo '```'
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
