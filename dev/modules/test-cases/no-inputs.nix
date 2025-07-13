{
  perSystem =
    { pkgs, ... }:
    {
      testCases = [
        {
          title = "no-inputs";
          module =
            pkgs.writeText "module.nix"
              # nix
              ''
                {
                  perSystem =
                    psArgs@{ pkgs, ... }:
                    {
                      packages.default =
                        assert psArgs.config.input-branches.commands.all == [ ];
                        pkgs.hello;
                    };
                }
              '';

          script = pkgs.writeShellApplication {
            name = "script";
            text = ''
              set -o xtrace

              nix build
              declare out
              touch "$out"
            '';
          };
        }
      ];
    };
}
