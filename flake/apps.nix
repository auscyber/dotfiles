{
  inputs,
  ...
}:

{

  perSystem =
    { system, pkgs, ... }:
    {
      apps.fetch = {
        type = "app";
        program = pkgs.writeShellApplication {
          name = "fetch";
          runtimeInputs = [
            inputs.nvfetcher.packages."${system}".default
          ];
          text = ''
            set -e
            nvfetcher -k "$HOME/.config/nvchecker.toml"

'';
        };

      };
    };
}
