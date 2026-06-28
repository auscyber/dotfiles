{ den, lib, ... }:
{
  den.aspects.updater = {
    homeManager =
      { pkgs, config, ... }:
      let
        finalPackage = pkgs.writeShellApplication {
          name = "update-config";
          runtimeInputs = [ pkgs.nvfetcher ];
          text = ''
            set -e
            cd "${config.flakeFolder or "$HOME/dotfiles"}"
            nix flake update
            ${lib.optionalString (config.age.templates ? "nvchecker.toml") ''
              nvfetcher -k ${lib.escapeShellArg config.age.templates."nvchecker.toml".path}
            ''}
          '';
        };
      in
      {
        home.packages = [ finalPackage ];
      };
  };
}
