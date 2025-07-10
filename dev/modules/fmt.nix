{ inputs, ... }:
{
  imports = [ inputs.treefmt-nix.flakeModule ];
  perSystem = {
    treefmt = {
      projectRoot = ../..;
      programs = {
        nixfmt.enable = true;
        nixf-diagnose.enable = true;
        prettier.enable = true;
      };
      settings.on-unmatched = "fatal";
    };

    pre-commit.settings.hooks.treefmt.enable = true;
  };
}
