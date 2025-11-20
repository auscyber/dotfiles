{ inputs, ... }:
{
  imports = [ inputs.treefmt-nix.flakeModule ];
  perSystem =
    { pkgs, lib, ... }:
    {
      treefmt = {
        projectRootFile = ".git/config";
        programs.nixfmt.enable = true;
        programs.shellcheck.enable = true;
      };

    };
}
