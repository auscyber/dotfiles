{ inputs, ... }:
{
  imports = [ inputs.treefmt-nix.flakeModule ];
  perSystem =
    { pkgs, lib, ... }:
    {
      treefmt = {
        projectRootFile = ".git/config";
        programs.nixfmt.enable = true;
        programs.fnlfmt.enable = true;
        programs.fourmolu.enable = true;
        programs.stylua.enable = true;
        #        programs.shellcheck.enable = true;
        settings.formatter."grafana-alloy" = {
          command = "${pkgs.grafana-alloy}/bin/alloy";
          options = [
            "fmt"
            "$@"

          ];
          includes = [ "*.alloy" ];
        };
      };

    };
}
