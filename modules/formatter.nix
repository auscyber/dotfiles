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
          command = "${pkgs.bash}/bin/bash";
          options = [
            "-euc"
            ''
              for file in "$@"; do
                ${lib.getExe pkgs.grafana-alloy} fmt --write "$file"
              done
            ''
            "--" # bash swallows the second argument when using -c
          ];
          includes = [ "**/*.alloy" ];
        };
      };

    };
}
