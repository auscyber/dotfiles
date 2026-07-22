{ inputs, ... }: {
  ff.treefmt-nix.url = "github:numtide/treefmt-nix";

  imports = [ inputs.treefmt-nix.flakeModule ];

  perSystem = { pkgs, ... }: {
    treefmt = {
      projectRootFile = "README.md";
      settings = {
        excludes = [ ".workspaces" ];
        includes = [ ".config" ];
      };
      programs.nixfmt.enable = true;
      programs.alejandra.enable = true;
    };
  };
}
