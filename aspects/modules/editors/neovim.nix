{
  den,
  lib,
  inputs,
  ...
}:
{
  ff.neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  ff.neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
  ff.pandoc = {
    url = "github:srid/pandoc?ref=haskell-flake-revamp";
    #  inputs.nixpkgs.follows = "nixpkgs";
  };
  den.aspects.neovim = {
    includes = [
      den.aspects.nixvim
      den.aspects.stylix
    ];
    overlays.neovim = inputs.neovim-nightly-overlay.overlays.default;
    #    overlays.pandoc = self: super: {
    #      pandoc = inputs.pandoc.packages.${super.system}.pandoc-cli.overrideAttrs {
    #        configureFlags = [ "--flags=lua" ];
    #      };
    #      pandoc-cli = self.pandoc;
    #    };

    nvim.enableMan = false;
    provides.to-users.gui.homeManager = { pkgs, ... }: { home.packages = with pkgs; [ neovide ]; };
    provides.to-users.homeManager =
      { pkgs, ... }:
      {

        stylix.targets.nixvim.enable = false;
        programs.nixvim = {
          enable = true;
          #          package = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
        };
        home.packages = with pkgs; [ tree-sitter ];
        home.sessionVariables.EDITOR = "vim";

        # Disable stylix nixvim target if stylix is enabled
      };
  };
}
