{
  den,
  lib,
  inputs,
  ...
}:
{
  den.aspects.neovim = {
    includes = [
      den.aspects.nixvim
      den.aspects.stylix
    ];

    homeManager =
      { pkgs, options, ... }:
      {

        stylix.targets.nixvim.enable = false;
        programs.nixvim = {
          enable = true;
          nixpkgs.useGlobalPackages = true;
        };
        home.packages = with pkgs; [ tree-sitter ];
        home.sessionVariables.EDITOR = "vim";

        # Disable stylix nixvim target if stylix is enabled
      };
  };
}
