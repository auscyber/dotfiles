{ den, ... }:
{
  den.aspects.neovim = {
    includes = [ den.aspects.nixvim ];

    homeManager =
      {
        options,
        lib,
        pkgs,
        inputs,
        ...
      }:
      {
        programs.nixvim = {
          enable = true;
          nixpkgs.useGlobalPackages = true;
        };
        home.packages = with pkgs; [ tree-sitter ];
        home.sessionVariables.EDITOR = "vim";
      }
      // lib.optionalAttrs (lib.hasAttrByPath [ "stylix" "targets" "nixvim" "enable" ] options) {
        stylix.targets.nixvim.enable = false;
      };
  };
}
