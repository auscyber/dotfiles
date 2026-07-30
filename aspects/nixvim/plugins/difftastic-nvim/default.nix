{ inputs, ... }:
let
  nixvimLib = inputs.nixvim.lib.nixvim;
in
{
  den.aspects.nixvim.nvim =
    {
      lib,
      pkgs,
      ...
    }:
    nixvimLib.plugins.mkNeovimPlugin {
      name = "difftastic";
      package = "difftastic-nvim";
      moduleName = "difftastic-nvim";
      extraPackages = [
        pkgs.difftastic
        pkgs.jujutsu
      ];

      maintainers = [ lib.maintainers.auscyber ];
    };
}
