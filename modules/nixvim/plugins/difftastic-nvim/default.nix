{ lib, pkgs, ... }:
lib.nixvim.plugins.mkNeovimPlugin {
  name = "difftastic";
  package = "difftastic-nvim";
  moduleName = "difftastic-nvim";
  extraPackages = [
    pkgs.difftastic
    pkgs.jujutsu
  ];

  maintainers = [ lib.maintainers.auscyber ];
}
