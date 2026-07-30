{
  den,
  inputs,
  ...
}:
let
  nixvimLib = inputs.nixvim.lib.nixvim;
in
{
  den.aspects.nixvim = {
    nvim =
      { lib, ... }:
      nixvimLib.plugins.mkNeovimPlugin {
        name = "eagle";
        package = "eagle-nvim";
        moduleName = "eagle";
        maintainers = [ lib.maintainers.auscyber ];
      };
    includes = [ den.aspects.packages.eagle-nvim ];
  };
}
