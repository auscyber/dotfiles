{
  den.aspects.nixvim.nvim =

    { lib, ... }:

    lib.nixvim.plugins.mkNeovimPlugin {
      name = "eagle";
      package = "eagle-nvim";
      moduleName = "eagle";
      maintainers = [ lib.maintainers.auscyber ];
    };
}
