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
        name = "lsp-file-operations";
        package = "nvim-lsp-file-operations";
        moduleName = "lsp-file-operations";
        maintainers = [ lib.maintainers.auscyber ];
      };
    includes = [ den.aspects.packages.eagle-nvim ];
  };
}
