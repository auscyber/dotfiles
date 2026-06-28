{ den, ... }:
{
  den.aspects.nixvim = {
    nvim =
      { lib, ... }:
      lib.nixvim.plugins.mkNeovimPlugin {
        name = "lsp-file-operations";
        package = "nvim-lsp-file-operations";
        moduleName = "lsp-file-operations";
        maintainers = [ lib.maintainers.auscyber ];
      };
    includes = [ den.aspects.packages.eagle-nvim ];
  };

}
