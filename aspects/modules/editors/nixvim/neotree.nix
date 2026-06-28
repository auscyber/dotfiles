{

  den.aspects.nixvim.nvim = {
    plugins.neo-tree.enable = true;
    plugins.lsp-file-operations.enable = true;
    keymaps = [
      {
        mode = "n";
        key = "<C-n>";
        action = "<Cmd>Neotree toggle<CR>";
        options = {
          noremap = true;
          silent = true;
        };
      }
    ];
  };

}
