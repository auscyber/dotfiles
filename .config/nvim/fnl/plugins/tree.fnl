(module plugins.tree
  {require {nvim aniseed.nvim
            a aniseed.core
            tree nvim-tree.config}
   require-macros [macros]})
(set vim.g.nvim_tree_auto_open 1)
(nvim.set_keymap :n :<C-n> "<Cmd>NvimTreeToggle<CR>" {:noremap true :silent true})
