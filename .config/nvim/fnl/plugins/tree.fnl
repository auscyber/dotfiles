(module plugins.tree
  {require {nvim aniseed.nvim
            a aniseed.core
            tree nvim-tree
            tree_config nvim-tree.config}
   require-macros [macros]})
;(set vim.g.nvim_tree_auto_open 1)
(tree.setup {
;             :auto_open true
             :update_cwd true
             :diagnostics {
                           :enable true}})
(nvim.set_keymap :n :<C-n> "<Cmd>NvimTreeToggle<CR>" {:noremap true :silent true})
