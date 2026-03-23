(module plugins.tree {require {nvim aniseed.nvim a aniseed.core tree nvim-tree}
                      require-macros [macros]})

;(set vim.g.nvim_tree_auto_open 1)
(tree.setup {:hijack_netrw true :update_cwd true :diagnostics {:enable true}})

(nvim.set_keymap :n :<C-n> :<Cmd>NvimTreeToggle<CR>
                 {:noremap true :silent true})
