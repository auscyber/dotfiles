(module plugins.telescope
  {require {utils utils
            telescope telescope
            actions telescope.actions}})

(telescope.setup
  {:defaults {:mappings {:i {:<esc> actions.close}}}})


(utils.keymap :n :<C-f> ":Telescope find_files<CR>")
(utils.keymap :n :<C-b> "<cmd> lua require'telescope.builtin'.buffers {}<CR>")
