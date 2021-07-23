(module plugins.telescope
  {require {utils utils
            telescope telescope
            actions telescope.actions}})


; (telescope.load_extension "frecency")
(telescope.setup
  {:defaults {:mappings {:i {:<esc> actions.close}}}})


(utils.keymap :n :<C-f> ":Telescope frecency<CR>")
(utils.keymap :n :<C-b> "<cmd> lua require'telescope.builtin'.buffers {}<CR>")
