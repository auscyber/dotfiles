(module plugins.telescope
  {require {utils utils
            telescope telescope
            actions telescope.actions}})



(telescope.setup
  {:defaults {:mappings {:i {:<esc> actions.close}}}}
 (telescope.load_extension "frecency"))


(utils.keymap :n :<C-f> "<cmd> lua require 'telescope.builtin'.find_files()<CR>")
(utils.keymap :n :<C-b> "<cmd> lua require'telescope.builtin'.buffers {}<CR>")
