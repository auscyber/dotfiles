(module plugins.animate {require {nvim aniseed.nvim utils utils}
                         require-macros [macros]})

(fn animate_fn [sub_f ...]
  ((. vim.fn (.. "animate#" sub_f)) ...))

(fn window_percent_width [...] (animate_fn :window_percent_width ...))

;(utils.keymap
; :n "<C-w>v"
; "lua require'plugins.animate'.animated_split()" {:noremap true})

{:animated_split (fn animated_split []
                   (nvim.command "new | wincmd L | vertical resize 0 ")
                   (window_percent_width 0.5))}
