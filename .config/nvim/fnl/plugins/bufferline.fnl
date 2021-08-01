(module plugins.bufferline 
  {require {bufferline bufferline
            nvim aniseed.nvim}
   require-macros [macros]})

(bufferline.setup {
                   :diagnostics  "nvim_lsp"
                   :highlights {
                                :indicator_selected {
                                                     :guifg :#8BB2C1}}})
(nvim.set_keymap :n :gb :BufferLinePick<CR> {:noremap true :silent true})
