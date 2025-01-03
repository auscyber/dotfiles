(module plugins.bufferline
  {require {bufferline bufferline
            nvim aniseed.nvim}
   require-macros [macros]})

(bufferline.setup {
                   :options {
                             :diagnostics  "nvim_lsp"}
                   :highlights {
                                :indicator_selected {
                                                     :fg :#8BB2C1}}})
(nvim.set_keymap :n :gb :BufferLinePick<CR> {:noremap true :silent true})
