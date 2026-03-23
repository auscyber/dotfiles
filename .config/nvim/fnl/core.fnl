(module core {require {_ plugins}
              autoload {nvim aniseed.nvim a aniseed.core fennel aniseed.fennel}
              require-macros [macros zest.macros]})

; (let [(res err) (pcall #(require :packer_compiled))] #(do nil))
;  (if err))
;    (vim.notify (string.format "Failure loading packer_compiled %s" (tostring err)) vim.log.levels.ERROR)))
(opt-set mouse :a)
(opt-set termguicolors true)
(if (> (vim.fn.has :win32) 0)
    (opt-set guifont "FiraCode Nerd Font:h13")
    (opt-set guifont "FiraCode Nerd Font:h10"))

(nvim.command "colorscheme pink_ocean")
;(_: colorscheme :pink_ocean)
(opt-set showmode false)
(opt-set tabstop 4)
(opt-set shiftwidth 4)
(opt-set hidden true)
(opt-set updatetime 300)
(opt-set signcolumn :yes)
(opt-set cursorline true)
(opt-set cursorlineopt :number)
(opt-set updatetime 400)
(opt-set conceallevel 3)
(opt-local-set rnu true)
(opt-local-set nu true)
(opt-set mapleader "\\")

(def-keymap :<F11> [nvi]
  "<cmd>let g:neovide_fullscreen=!g:neovide_fullscreen<cr>")

;{:switch_fullscreen switch_fullscreen}
;(_: autocmd  "BufNewFile,BufRead" "*.agda" "setf agda")
