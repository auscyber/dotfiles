(module core
  {require {_ plugins
            _ packer_compiled
            }
   autoload {nvim aniseed.nvim
             a aniseed.core
             fennel aniseed.fennel}

   require-macros [macros]})
(local bo nvim.bo)
(local wo nvim.o)
(local o nvim.o)
(set o.mouse "a")
(set vim.o.termguicolors true)
(if (> (vim.fn.has "win32") 0)
  (do
    (set o.guifont "FiraCode Nerd Font:h13"))
  (set o.guifont "FiraCode Nerd Font:h10"))
(nvim.command "colorscheme pink_ocean")
;(_: colorscheme :pink_ocean)
(set o.showmode false)
(set bo.tabstop 4)
(set bo.shiftwidth 4)
(set bo.expandtab  true)
(set o.tabstop 4)
(set o.shiftwidth 4)
(set o.hidden  true)
(set o.updatetime 300)
(set o.signcolumn "yes")
(set wo.rnu true)
(set wo.nu  true)


(fn _G.switch_fullscreen []
      (local nvim (require :aniseed.nvim))
      (if vim.g.neovide_fullscreen
;      (set nvim.g.neovide_fullscreen false)
;      (set nvim.g.neovide_fullscreen true))
        (nvim.command "let g:neovide_fullscreen=v:false")
       (nvim.command "let g:neovide_fullscreen=v:true")))

;(_: nmap :<F11> "<Expr>v:lua.switch_fullscreen() <Cr>")
;{:switch_fullscreen switch_fullscreen}
;(_: autocmd  "BufNewFile,BufRead" "*.agda" "setf agda")
