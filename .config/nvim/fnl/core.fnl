(module core
  {require {nvim aniseed.nvim}
   require-macros [macros]})
(local bo nvim.bo)
(local wo nvim.o)
(local o nvim.o)
(set o.mouse "a")
(set o.termguicolors true)
(set o.guifont "Hasklug Nerd Font:12")
(_: colorscheme :pink_ocean)
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

(nvim.command "set rnu nu")

(fn _G.switch_fullscreen []
      (local nvim (require :aniseed.nvim))
      (if vim.g.neovide_fullscreen 
;      (set nvim.g.neovide_fullscreen false)
;      (set nvim.g.neovide_fullscreen true))
        (nvim.command "let g:neovide_fullscreen=v:false")
       (nvim.command "let g:neovide_fullscreen=v:true")))

(_: nmap :<F11> "<Expr>v:lua.switch_fullscreen() <Cr>")
{:switch_fullscreen switch_fullscreen}
(_: autocmd  "BufNewFile,BufRead" "*.agda" "setf agda")




