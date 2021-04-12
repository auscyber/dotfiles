hi clear
set background=dark
if exists("syntax_on")
    syntax reset
endif

function! s:h(group, fg, bg, attr)
  exec "hi " . a:group . " guifg=" . a:fg . " guibg=" . a:bg
  if a:attr != ""
    exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
  else
    exec "hi " . a:group . " gui=NONE cterm=NONE"
  endif
endfun

" let g:colors_name="auscyber_theme"
let g:colors_name="pink_ocean"

let s:bg = "#1F1F1F"

hi Normal  ctermfg=255 ctermbg=235 guifg=#eeeeee guibg=#282828
hi CursorLineNr guifg=#ffd1dc  
hi LineNr guifg=#ffd1dc
hi Statement guifg=#8BB2C1
hi Type guifg=#738290 
hi Title guifg=#738290
hi Comment guifg=#707078
hi Identifier guifg=#A04668
hi PreProc guifg=#DB5461
hi Special guifg=#ffd1dc
hi Error guibg=#DB5461
hi Pmenu guifg=#121212 guibg=#FFFFFF
hi NonText guifg=#1F1F1F
