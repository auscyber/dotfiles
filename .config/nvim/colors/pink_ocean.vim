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
let &t_ZH="\e[3m"
let &t_ZR="\e[23m"

let s:bg = "#1F1F1F"

hi Search guibg=#ffd1dc
hi BufferLineTabSelected guifg=#ffd1dc
hi Constant ctermfg=217 guifg=#ffa0a0
"if exists("g:neovide_refresh_rate")
    hi Normal guibg=#1f1f1f
"endi
hi Normal ctermfg=251 ctermbg=235 guifg=#eeeeee
hi Folded guifg=#A04668 guibg=#282828
" hi CursorLine ctermfg=224 guifg=#ffd1dc term=bold gui=bold
hi CursorLineNr gui=bold term=bold guifg=#ffd1dc
hi LineNr guifg=#ffd1dc ctermfg=224
hi Statement guifg=#8BB2C1 ctermfg=109 gui=bold,italic
hi Identifier guifg=#738290 ctermfg=246
hi Title guifg=#738290 ctermfg=246
hi Comment guifg=#707078 ctermfg=60
hi Type guifg=#A04668 ctermfg=131
hi PreProc guifg=#DB5461 ctermfg=167
hi Special guifg=#ffd1dc ctermfg=224
hi Error guibg=#DB5461 ctermfg=167
hi Pmenu  guifg=#FfFfFf guibg=#3f3f3f ctermfg=16 ctermbg=231
hi PmenuSel guifg=#f0f0f0 guibg=#5A5A5A
"hi PmenuThumb guifg=#3f3f3f
hi VertSplit guifg=#1f1f1f guibg=#E0E0E0
hi NonText guifg=#4a4a4a ctermfg=16
hi DiffAdd guibg=#8BB2C1
hi link DiffChange Folded
hi Directory guifg=#8BB2C1
hi link DiffDelete Statement
hi SignColumn guibg=#1f1f1f
hi DiffDelete guibg=#DB5461 guifg=#eeeeee
hi DiffChange guibg=#a04668
hi DiffAdd guibg=#2dde92 guifg=#1f1f1f
hi IndentBlanklineChar guifg=#303038
hi GitSignsDiffAdd guifg=#2dde92
hi GitSignsDiffChange guifg=#ffa0a0
hi GitSignsDiffDelete guifg=#DB5461
hi CmpItemMenu guifg=#707078
" gray
hi! CmpItemAbbrDeprecated guibg=NONE gui=strikethrough guifg=#808080
" blue
hi! CmpItemAbbrMatch guibg=NONE guifg=#738290
hi! CmpItemAbbrMatchFuzzy guibg=NONE guifg=#738290
" light blue
hi! CmpItemKindVariable guibg=NONE guifg=#9CDCFE
hi! CmpItemKindInterface guibg=NONE guifg=#9CDCFE
hi! CmpItemKindText guibg=NONE guifg=#8bb2c1
" pink
hi! CmpItemKindFunction guibg=NONE guifg=#a04668
hi! CmpItemKindMethod guibg=NONE guifg=#A04668
" front
hi! CmpItemKindKeyword guibg=NONE guifg=#D4D4D4
hi! CmpItemKindProperty guibg=NONE guifg=#D4D4D4
hi! CmpItemKindUnit guibg=NONE guifg=#D4D4D4
hi! LspSignatureActiveParameter guifg=#8bb2c1

