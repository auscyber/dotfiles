function NerdTree() abort
   :NERDTreeToggle
   wincmd H | vertical resize 0
   call animate#window_percent_width(0.20) 
endfunction
nmap <M-t> :call NerdTree()<CR>

function AnimatedSplit() abort
    new | wincmd L | vertical resize 0
    call animate#window_percent_width(0.50)
endfunction
nmap <C-w>v :call AnimatedSplit()<CR>

let g:fzf_layout = {
 \ 'window': 'new | wincmd J | resize 1 | call animate#window_percent_height(0.5)'
 \}

nnoremap <silent> <Up>    :call animate#window_delta_height(10)<CR>
nnoremap <silent> <Down>  :call animate#window_delta_height(-10)<CR>
nnoremap <silent> <Left>  :call animate#window_delta_width(10)<CR>
nnoremap <silent> <Right> :call animate#window_delta_width(-10)<CR>

let g:currentmode={
       \ 'n'  : 'NORMAL ',
       \ 'v'  : 'VISUAL ',
       \ 'V'  : 'V·Line ',
       \ '' : 'V·Block ',
       \ 'i'  : 'INSERT ',
       \ 'R'  : 'R ',
       \ 'Rv' : 'V·Replace ',
       \ 'c'  : 'Command ',
       \ 't'  : 'Terminal ',
       \ }

" colorscheme industry
set mouse=a
set noshowmode
hi CursorLineNr ctermbg=242 ctermfg=210
hi CursorLine ctermbg=238 ctermfg=210
"hi Statusline ctermfg=224 ctermbg=253
"hi User1 ctermbg=210 ctermfg=231
"hi LineNr ctermfg=210
"set statusline+=%#PmenuSel#
"set statusline+=%{&modified?'[+]':''}
"set statusline+=%#LineNr#
"set statusline+=\ %f
"set statusline+=%m\ 
"set statusline+=%=
"set statusline+=%#CursorColumn#
"set statusline+=\ %y
"set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
"set statusline+=\ [%{&fileformat}\]
"set statusline+=\ %p%%
"set statusline+=\ %l:%c
"set statusline+=\ 
