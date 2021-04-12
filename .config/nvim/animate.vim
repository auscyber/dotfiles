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

"let g:fzf_layout = {
" \ 'window': 'new | wincmd J | resize 1 | call animate#window_percent_height(0.5)'
" \}

nnoremap <silent> <Up>    :call animate#window_delta_height(10)<CR>
nnoremap <silent> <Down>  :call animate#window_delta_height(-10)<CR>
nnoremap <silent> <Left>  :call animate#window_delta_width(10)<CR>
nnoremap <silent> <Right> :call animate#window_delta_width(-10)<CR>
nnoremap <C-p> :Files<CR>

nnoremap <C-b>n :BufferLineCycleNext<CR>
nnoremap <C-b>p :BufferLineCyclePrev<CR>
nnoremap <C-b>c :bdelete<CR>

lua << EOF
require'bufferline'.setup{
    options = {
        separator_style = {'', ''}
        }
}
EOF


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

augroup filetypedetect
    au BufRead,BufNewFile *.pl set filetype=prolog
augroup END


" colorscheme industry
" colorscheme onehalfdark
set mouse=a
set noshowmode
