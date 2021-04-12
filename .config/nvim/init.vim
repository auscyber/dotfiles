set rnu nu
set hidden
syntax on
filetype on
filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab
set termguicolors
so ~/.config/nvim/theme.vim
lua require('plugins')
" so ~/.config/nvim/plugins.vim
so ~/.config/nvim/coc.vim
so ~/.config/nvim/animate.vim
autocmd BufWritePost plugins.lua PackerCompile
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif
