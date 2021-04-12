call plug#begin(stdpath('data') . '/plugged')

" Utilities
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'
Plug 'hugolgst/vimsence'
Plug 'jiangmiao/auto-pairs'

" Aesthetics
Plug 'camspiers/animate.vim'
Plug 'ntpeters/vim-better-whitespace'
Plug 'kyazdani42/nvim-web-devicons' " Recommended (for coloured icons)
" Plug 'ryanoasis/vim-devicons' Icons without colours
Plug 'akinsho/nvim-bufferline.lua'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'Yggdroot/indentLine'
Plug 'sonph/onehalf', { 'rtp': 'vim' }
Plug 'jacoborus/tender.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_theme='tender'

" Languages
Plug 'udalov/kotlin-vim'
Plug 'derekelkins/agda-vim'
Plug 'dag/vim-fish'
Plug 'purescript-contrib/purescript-vim'
Plug 'neoclide/coc.nvim'

call plug#end()
colorscheme tender
