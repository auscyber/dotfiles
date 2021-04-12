local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
  execute 'packadd packer.nvim'
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'
    use 'junegunn/fzf.vim'
    use 'preservim/nerdtree'
    use 'hugolgst/vimsence'
    use 'jiangmiao/auto-pairs'

    -- Aesthetics
    use 'camspiers/animate.vim'
    use 'ntpeters/vim-better-whitespace'
    use 'kyazdani42/nvim-web-devicons'
    use 'ryanoasis/vim-devicons'
    use 'akinsho/nvim-bufferline.lua'
    use 'nathanaelkane/vim-indent-guides'
    use 'Yggdroot/indentLine'
    use 'jacoborus/tender.vim'
    use  {'vim-airline/vim-airline',
        requires = {'vim-airline/vim-airline-themes','jacoborus/tender.vim'},
        config = function() vim.g.airline_theme = 'tender'end }

    use 'rafcamlet/coc-nvim-lua'
--    use (require 'nvim_lsp')    -- Languages
    use 'udalov/kotlin-vim'
    use 'derekelkins/agda-vim'
    use 'dag/vim-fish'
    use 'purescript-contrib/purescript-vim'
    use 'neoclide/coc.nvim'

end)
