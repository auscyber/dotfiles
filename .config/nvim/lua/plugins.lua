local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
  execute 'packadd packer.nvim'
  vim.api.nvim_command [[PackerSync]]
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
    -- Packer can manage itself
    use {'wbthomason/packer.nvim',
    config = function ()
	vim.api.nvim_command [[ autocmd BufWrite plugins.lua PackerCompile]]
	end
	}
    use 'junegunn/fzf'
    use 'junegunn/fzf.vim'
    use 'preservim/nerdtree'
    use 'hugolgst/vimsence'
    use 'jiangmiao/auto-pairs'


    -- Aesthetics
    use {'camspiers/animate.vim' 
--    , 
--    	config = function ()
--	    nerd_tree = {}
--        nerd_tree.open = false
--        function nerd_tree.start ()
--           if nerd_tree.open then
--               vim.api.nvim_command [[
--                call animate#window_percent_width(1)
--                NERDTreeClose
--                ]]
--               nerd_tree.open = false
--            else
--                vim.api.nvim_command [[
--                NERDTreeFocus
--                wincmd H | vertical resize 0
--                call animate#window_percent_width(0.2)
--                ]]
--		nerd_tree.open =true
--            end
--        end
--	end
	}
    use 'ntpeters/vim-better-whitespace'
    use 'kyazdani42/nvim-web-devicons'
    use 'ryanoasis/vim-devicons'
    use {'akinsho/nvim-bufferline.lua', config = function()
	    	require 'bufferline'.setup { options = { separator_style = {'',''} }}
	end}
--    use {'shougo/deoplete.nvim', post_ = function () vim.api.nvim_command [[let g:deoplete#enable_at_startup = 1]] end}
    use 'nathanaelkane/vim-indent-guides'
    use 'Yggdroot/indentLine'
    use 'jacoborus/tender.vim'
    use  {'vim-airline/vim-airline',
        requires = {'vim-airline/vim-airline-themes','jacoborus/tender.vim'},
        config = function() vim.g.airline_theme = 'tender'end 
	}
    use 'christoomey/vim-tmux-navigator'
    use {
  	'nvim-telescope/telescope.nvim',
  	    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
	    config = function () require'telescope'.setup{} end
	}

-- Languages

    use 'ziglang/zig.vim'
--    use 'rafcamlet/coc-nvim-lua'
--    use {'neoclide/coc.nvim', config = function () vim.api.nvim_command [[ so ~/.config/nvim/coc.vim ]] end, ft={"rust"}, requires = {"antoinemadec/coc-fzf"}}
    use (require 'nvim_lsp')
    use 'rust-lang/rust.vim'
    use 'udalov/kotlin-vim'
    use 'derekelkins/agda-vim'
    use 'dag/vim-fish'
    use 'purescript-contrib/purescript-vim'

end)
