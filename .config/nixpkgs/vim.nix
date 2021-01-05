{pkgs, ...}:
let cocSettings = ./coc.vim;
    in
{
    enable = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [ 
      { plugin = vim-airline;
      config = ''
        let g:airline_powerline_fonts = 1
        let g:airline_theme='fruit_punch'
        '';}
        vim-addon-nix
	vim-indent-guides 
        vim-airline-themes
        purescript-vim
        vimsence
	{ plugin = coc-nvim;
	  config = ''let g:coc_user_config = {'languageserver':{
	\	'haskell': {
	\	  'command': 'haskell-language-server-wrapper',
	\	  'args': ['--lsp'],
	\	  'rootPatterns': ['*.cabal', 'stack.yaml', 'cabal.project', 'package.yaml', 'hie.yaml'],
	\	  'filetypes': ['haskell', 'lhaskell']
	\	},
	\      'nix': {
	\	  'command': 'rnix-lsp',
	\	  'filetypes': [
	\	    'nix'
	\	  ]
	\	},
    \     "purescript": {
    \         "command": "purescript-language-server",
    \         "args": ["--stdio"],
    \         "filetypes": ["purescript"],
    \         "rootPatterns": ["bower.json", "psc-package.json", "spago.dhall"]
    \       }
    \  }}


	  ''; }

	  ];
        extraConfig = ''
          set rnu nu
          set hidden
          syntax on
          filetype on
          filetype plugin indent on
	      so ${cocSettings}




          '';


  }
