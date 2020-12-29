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
        vim-airline-themes
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
	\	}}}


	  ''; }

	  ];
        extraConfig = ''
          set rnu nu
          set hidden
	  
	  so ${cocSettings}




          '';


  }
