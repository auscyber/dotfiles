{pkgs, lib, ...}:
let cocSettings = ./coc.vim;
    init = ./init.vim;
    animate = ./animate.vim;
    animate-vim = pkgs.vimUtils.buildVimPlugin {
    name = "animate.vim";
    src = pkgs.fetchFromGitHub {
      owner = "camspiers";
      repo = "animate.vim";
      rev = "ca124da441b4d4ea721f33a999d4493e0d0a7a31";
      sha256 = "Nc1DkNGgSeHWKcS9G7u0r4HVGs9pPR0nUECSt0OlYFs=";
    };
  };
    in
{
    programs.neovim = {
    package = pkgs.neovim-nightly;
    enable = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [ 
      { plugin = vim-airline;
      config = ''
        let g:airline_powerline_fonts = 1
        let g:airline_theme='fruit_punch'
        '';}
        kotlin-vim animate-vim nerdtree vim-better-whitespace agda-vim vim-addon-nix vim-indent-guides vim-airline-themes vim-fish
        purescript-vim vimsence fzf-vim indentLine
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
    \       },
    \ "rust": {
    \  "command": "rust-analyzer",
    \  "filetypes": ["rust"],
    \ "rootPatterns": ["Cargo.toml"]
    \  },
    \   "ccls": {
    \   "command": "ccls",
    \   "filetypes": ["c", "cc", "cpp", "c++", "objc", "objcpp"],
    \   "rootPatterns": [".ccls", "compile_commands.json", ".git/", ".hg/"],
    \   "initializationOptions": {
    \       "cache": {
    \         "directory": "/tmp/ccls"
    \       }
    \     }
    \   },
    \ "lua": {
    \  "command": "lua-lsp",
    \ "filetypes": ["lua"]
    \ },
    \ "ocaml": {
    \ "command": "opam",
    \ "args": ["config", "exec", "--", "ocaml-language-server", "--stdio"],
    \ "filetypes": ["ocaml", "reason"]
    \ }
    \  }}


	  ''; }

	  ];
        extraConfig = ''
          set rnu nu
          set hidden
          syntax on
          filetype on
          filetype plugin indent on
          set tabstop=4
          set shiftwidth=4
          set expandtab
         " so ${init}
	      so ${cocSettings}
          so ~/.config/nixpkgs/animate.vim
          '';


  };}
