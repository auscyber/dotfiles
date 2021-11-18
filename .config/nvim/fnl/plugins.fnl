(module plugins {require {nvim aniseed.nvim
            a aniseed.core}
   require-macros [macros]
   })
;; Plugins to be managed by packer.
; TODO FIX STUFF


(packer-use
  ; Startup and typical operation
  :Olical/aniseed {:branch :develop}
  :lewis6991/impatient.nvim {:config #(require "impatient") }
  :wbthomason/packer.nvim {}

  ; libraries
  :nvim-lua/plenary.nvim {:module :plenary}
  :tami5/sqlite.lua {:module :sqlite}
  :tsbohc/zest.nvim {:config #(let [zest (require :zest)] (zest.setup))}

  ;; gui features
  :kyazdani42/nvim-tree.lua {:config #(require :plugins.tree)} ;:mod tree}
  :akinsho/nvim-bufferline.lua {:config #(require :plugins.bufferline) }; :mod bufferline}
  :famiu/feline.nvim {:config #(require "plugins.feline")} ; :mod feline}
  :nvim-telescope/telescope.nvim
        {
         :requires [ :nvim-telescope/telescope-packer.nvim "plenary.nvim"  {1 :nvim-telescope/telescope-frecency.nvim :requires :sqlite.lua}]
         :config #(require :plugins.telescope)}
  :lewis6991/gitsigns.nvim {:requires [:plenary.nvim] :config #(require :plugins.gitsigns)} ;:mod gitsigns}
  :kyazdani42/nvim-web-devicons {}
  :TimUntersberger/neogit {:cmd :Neogit :requires [:plenary.nvim] :config #(require :plugins.neogit)} ;:mod neogit}
  :norcalli/nvim-colorizer.lua {:config
                                        #((. (require "colorizer") :setup))}
  :rcarriga/nvim-notify {:config #(set vim.notify (require "notify"))}

  :folke/todo-comments.nvim {:requires :plenary.nvim :config #(require "plugins.todo")} ;:mod todo}
 ;  :tpope/vim-fugitive {}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
  ;;; quality of life improvements ;;;
  ;;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  :christoomey/vim-tmux-navigator {}
  :folke/which-key.nvim {:config #(require "plugins.whichkey")}
  :windwp/nvim-autopairs {:config #(require "plugins.autopairs")} ; :mod autopairs}
  :Yggdroot/indentLine {}
  :ntpeters/vim-better-whitespace {}
  :nathanaelkane/vim-indent-guides {}
  :jghauser/mkdir.nvim {:config  #(require "mkdir")}

  :tweekmonster/startuptime.vim {:cmd :StartupTime}
  :andweeb/presence.nvim {:config #(require :plugins.presence)}; :mod presence}
      :wakatime/vim-wakatime {}
      :tjdevries/train.nvim {}

;      :jacoborus/tender.vim {}
;  :v
;    iline-themes" "jacoborus/tender.vim"]
;          :mod :telescope}


  ;Language support

  :iamcco/markdown-preview.nvim {:ft :markdown :run "cd app && yarn install"}
  :Olical/conjure {:ft [:fennel :racket :clojure]}
  :wlangstroth/vim-racket {:ft :racket}
  :vmchale/dhall-vim {:ft :dhall}
  :ziglang/zig.vim {:ft [:zig]}
  :edwinb/idris2-vim {:ft [:idris2]}
  :udalov/kotlin-vim {:ft :kotlin}
  :derekelkins/agda-vim {:ft :agda :config #(vim.api.nvim_command "let maplocalleader = \",\"")}
  :LnL7/vim-nix  {:ft :nix}
  :tikhomirov/vim-glsl {:ft :glsl}
  :dag/vim-fish {:ft :fish}
  :purescript-contrib/purescript-vim {:ft :purescript}
  :ranfdev/parinfer-rust {:ft [:fennel :racket :scheme] :run "cargo build --release"}
  :elkowar/yuck.vim {:ft :yuck}
  :elkowar/nvim-gehzu {:ft :fnl}
  :vhyrro/neorg {:ft :org :require [[:plenary.nvim] [:hrsh7th/nvim-compe]] :mod neorg}

    ; Completion
    :saadparwaiz1/cmp_luasnip {:opt true :requires :luasnip :module :cmp_luasnip}
    :L3mON4D3/luasnip {:module :luasnip}
    :hrsh7th/cmp-buffer {:opt true :module :cmp_buffer}
    :hrsh7th/cmp-nvim-lua {:ft [:lua ] :opt true }
    :hrsh7th/nvim-cmp {:module :plugins.cmp :ft [:lua] :requires [:cmp_luasnip :luasnip :cmp-buffer]}
    ; Lsp plugins
    :neovim/nvim-lspconfig {
                              :ft [:haskell :rust :typescript :javascript :lua :zig :go :c :cpp :typescriptreact :scala :nix :purescript :ocaml :idris2 :ps1]
                              :requires [{1 :simrat39/rust-tools.nvim :requires [:plenary.nvim :nvim-lua/popup.nvim :nvim-dap] :module :rust-tools}
;                                         {1 :rafaelsq/completion-nvim :branch :changeHandlerSignature :module :completion}
                                         {1 :nvim-lua/lsp_extensions.nvim :module :lsp_extensions} {1 :scalameta/nvim-metals :ft :scala :requires :plenary.nvim}
                                          {1 :kosayoda/nvim-lightbulb :module :nvim-lightbulb }
                                          {1 :onsails/lspkind-nvim :module :lspkind}
                                          :which-key.nvim
                                          :nvim-cmp
                                          {1 :hrsh7th/cmp-nvim-lsp :module :cmp_nvim_lsp}
                                            ]
                                :config #(require "plugins.nvim_lsp")}
                              ; :mod nvim_lsp}

       :nvim-treesitter/nvim-treesitter {:do "TSUpdate" :requires [:nvim-treesitter/playground :folke/twilight.nvim] :config #(require "plugins.treesitter")} ; :mod treesitter}
;      :karb94/neoscroll.nvim {:config #(. (require "neoscroll") :setup) }
      :mfussenegger/nvim-dap {:ft [:rust] :config #(require :plugins.dap) :requires [{1 :rcarriga/nvim-dap-ui :module :dapui}]}
      )
