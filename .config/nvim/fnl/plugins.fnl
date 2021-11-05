(require-macros :macros)
(module plugins
  {require {nvim aniseed.nvim
            a aniseed.core}
   require-macros [macros zest.macros]
   })
;; Plugins to be managed by packer.
; TODO FIX STUFF


(packer-use
      :Olical/aniseed {:branch :develop}
      :lewis6991/impatient.nvim {:config #(require "impatient") }
      :wbthomason/packer.nvim {}
;  :preservim/nerdtree {}
      :windwp/nvim-autopairs {:config #(require "plugins.autopairs")} ; :mod autopairs}
      :ntpeters/vim-better-whitespace {}
      :kyazdani42/nvim-web-devicons {}

      :lewis6991/gitsigns.nvim {:requires [:plenary.nvim] :config #(require :plugins.gitsigns)} ;:mod gitsigns}
      :tweekmonster/startuptime.vim {:cmd :StartupTime}
      :nvim-lua/plenary.nvim {:module :plenary}
;  :y
;      :ryanoasis/vim-devicons {}
      :kyazdani42/nvim-tree.lua {:config #(require :plugins.tree)} ;:mod tree}
      :akinsho/nvim-bufferline.lua {:config #(require :plugins.bufferline) }; :mod bufferline}
;    ogo/deoplete.nvim {:post_ (fn [] (vim.api.nvim_command "let g:deoplete#enable_at_startup = 1"))}
      :nathanaelkane/vim-indent-guides {}
      :Yggdroot/indentLine {}
      :andweeb/presence.nvim {:config #(require :plugins.presence)}; :mod presence}
      :wakatime/vim-wakatime {}
      :tpope/vim-fugitive {}
      :TimUntersberger/neogit {:requires [:plenary.nvim] :config #(require :plugins.neogit)} ;:mod neogit}
      :famiu/feline.nvim {:config #(require "plugins.feline")} ; :mod feline}
      :iamcco/markdown-preview.nvim {:run "cd app && yarn install"}
      :tjdevries/train.nvim {}
      :jghauser/mkdir.nvim {:config  #(require "mkdir")}

;      :jacoborus/tender.vim {}
;  :v
;    iline-themes" "jacoborus/tender.vim"]
      :christoomey/vim-tmux-navigator {}
      :tami5/sqlite.lua {:module :sqlite}
      :nvim-telescope/telescope.nvim
        {
         :requires [["nvim-lua/popup.nvim"] :nvim-telescope/telescope-packer.nvim "plenary.nvim"  {1 :nvim-telescope/telescope-frecency.nvim :requires :tami5/sqlite.lua}]
         :config #(require :plugins.telescope)}
;          :mod :telescope}

      :norcalli/snippets.nvim {:config (fn [] ((. (require "snippets") :use_suggested_mappings)) (set vim.g.completion_enable_snippet "snippets.nvim"))}

     ;Languages
;
      :vhyrro/neorg {:require [[:plenary.nvim] [:hrsh7th/nvim-compe]] :mod neorg}

      :Olical/conjure {:ft [:fennel :racket :clojure]}
      :wlangstroth/vim-racket {:ft :racket}
      :vmchale/dhall-vim {:ft :dhall}
      :ziglang/zig.vim {:ft [:zig]}
      :edwinb/idris2-vim {:ft [:idris2]}
;    2{:ft :idris}
      :rust-lang/rust.vim {:ft :rust}
      :udalov/kotlin-vim {:ft :kotlin}
      :derekelkins/agda-vim {:ft [:agda] :config #(vim.api.nvim_command "let maplocalleader = \",\"")}
      :LnL7/vim-nix  {:ft :nix}
      :tikhomirov/vim-glsl {:ft :glsl}

      :dag/vim-fish {:ft :fish}
      :purescript-contrib/purescript-vim {:ft [:purescript]}
      :ranfdev/parinfer-rust {:ft [:fennel :racket :scheme] :run "cargo build --release"}

;  :r
;  :n
;    [ (vim.api.nvim_command "so ~/.config/nvim/coc.vim"))
;    atoinemadec/coc-fzf"]

      :norcalli/nvim-colorizer.lua {:config
                                                   #((. (require "colorizer") :setup))}
      ; Lsp plugins
      :neovim/nvim-lspconfig {
                              :ft [:haskell :rust :typescript :javascript :lua :zig :go :c :cpp :typescriptreact :scala :nix :purescript :ocaml :idris2 :ps1]
                              :requires [{1 :simrat39/rust-tools.nvim :requires [:plenary.nvim :nvim-lua/popup.nvim :mfussenegger/nvim-dap] :module :rust-tools}
                                         {1 :rafaelsq/completion-nvim :branch :changeHandlerSignature :module :completion}
                                         {1 :nvim-lua/lsp_extensions.nvim :module :lsp_extensions} {1 :scalameta/nvim-metals :requires :plenary.nvim}
                                          {1 :kosayoda/nvim-lightbulb :module :nvim-lightbulb }
                                          {1 :onsails/lspkind-nvim :module :lspkind}
                                                  ]
                                :config #(require "plugins.nvim_lsp")}
                              ; :mod nvim_lsp}

      :rcarriga/nvim-notify {:config #(set vim.notify (require "notify"))}
      :folke/todo-comments.nvim {:requires :plenary.nvim :mod todo}
      :tsbohc/zest.nvim {:config #(let [zest (require :zest)] (zest.setup))}
;    n "BufReadPre"
;    ue "persistence" :config (fn [] ((. (require "persistence") setup)))
      :elkowar/yuck.vim {:ft :yuck}
      :elkowar/nvim-gehzu {:ft :fnl}
      :nvim-treesitter/nvim-treesitter {:do "TSUpdate" :requires [:nvim-treesitter/playground :folke/twilight.nvim] :config #(require "plugins.treesitter")} ; :mod treesitter}
;      :karb94/neoscroll.nvim {:config #(. (require "neoscroll") :setup) }
      :mfussenegger/nvim-dap {:ft [:rust] :config #(require :plugins.dap) :requires [{1 :rcarriga/nvim-dap-ui :module :dapui}]})
;  :camspiers/animate.vim {:mod :animate}
;  :glepnir/galaxyline.nvim {:mod :galaxyline}

