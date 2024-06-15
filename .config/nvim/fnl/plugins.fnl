(module plugins {require {nvim aniseed.nvim
                          a aniseed.core}
                 require-macros [macros zest.macros]})


;; Plugins to be managed by packer.
; TODO FIX STUFF
(packer-use
  ;; libraries
  :nvim-lua/plenary.nvim {:module :plenary}
  :tami5/sqlite.lua {:module :sqlite}
  :tsbohc/zest.nvim {};:config #(. (require :zest) :setup)}
  :MunifTanjim/nui.nvim {:module :nui}

  ; Startup and typical operation
  :Olical/aniseed {:branch :develop}
  :lewis6991/impatient.nvim {:require :sqlite.lua :config #(require "impatient")}
  :wbthomason/packer.nvim {}
  :kyazdani42/nvim-web-devicons {:config #(require :plugins.devicons)}


  ;; gui features
  :ellisonleao/glow.nvim {}
  :rktjmp/lush.nvim {}
  :camspiers/animate.vim {:module :animate}
  :kyazdani42/nvim-tree.lua {:config #(require :plugins.tree)}; :keys :<C-n>} ;:mod tree}
  :famiu/feline.nvim {:config #(require "plugins.feline")} ; :mod feline}
  :akinsho/nvim-bufferline.lua {:config #(require :plugins.bufferline)};
  :nvim-telescope/telescope.nvim
        {:cmd :Telescope
         :keys [:<C-f> :<C-b>]
         :module :telescope
         :requires [:plenary.nvim  {1 :nvim-telescope/telescope-frecency.nvim :requires :sqlite.lua}]
         :config #(require :plugins.telescope)}
  :numToStr/Comment.nvim {:config #(require :plugins.comment)}
  :lewis6991/gitsigns.nvim {:requires [:plenary.nvim] :config #(require :plugins.gitsigns)} ;:mod gitsigns}
  :TimUntersberger/neogit {:cmd :Neogit :requires [:plenary.nvim] :config #(require :plugins.neogit)} ;:mod neogit}
  :norcalli/nvim-colorizer.lua {:config
                                        #((. (require "colorizer") :setup))}
  :rcarriga/nvim-notify {:config #(set vim.notify (require "notify"))}

  :folke/todo-comments.nvim {:requires :plenary.nvim :config #(require "plugins.todo")} ;:mod todo}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
  ;;; quality of life improvements ;;;
  ;;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  :glepnir/dashboard-nvim {:config #(require "plugins.dashboard")}
;  :folke/persistence.nvim {:event :BufReadPre :cmd :Load :module :persistence :config #(let [persistence (require "persistence")]
                                                                                         ;(vim.api.nvim_command "command! -nargs=0 Load :lua require('persistence').load()")
          ;                                                                               (persistence.setup))}
  :christoomey/vim-tmux-navigator {}
  :folke/which-key.nvim {:opt false :config #(require "plugins.whichkey")}
  :windwp/nvim-autopairs {:config #(require "plugins.autopairs")} ; :mod autopairs}
;  :Yggdroot/indentLine {} ;; disable in favour of below plugin
  :lukas-reineke/indent-blankline.nvim
    {:config #(require :plugins.indent)}
  :ntpeters/vim-better-whitespace {:config #(do
                                              (set vim.g.better_whitespace_filetypes_blacklist [:dashboard :diff :git :gitcommit :unite :qf :help :markdown :fugitive])
                                              (set vim.g.strip_whitespace_confirm 0)
                                              (set vim.g.strip_whitespace_on_save 1))}
;  :nathanaelkane/vim-indent-guides {}
  :jghauser/mkdir.nvim {:config  #(require "mkdir")}
  :chipsenkbeil/distant.nvim {:config #(require "plugins.distant")}
  :tweekmonster/startuptime.vim {:cmd :StartupTime}
  :andweeb/presence.nvim {:config #(require :plugins.presence)}; :mod presence}
  :wakatime/vim-wakatime {}
  :tjdevries/train.nvim {:opt true}

  ; Completion
  :github/copilot.vim {};:opt true}
  :saadparwaiz1/cmp_luasnip {:opt true :requires :luasnip :module :cmp_luasnip}
  :L3mON4D3/luasnip {:module :luasnip}
  :uga-rosa/cmp-dictionary {:module :cmp_dictionary}
  :hrsh7th/cmp-buffer {:opt true :module :cmp_buffer}
  :hrsh7th/cmp-path {}
  :hrsh7th/cmp-cmdline {}
  :hrsh7th/cmp-nvim-lua {:ft [:lua :fennel]}
  :hrsh7th/cmp-copilot {}
  :Saecki/crates.nvim {:requires :plenary.nvim :ft [:toml] :config #((. (require :crates) :setup))}
  :hrsh7th/nvim-cmp {:requires [:crates.nvim :cmp-copilot :cmp_luasnip :luasnip :hrsh7th/cmp-buffer] :config #(require :plugins.cmp)}; :module [:plugins.cmp :cmp] :ft [:lua :fennel]


  ;Language support
  :nvim-treesitter/nvim-treesitter
        {:do :TSUpdate
         :requires [:JoosepAlviste/nvim-ts-context-commentstring :nvim-treesitter/nvim-treesitter-refactor :nvim-treesitter/playground ] :config #(require "plugins.treesitter")} ; :mod treesitter}

  :folke/twilight.nvim {:cmd :Twilight :requires :nvim-treesitter}
  :iamcco/markdown-preview.nvim {:ft :markdown :run "cd app && yarn install"}
  :Olical/conjure {:ft [:fennel :racket :clojure]}
  :wlangstroth/vim-racket {:ft :racket}
  :vmchale/dhall-vim {:ft :dhall}
  :ziglang/zig.vim {:ft [:zig]}
  :edwinb/idris2-vim {:ft [:idris2]}
  :shinKage/idris2-nvim {:ft :idris2 :requires :nui.nvim}
  :udalov/kotlin-vim {:ft [:kotlin]}
  :derekelkins/agda-vim {:ft :agda :config #(vim.api.nvim_command "let maplocalleader = \",\"")}
  :LnL7/vim-nix  {:ft :nix}
  :tikhomirov/vim-glsl {:ft :glsl}
  :dag/vim-fish {:ft :fish}
  :purescript-contrib/purescript-vim {:ft :purescript}
;  :eraserhd/parinfer-rust {:ft [:fennel :racket :scheme :lisp] :run "cargo build --release"}
;  :gpanders/nvim-parinfer {:ft [:fennel :racket :scheme :lisp]}
  :elkowar/yuck.vim {:ft :yuck}
  :elkowar/nvim-gehzu {:ft :fnl}
  :vhyrro/neorg {:after [:nvim-cmp :nvim-treesitter]  :ft :norg :require [:plenary.nvim] :config #(require :plugins.neorg)} ;:mod neorg}

   :nvim-lua/lsp-status.nvim {:module :lsp-status}
   :simrat39/symbols-outline.nvim {:opt true}
   :scalameta/nvim-metals {:ft :scala :requires [:plenary.nvim :nvim-lspconfig]}
   :onsails/lspkind-nvim {:module :lspkind}
   :neovim/nvim-lspconfig
    {
      :ft [:haskell :rust :typescript :javascript :lua :zig :go :c :cpp :typescriptreact :scala :nix :purescript :ocaml :idris2 :ps1 :java :python :kotlin :cs]
      :opt true
      :requires [{1 :simrat39/rust-tools.nvim :requires [:plenary.nvim :nvim-lua/popup.nvim :nvim-dap] :module :rust-tools}
                 :nvim-lua/lsp_extensions.nvim
                 :kosayoda/nvim-lightbulb
                 :williamboman/nvim-lsp-installer
                 {1 :filipdutescu/renamer.nvim :branch :master :module :renamer :requires [:plenary.nvim]}
                 :which-key.nvim
                 {1 :ray-x/lsp_signature.nvim :module :lsp_signature}
                 :nvim-cmp
                 :luasnip
                 {1 :hrsh7th/cmp-nvim-lsp :module :cmp_nvim_lsp}
                 :symbols-outline.nvim
                 :idris2-nvim]

         :config #(require "plugins.lsp")}
                              ; :mod nvim_lsp}

;      :karb94/neoscroll.nvim {:config #(. (require "neoscroll") :setup) }
    :mfussenegger/nvim-dap {:ft [:rust] :config #(require :plugins.dap) :requires [{1 :rcarriga/nvim-dap-ui :module :dapui}]})

(when _G.packer_bootstrap
  (vim.api.nvim_command "au! User PackerComplete :q! | lua vim.notify(\"Successfully bootstrapped\")")
  ((. (require "packer") :sync)))

