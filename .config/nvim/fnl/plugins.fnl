(module plugins
  {require {nvim aniseed.nvim
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
  :Olical/aniseed {}
  :rktjmp/hotpot.nvim {}
;  :lewis6991/impatient.nvim {:require :sqlite.lua :config #(require "impatient")}
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
         :dependencies [:plenary.nvim  :nvim-telescope/telescope-ui-select.nvim {1 :nvim-telescope/telescope-frecency.nvim :dependencies :sqlite.lua}]
         :config #(require :plugins.telescope)}
  :numToStr/Comment.nvim {:config #(require :plugins.comment)}
  :lewis6991/gitsigns.nvim {:dependencies [:plenary.nvim] :config #(require :plugins.gitsigns)} ;:mod gitsigns}
  :TimUntersberger/neogit {:cmd :Neogit :dependencies [:plenary.nvim] :config #(require :plugins.neogit)} ;:mod neogit}
  :norcalli/nvim-colorizer.lua {:config
                                        #((. (require "colorizer") :setup))}
  :rcarriga/nvim-notify {:config #(set vim.notify (require "notify"))}

  :folke/todo-comments.nvim {:dependencies :plenary.nvim :config #(require "plugins.todo")} ;:mod todo}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
  ;;; quality of life improvements ;;;
  ;;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  :glepnir/dashboard-nvim {:config #(require "plugins.dashboard")}
;  :folke/persistence.nvim {:event :BufReadPre :cmd :Load :module :persistence :config #(let [persistence (require "persistence")]
                                                                                         ;(vim.api.nvim_command "command! -nargs=0 Load :lua require('persistence').load()")
          ;                                                                               (persistence.setup))}
  :christoomey/vim-tmux-navigator {}
  :folke/which-key.nvim {:lazy false :config #(require "plugins.whichkey")}
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
  :tjdevries/train.nvim {:lazy true}

  ; Completion
  :github/copilot.vim {};:lazy true}
  :saadparwaiz1/cmp_luasnip {:lazy true :dependencies :luasnip :module :cmp_luasnip}
  :L3mON4D3/luasnip {:module :luasnip}
  :uga-rosa/cmp-dictionary {:module :cmp_dictionary}
  :hrsh7th/cmp-buffer {:lazy true :module :cmp_buffer}
  :hrsh7th/cmp-path {}
  :hrsh7th/cmp-cmdline {}
  :hrsh7th/cmp-nvim-lua {:ft [:lua :fennel]}
  :hrsh7th/cmp-copilot {}
  :Saecki/crates.nvim {:dependencies :plenary.nvim :ft [:toml] :config #((. (require :crates) :setup))}
  :hrsh7th/nvim-cmp {:dependencies [:crates.nvim :cmp-copilot :cmp_luasnip :luasnip :hrsh7th/cmp-buffer] :config #(require :plugins.cmp)}; :module [:plugins.cmp :cmp] :ft [:lua :fennel]


  ;Language support
  :nvim-treesitter/nvim-treesitter
        {:do :TSUpdate
         :dependencies [:JoosepAlviste/nvim-ts-context-commentstring :nvim-treesitter/nvim-treesitter-refactor :nvim-treesitter/playground ] :config #(require "plugins.treesitter")} ; :mod treesitter}

  :folke/twilight.nvim {:cmd :Twilight :dependencies :nvim-treesitter}
  :iamcco/markdown-preview.nvim {:ft :markdown :run "cd app && yarn install"}
;  :plasticboy/vim-markdown {:ft :markdown :config #(vim.cmd "
;                                              let g:vim_markdown_fenced_languages = ['bash=sh','nix=nix']
;                                              ")}
  :Olical/conjure {:ft [:fennel :racket :cl]}
  :wlangstroth/vim-racket {:lazy true}
  :vmchale/dhall-vim {:lazy true}
  :ziglang/zig.vim {:lazy true}
  :edwinb/idris2-vim {:lazy true}
  :shinKage/idris2-nvim {:ft :idris2 :dependencies :nui.nvim}
  :udalov/kotlin-vim {:lazy true}
  :derekelkins/agda-vim {:ft :agda :config #(vim.api.nvim_command "let maplocalleader = \",\"")}
  :LnL7/vim-nix  {:lazy true}
  :tikhomirov/vim-glsl {:ft :glsl}
  :dag/vim-fish {:ft :fish}
  :purescript-contrib/purescript-vim {:ft :purescript}
  :eraserhd/parinfer-rust {:ft [:fennel :racket :scheme :lisp] :build (fn [] (if (> (vim.fn.exists "/nix/") 0) "nix-shell --run 'cargo build --release'" "cargo build --release"))}
;  :gpanders/nvim-parinfer {:ft [:fennel :racket :scheme :lisp]}
  :elkowar/yuck.vim {:ft :yuck}
;  :elkowar/nvim-gehzu {:ft :fnl}
  ; :vhyrro/neorg {:after [:nvim-cmp :nvim-treesitter]  :ft :norg :require [:plenary.nvim] :config #(require :plugins.neorg)} ;:mod neorg}

   :nvim-lua/lsp-status.nvim {:module :lsp-status}
   :simrat39/symbols-outline.nvim {:lazy true}
   :scalameta/nvim-metals {:ft :scala :dependencies [:plenary.nvim :nvim-lspconfig]}
   :onsails/lspkind-nvim {:module :lspkind}
   :jamestthompson3/nvim-remote-containers {}
   :esensar/nvim-dev-container {:config (fn [] ((. (require "devcontainer") :setup) {}))}
   :neovim/nvim-lspconfig
    {
      :ft [:haskell :rust :typescript :javascript :lua :zig :go :c :cpp :typescriptreact :scala :nix :purescript :ocaml :idris2 :ps1 :java :python :kotlin :cs :ada]
      :lazy true
      :dependencies [;FIXME change to use branch
                     {1 :mrcjkb/rustaceanvim :dependencies [:nvim-dap]}
                     :nvim-lua/lsp_extensions.nvim
                     :kosayoda/nvim-lightbulb
                     {1 :williamboman/mason.nvim :module :mason}
                     {1 :williamboman/mason-lspconfig.nvim :module :mason-lspconfig}
                     {1 :filipdutescu/renamer.nvim :branch :master :module :renamer :dependencies [:plenary.nvim]}
                     :which-key.nvim
                     {1 :ray-x/lsp_signature.nvim :module :lsp_signature}
                     :nvim-cmp
                     :jubnzv/virtual-types.nvim
                     :luasnip
                     {1 :hrsh7th/cmp-nvim-lsp :module :cmp_nvim_lsp}
                     :symbols-outline.nvim
                     :idris2-nvim]
         :config #(require "plugins.lsp")}
                              ; :mod nvim_lsp}

;      :karb94/neoscroll.nvim {:config #(. (require "neoscroll") :setup) }
    :mfussenegger/nvim-dap {:ft [:rust :c :cpp] :lazy true :config #(require :plugins.dap) :dependencies [{1 :rcarriga/nvim-dap-ui :module :dapui :dependencies [:nvim-neotest/nvim-nio]}]})


