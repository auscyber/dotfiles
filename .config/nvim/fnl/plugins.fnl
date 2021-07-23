(module plugins
  {require {nvim aniseed.nvim
            a aniseed.core}
   require-macros [macros]})


;; Plugins to be managed by packer.
(packer-use
  ; "~/repos/Olical/conjure" {:mod :conjure}
  ; "~/repos/Olical/aniseed" {}
  ; "~/repos/Olical/nvim-local-fennel" {}
  :wbthomason/packer.nvim {}
  :junegunn/fzf {}
  :junegunn/fzf.vim {}
;  :preservim/nerdtree {}
  :windwp/nvim-autopairs {:mod :autopairs}
  :Olical/conjure {:ft "fnl"}
  :ntpeters/vim-better-whitespace {}
  :kyazdani42/nvim-web-devicons {}
;  :yamatsum/nvim-nonicons {}
  :ryanoasis/vim-devicons {}
  :kyazdani42/nvim-tree.lua {:mod :tree}
  :akinsho/nvim-bufferline.lua {:mod :bufferline}
  :shougo/deoplete.nvim {:post_ (fn [] (vim.api.nvim_command "let g:deoplete#enable_at_startup = 1"))}
  :nathanaelkane/vim-indent-guides {}
  :Yggdroot/indentLine {}
;  :jacoborus/tender.vim {}
;  :vim-airline/vim-airline
;    {:requires ["vim-airline/vim-airline-themes" "jacoborus/tender.vim"]
;          :mod :airline-theme}
  :christoomey/vim-tmux-navigator {}
  :nvim-telescope/telescope.nvim
    {
     :requires [["nvim-lua/popup.nvim"] ["nvim-lua/plenary.nvim"]  {1 :nvim-telescope/telescope-frecency.nvim
                                                                     :requires {1 :tami5/sql.nvim :mod :sqlite}}]

;     :cmd "Telescope frecency"
     :mod :telescope}



;
; Languages
;
  :onsails/lspkind-nvim {}
  :Olical/aniseed {}
  :ziglang/zig.vim {:ft [:zig]}
;  :rafcamlet/coc-nvim-lua {}
;  :neoclide/coc.nvim {
;                      :config (fn [] (vim.api.nvim_command "so ~/.config/nvim/coc.vim"))
;                      :ft [:rust]
;                      :requires ["antoinemadec/coc-fzf"]

  :norcalli/snippets.nvim {:config (fn [] (set vim.g.completion_enable_snippet "snippets.nvim"))}
  :norcalli/nvim-colorizer.lua {:config (fn [] (set vim.o.termguicolors true)
                                               ((. (require "colorizer") :setup)))}
  :neovim/nvim-lspconfig { :mod :nvim_lsp
                          :ft [:haskell :rust :typescript :javascript :lua :zig :go :c :cpp :typescriptreact :scala :nix]
                          :requires [:nvim-lua/completion-nvim :nvim-lua/lsp_extensions.nvim :scalameta/nvim-metals]}

  :kosayoda/nvim-lightbulb {
                            :config
                              (fn []
                                (let [nvim (require :aniseed.nvim)]
                                  (autocmd "CursorHold,CursorHoldI" "*" "lua require'nvim-lightbulb'.update_lightbulb()")))}

  :rust-lang/rust.vim {:ft [:rust]}
  :udalov/kotlin-vim {:ft [:kotlin]}
;  :derekelkins/agda-vim {:ft [:agda]} ; :config (fn [] (vim.api.nvim_command "let maplocalleader = \",\""))}
  :dag/vim-fish {:ft :fish}
  :purescript-contrib/purescript-vim {:ft [:ft [:purescript]]}
  :ranfdev/parinfer-rust {:ft [:fennel] :run "nix-shell --run \"cargo build --release\""}
  :nvim-treesitter/nvim-treesitter {:do "TSUpdate" :mod :treesitter :requires [:nvim-treesitter/playground :folke/twilight.nvim]}
;  :camspiers/animate.vim {:mod :animate}
  :elkowar/nvim-gehzu {:ft :fnl}
;  :glepnir/galaxyline.nvim {:mod :galaxyline}
  :tpope/vim-fugitive {}
  :famiu/feline.nvim {:mod :feline}
  :andweeb/presence.nvim {:mod :presence}
  :LnL7/vim-nix  {:ft :nix}
  :tikhomirov/vim-glsl {:ft :glsl}
  :tweekmonster/startuptime.vim {:cmd :StartupTime}
  :ShinKage/nvim-idris2 {:ft :idris})


