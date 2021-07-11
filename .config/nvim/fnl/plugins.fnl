(module plugins
  {require {nvim aniseed.nvim
            a aniseed.core}
   require-macros [macros]})

;(defn- use [...]
;  "Iterates through the arguments as pairs and calls packer's use function for
;  each of them. Works around Fennel not liking mixed associative and sequential
;  tables as well." (let [pkgs [...]]
;    (packer.startup
;      (fn [use]
;        (for [i 1 (a.count pkgs) 2]
;          (packer-use pkgs i
;            (use (a.assoc opts 1 name))))))))


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
  :Olical/conjure {}
  :ntpeters/vim-better-whitespace {}
  :kyazdani42/nvim-web-devicons {}
;  :yamatsum/nvim-nonicons {}
  :ryanoasis/vim-devicons {}
  :kyazdani42/nvim-tree.lua {:mod :tree}
  :akinsho/nvim-bufferline.lua {:mod :bufferline}
  :shougo/deoplete.nvim {:post_ (fn [] (vim.api.nvim_command "let g:deoplete#enable_at_startup = 1"))}
  :nathanaelkane/vim-indent-guides {}
  :Yggdroot/indentLine {}
  :jacoborus/tender.vim {}
;  :vim-airline/vim-airline
;    {:requires ["vim-airline/vim-airline-themes" "jacoborus/tender.vim"]
;          :mod :airline-theme}
  :christoomey/vim-tmux-navigator {}
  :nvim-telescope/telescope.nvim
    {
     :requires [["nvim-lua/popup.nvim"] ["nvim-lua/plenary.nvim"]]
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

  :norcalli/snippets.nvim {}
  :neovim/nvim-lspconfig { :mod :nvim_lsp 
                          :ft [:haskell :rust :typescript :javascript :lua :zig :go :c :cpp :typescriptreact :scala :nix]
                          :requires [:nvim-lua/completion-nvim :nvim-lua/lsp_extensions.nvim :scalameta/nvim-metals]
                          }
  :kosayoda/nvim-lightbulb {
                            :config
                              (fn []
                                (let [nvim (require :aniseed.nvim)]
                                  (autocmd "CursorHold,CursorHoldI" "*" "lua require'nvim-lightbulb'.update_lightbulb()")))}

  :rust-lang/rust.vim {:ft [:rust]}
  :udalov/kotlin-vim {:ft [:kotlin]}
;  :derekelkins/agda-vim {:ft [:agda]} ; :config (fn [] (vim.api.nvim_command "let maplocalleader = \",\""))}
  :dag/vim-fish {}
  :purescript-contrib/purescript-vim {:ft [:ft [:purescript]]}
;  :eraserhd/parinfer-rust {:ft [:fennel] :run "nix-shell --run \"cargo build --release\""}
  :nvim-treesitter/nvim-treesitter {:do "TSUpdate" :mod :treesitter :requires [:nvim-treesitter/playground]}
;  :camspiers/animate.vim {:mod :animate}
  :elkowar/nvim-gehzu {}
;  :glepnir/galaxyline.nvim {:mod :galaxyline}
  :tpope/vim-fugitive {}
  :famiu/feline.nvim {:mod :feline}
  :andweeb/presence.nvim {:mod :presence}
  :LnL7/vim-nix  {}
  :ShinKage/nvim-idris2 {})


