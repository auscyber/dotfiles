(module plugin
  {require {nvim aniseed.nvim
            a aniseed.core
            packer packer}
   require-macros [macros]})
(defn safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :plugins. name))]
    (when (not ok?)
      (print (.. "dotfiles error: " val-or-err)))))

(defn- use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup
      (fn [use]
        (for [i 1 (a.count pkgs) 2]
          (let [name (. pkgs i)
                opts (. pkgs (+ i 1))]
            (-?> (. opts :mod) (safe-require-plugin-config))
            (use (a.assoc opts 1 name))))))))


;; Plugins to be managed by packer.
(use
  ; "~/repos/Olical/conjure" {:mod :conjure}
  ; "~/repos/Olical/aniseed" {}
  ; "~/repos/Olical/nvim-local-fennel" {}
  :junegunn/fzf {}
  :junegunn/fzf.vim {}
  :preservim/nerdtree {}
  :windwp/nvim-autopairs {:mod :autopairs}
  :Olical/conjure {}
  :ntpeters/vim-better-whitespace {}
  :kyazdani42/nvim-web-devicons {}
;  :yamatsum/nvim-nonicons {}
  :ryanoasis/vim-devicons {}
  :kyazdani42/nvim-tree.lua {}
  :akinsho/nvim-bufferline.lua {:mod :bufferline}
  :shougo/deoplete.nvim {:post_ (fn [] (vim.api.nvim_command "let g:deoplete#enable_at_startup = 1"))}
  :nathanaelkane/vim-indent-guides {}
  :Yggdroot/indentLine {}
  :jacoborus/tender.vim {}
  :vim-airline/vim-airline 
    {:requires ["vim-airline/vim-airline-themes" "jacoborus/tender.vim"]
          :mod :airline-theme} 
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
  :ziglang/zig.vim {}
  :rafcamlet/coc-nvim-lua {}
;  :neoclide/coc.nvim {
;                      :config (fn [] (vim.api.nvim_command "so ~/.config/nvim/coc.vim"))
;                      :ft [:rust]
;                      :requires ["antoinemadec/coc-fzf"]

  :nvim-lua/completion-nvim {}
  :nvim-lua/lsp_extensions.nvim {}
  :neovim/nvim-lspconfig {:mod :nvim_lsp} ;:ft [:haskell :rust :typescript :javascript :lua]}
  :kosayoda/nvim-lightbulb {
                            :config
                              (fn []
                                (let [nvim (require :aniseed.nvim)]
                                  (autocmd "CursorHold,CursorHoldI" "*" "lua require'nvim-lightbulb'.update_lightbulb()")))}

  :rust-lang/rust.vim {}
  :udalov/kotlin-vim {}
  :derekelkins/agda-vim {:config (fn [] (vim.api.nvim_command "let maplocalleader = \",\""))}
  :dag/vim-fish {}
  :purescript-contrib/purescript-vim {}
  :wbthomason/packer.nvim {}
  :eraserhd/parinfer-rust {:run "cargo build --release"}
  :nvim-treesitter/nvim-treesitter {:do "TSUpdate"}
  :campsiers/animate.vim {:mod :animate}
  :elkowar/nvim-gehzu {}
  :andweeb/presence.nvim {:mod :presence})


