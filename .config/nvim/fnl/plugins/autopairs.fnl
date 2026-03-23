(module autopairs {autoload {nvim aniseed.nvim
                             util dotfiles.util
                             autopairs nvim-autopairs}})

(autopairs.setup {:disable_filetype [:TelescopePrompt :vim :haskell :ps1]})
