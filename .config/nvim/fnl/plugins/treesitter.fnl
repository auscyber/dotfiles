(module plugins.treesitter
  {require {treesitter nvim-treesitter.configs
            parsers nvim-treesitter.parsers}})

(tset (parsers.get_parser_configs) :agda {
                                          :install_info {:url "~/packages/tree-sitter-agda"
                                                         :files [:src/parser.c :src/scanner.cc]}
                                          :filetype :agda})
;(set vim.wo.foldmethod "expr")
(set vim.wo.foldexpr "nvim_treesitter#foldexpr()")

(treesitter.setup {
                   :ensure_installed [:rust :haskell :javascript :c :fennel :go :zig  :agda :nix :cpp]
                   :highlight {
                               :enable true}
                   :autopairs {:enable true}
                   :playground {
                                :enable true}
                   :query_linter {:enable true}})
