(module plugins.treesitter
  {require {treesitter nvim-treesitter.configs
            parsers nvim-treesitter.parsers}})

(tset (parsers.get_parser_configs) :agda {
                                          :install_info {:url "~/packages/tree-sitter-agda"
                                                         :files [:src/parser.c :src/scanner.cc]}
                                          :filetype :agda})

(treesitter.setup {
                   :ensure_installed [:rust :haskell :agda :javascript :c :fennel :go :zig :query :nix]
                   :highlight {
                               :enable true}
                   :autopairs {:enable true}
                   :playground {
                                :enable true}
                   :query_linter {:enable true}})
