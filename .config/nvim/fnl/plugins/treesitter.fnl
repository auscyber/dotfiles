(module plugins.treesitter
        {require {treesitter nvim-treesitter.configs
                  parsers nvim-treesitter.parsers
                  install nvim-treesitter.install}})

;(tset (parsers.get_parser_configs) :agda {
;                                          :install_info {:url "~/packages/tree-sitter-agda"
;                                                         :files [:src/parser.c :src/scanner.cc]}
;                                          :filetype :agda}})
; (tset (parsers.get_parser_configs) :norg {
;                                            :install_info {:url "https://github.com/nvim-neorg/tree-sitter-norg" :files [:src/parser.c :src/scanner.cc] :branch :main}}})
(set vim.wo.foldmethod :expr)
(set vim.wo.foldexpr "nvim_treesitter#foldexpr()")
(set vim.wo.foldlevel 1)

;(if (> (vim.fn.has :win32) 0))
;  (tset (. install :compilers) [:clang])

(tset install :compilers [(let [(status res) (pcall (fn []
                                                      (require :treesitter_compiler)))]
                            (if status res))])

(treesitter.setup {:ensure_installed [:rust
                                      :haskell
                                      :javascript
                                      :c
                                      :fennel
                                      :go
                                      :zig
                                      :nix
                                      :cpp
                                      :bash
                                      :glsl
                                      :python
                                      :lua
                                      :toml
                                      :typescript
                                      :yaml
                                      :css
                                      :kotlin
                                      :markdown
                                      :jsonc]
                   :highlight {:enable true}
                   :autopairs {:enable true}
                   :playground {:enable true}
                   :query_linter {:enable true}
                   :refactor {;                              :highlight_current_scope {:enable true}}
                              :highlight_definitions {:enable true}}
                   :textobjects {:enable true
                                 :lookahead true
                                 :keymaps {:af "@function.outer"
                                           :if "@function.inner"
                                           :ac "@class.outer"
                                           :ic "@class.inner"}
                                 :move {:enable true :set_jumps true}}})
