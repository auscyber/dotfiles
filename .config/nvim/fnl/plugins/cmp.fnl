(module plugins.nvim_lsp
  {require {luasnip luasnip
            cmp_nvim_lsp cmp_nvim_lsp
            _ cmp_buffer
            _ cmp_luasnip
            cmp cmp
            cmp_autopairs nvim-autopairs.completion.cmp}
   require-macros [macros zest.macros]})


(cmp.setup {
              :snippet {
                        :expand (fn [args] (luasnip.lsp_expand args.body))}
              :mapping {
                        :<C-d> (cmp.mapping (cmp.mapping.scroll_docs -4) [ :i :c])
                        :<C-f> (cmp.mapping (cmp.mapping.scroll_docs 4) [ :i :c])
                        :<C-Space> (cmp.mapping (cmp.mapping.complete) [ :i :c])
                        :<C-e> (cmp.mapping {
                                                :i (cmp.mapping.abort)
                                                :c  (cmp.mapping.close)})
                        :<Up> (cmp.mapping.select_prev_item {:behavior cmp.SelectBehavior.Select})
                        :<Down> (cmp.mapping.select_next_item {:behavior cmp.SelectBehavior.Select})
                        :<Tab> (cmp.mapping.select_next_item)
                        :<S-Tab> (cmp.mapping.select_prev_item)
                        :<CR> (cmp.mapping.confirm {:behavior  cmp.ConfirmBehavior.Replace :select true})}
              :experimental {
                             :ghost_text true}
              :sources (cmp.config.sources [
                                            {:name :luasnip}]
                                          [{:name :buffer}])})

(cmp.event:on :confirm_done (cmp_autopairs.on_confirm_done {:map_char {:tex ""}}))
(def-autocmd-fn [:FileType] [(table.concat [:<buffer> :lua :fennel] ",")] (cmp.setup.buffer {:sources [{:name :nvim_lua}]}))
