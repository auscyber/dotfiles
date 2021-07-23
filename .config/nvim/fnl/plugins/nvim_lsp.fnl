(module plugins.nvim_lsp
  {autoload {lsp lspconfig
             a aniseed.core
             nvim aniseed.nvim
             completion completion
             utils utils
             lspkind lspkind
             npairs nvim-autopairs}

   require-macros [macros]})

(vim.lsp.set_log_level "debug")
(set nvim.o.completeopt "menuone,noinsert,noselect")
(set nvim.g.completion_enable_auto_popup 1)


(set _G.LOL {})

(set _G.LOL.completion_confirm (fn []
                                 (lua "
 if vim.fn.pumvisible() ~= 0  then
    if vim.fn.complete_info()[\"selected\"] ~= -1 then
      completion.confirmCompletion()
      return npairs.esc(\"<c-y>\")
    else
      vim.api.nvim_select_popupmenu_item(0 , false , false ,{})
      completion.confirmCompletion()
      return npairs.esc(\"<c-n><c-y>\")
    end
  else
    return npairs.autopairs_cr()
  end

                                  ")))

(fn on_attach [client bufnr]
  (completion.on_attach client bufnr)
  (lspkind.init {})
  (npairs.setup {})
  (let [ opts {:noremap true :silent true}
        map (fn [key command] "lol" (nvim.buf_set_keymap bufnr :n key command opts))
        imap (fn [key command] (nvim.buf_set_keymap bufnr :i key command {:noremap false :silent true}))
        inoremap (fn [key command] (nvim.buf_set_keymap bufnr :i key command (a.merge opts {:expr true})))]
    (nvim.buf_set_option bufnr :omnifunc :v:lua.vim.lsp.omnifunc)
    (map :gD "<Cmd>lua vim.lsp.buf.declaration()<CR>")
    (map :gd "<Cmd>lua vim.lsp.buf.definition()<CR>")
    (map :K "<Cmd>lua vim.lsp.buf.hover()<CR>")
    (map :gi "<cmd>lua vim.lsp.buf.implementation()<CR>")
    (map :<C-K> "<cmd>lua vim.lsp.buf.signature_help()<CR>")
;    (map :<leader>rn "<cmd> lua vim.lsp.)
    (map :<leader>rn "<cmd>lua vim.lsp.buf.rename()<CR>")
    (imap :<c-space> "<Plug>(completion_trigger)")
    (imap :<tab> "<Plug>(completion_smart_tab)")
    (imap :<s-tab> "<Plug>(completion_smart_s_tab)")
    (inoremap "<Tab>" "pumvisible() ? \"\\<C-n>\" : \"\\<Tab>\"")
    (inoremap "<S-Tab>" "pumvisible() ? \"\\<C-p>\" : \"\\<S-Tab>\"")
    (map :<space>a "<cmd>lua require 'telescope.builtin'.lsp_workspace_diagnostics {}<CR>")
    (map :ff "<cmd>lua vim.lsp.buf.formatting()<CR>")
    (map :<leader>a "<cmd>lua require'telescope.builtin'.lsp_code_actions{}<CR>")
    (inoremap :<CR> "v:lua.LOL.completion_confirm()"))
;    (autocmd "BufEnter,BufWinEnter,TabEnter *.rs :lua require'lsp_extensions'.inlay_hints{}")

    ;(autocmd :BufWritePre :<buffer> "lua vim.lsp.buf.formatting()")
  (if client.resolved_capabilities.document_highlight (do
  ;   (a.println "bob")
                                                       (utils.highlight "LspReferenceRead"  {:gui "underline"})
                                                       (utils.highlight "LspReferenceText"  {:gui "underline"})
                                                       (utils.highlight "LspReferenceWrite" {:gui "underline"})
                                                       (vim.api.nvim_exec
                                                          "augroup lsp_document_highlight
           autocmd! * <buffer>
           autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
           autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
           autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting()
         augroup END"
                                                         false))))



(fn init-lsp [lsp-name ?opts]
  "initialize a language server with defaults"
  (let [merged-opts (a.merge {:on_attach on_attach} (or ?opts {}))]
    ((. lsp lsp-name :setup) merged-opts)))

(init-lsp :tsserver)
(init-lsp :hls {:settings {:haskell {:formattingProvider :fourmolu}}})
(init-lsp :gopls)
(init-lsp :rust_analyzer)
(init-lsp :clangd)
(init-lsp :rnix)
;(init-lsp :denols)
(init-lsp :ocamllsp)
(init-lsp :pylsp)
(init-lsp :zls)
(init-lsp :metals)
