(module plugins.nvim_lsp
  {autoload {lsp lspconfig
             a aniseed.core
             nvim aniseed.nvim
             completion completion
             utils utils}

   require-macros [macros]})

(set nvim.o.completeopt "menuone,noinsert,noselect")
(set nvim.g.completion_enable_auto_popup 1)





(fn on_attach [client bufnr]
  (completion.on_attach client bufnr)
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
    (map :<leader>rn "<cmd>lua vim.lsp.buf.rename()<CR>")
    (imap :<c-space> "<Plug>(completion_trigger)")
    (inoremap "<Tab>" "pumvisible() ? \"\\<C-n>\" : \"\\<Tab>\"")
    (inoremap "<S-Tab>" "pumvisible() ? \"\\<C-p>\" : \"\\<S-Tab>\"")
    (map :<space>a "<cmd>lua require 'telescope.builtin'.lsp_workspace_diagnostics {}<CR>")
    (map :ff "<cmd>lua vim.lsp.buf.formatting()<CR>"))



  (if client.resolved_capabilities.document_highlight
   (do
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
(init-lsp :hls)
(init-lsp :gopls)
(init-lsp :rust_analyzer)
