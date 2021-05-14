(module plugins.nvim_lsp
  {autoload {lsp lspconfig
             a aniseed.core
             nvim aniseed.nvim
             completion completion
             utils util}})

  ; require-macros [macros]})

(fn on_attach [client bufnr]
;  (completion.on_attach client bufnr)


  (if client.resolved_capabilities.document_highlight
    (do
      (utils.highlight "LspReferenceRead"  {:gui "underline"})
      (utils.highlight "LspReferenceText"  {:gui "underline"})
      (utils.highlight "LspReferenceWrite" {:gui "underline"})
      (nvim.api.nvim_exec
         "augroup lsp_document_highlight
           autocmd! * <buffer> 
           autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight() 
           autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
         augroup END"
        false))))

(fn init-lsp [lsp-name ?opts]
  "initialize a language server with defaults"
  (let [merged-opts (a.merge {:on_attach on_attach} (or ?opts {}))]
    ((. lsp lsp-name :setup) merged-opts)))

(init-lsp :tsserver)
;(init-lsp :hls)
