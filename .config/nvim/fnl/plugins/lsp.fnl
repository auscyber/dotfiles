(module plugins.lsp
  {require {
            _ plugins.cmp}
   autoload {lsp lspconfig
             cmp_nvim_lsp cmp_nvim_lsp
             a aniseed.core
             nvim aniseed.nvim
             cmp cmp
             utils utils
             lspkind lspkind
             npairs nvim-autopairs
             rust-tools rust-tools
             lsp-status lsp-status
             configs lspconfig.configs}
     require-macros [macros zest.macros]})



;(vim.lsp.set_log_level "debug")
(set nvim.o.completeopt "menu,menuone,noselect")


(defn on_attach [client bufnr]
  (cmp.setup.buffer {:formatting
                     {:format (lspkind.cmp_format)}
                     :sources [{:name :nvim_lsp} {:name :buffer} {:name :luasnip}]})
  (lsp-status.on_attach client)
  (lspkind.init {})
  (npairs.setup {})
  (let [opts {:noremap true :silent true}
        map (fn [key command] (nvim.buf_set_keymap bufnr :n key command opts))
        imap (fn [key command] (nvim.buf_set_keymap bufnr :i key command {:noremap false :silent false}))
        inoremap (fn [key command] (nvim.buf_set_keymap bufnr :i key command (a.merge opts {:expr true})))]
    (nvim.buf_set_option bufnr :omnifunc :v:lua.vim.lsp.omnifunc)
    (map :gD "<Cmd>lua vim.lsp.buf.declaration()<CR>")
    (map :gd "<Cmd>lua vim.lsp.buf.definition()<CR>")
    (map :K "<Cmd>lua vim.lsp.buf.hover()<CR>")
    (map :gi "<cmd>lua vim.lsp.buf.implementation()<CR>")
    (map :<C-K> "<cmd>lua vim.lsp.buf.signature_help()<CR>")
    (map :<leader>rn "<cmd>lua vim.lsp.buf.rename()<CR>")
    (map :<space>a "<cmd>lua require 'telescope.builtin'.lsp_workspace_diagnostics {}<CR>")
    (map :ff "<cmd>lua vim.lsp.buf.formatting()<CR>")
    (map :<leader>a "<cmd>lua require'telescope.builtin'.lsp_code_actions{}<CR>")
;    (inoremap :<CR> (vlua-format "%s()"_G.LOL.completion_confirm)))

    (def-autocmd [:BufWritePre] :<buffer> "lua vim.lsp.buf.formatting()"))
  (if client.resolved_capabilities.document_highlight (do
                                                       (utils.highlight "LspReferenceRead"  {:gui "underline"})
                                                       (utils.highlight "LspReferenceText"  {:gui "underline"})
                                                       (utils.highlight "LspReferenceWrite" {:gui "underline"})
                                                       (vim.api.nvim_exec
                                                          "augroup lsp_document_highlight
           autocmd! * <buffer>
           autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
           autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
           autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
         augroup END"
                                                         false))))

(lsp-status.register_progress)
(local capabilities (cmp_nvim_lsp.update_capabilities (vim.tbl_extend :keep (vim.lsp.protocol.make_client_capabilities) lsp-status.capabilities)))

(lua "
     local lspconfig = require('lspconfig')
-- Flag to enable semantic highlightning on start, if false you have to issue a first command manually
local autostart_semantic_highlightning = true
lspconfig.idris2_lsp.setup {
  on_new_config = function(new_config, new_root_dir)
    new_config.capabilities['workspace']['semanticTokens'] = {refreshSupport = true}
  end,
  on_attach = function(client,bufnr)
    if autostart_semantic_highlightning then
      vim.lsp.buf_request(0, 'textDocument/semanticTokens/full',
        {textDocument = vim.lsp.util.make_text_document_params()}, nil)
    end
    -- Example of how to request a single kind of code action with a keymap,
    -- refer to the table in the README for the appropriate key for each command.
    vim.cmd [[nnoremap <Leader>cs <Cmd>lua vim.lsp.buf.code_action({diagnostics={},only={'refactor.rewrite.CaseSplit'}})<CR>]]
    on_attach(client,bufnr)
  end,
  autostart = true,
  capabilities = capabilities,
  handlers = {
    ['workspace/semanticTokens/refresh'] = function(err,  params, ctx, config)
      if autostart_semantic_highlightning then
        vim.lsp.buf_request(0, 'textDocument/semanticTokens/full',
          { textDocument = vim.lsp.util.make_text_document_params() }, nil)
      end
      return vim.NIL
    end,
    ['textDocument/semanticTokens/full'] = function(err,  result, ctx, config)
      -- temporary handler until native support lands
      local bufnr = ctx.bufnr
      local client = vim.lsp.get_client_by_id(ctx.client_id)
      local legend = client.server_capabilities.semanticTokensProvider.legend
      local token_types = legend.tokenTypes
      local data = result.data

      local ns = vim.api.nvim_create_namespace('nvim-lsp-semantic')
      vim.api.nvim_buf_clear_namespace(bufnr, ns, 0, -1)
      local tokens = {}
      local prev_line, prev_start = nil, 0
      for i = 1, #data, 5 do
        local delta_line = data[i]
        prev_line = prev_line and prev_line + delta_line or delta_line
        local delta_start = data[i + 1]
        prev_start = delta_line == 0 and prev_start + delta_start or delta_start
        local token_type = token_types[data[i + 3] + 1]
        local line = vim.api.nvim_buf_get_lines(bufnr, prev_line, prev_line + 1, false)[1]
        local byte_start = vim.str_byteindex(line, prev_start)
        local byte_end = vim.str_byteindex(line, prev_start + data[i + 2])
        vim.api.nvim_buf_add_highlight(bufnr, ns, 'LspSemantic_' .. token_type, prev_line, byte_start, byte_end)
      end
    end
  },
}

-- Set here your preferred colors for semantic values
vim.cmd [[highlight link LspSemantic_type Include]]   -- Type constructors
vim.cmd [[highlight link LspSemantic_function Identifier]] -- Functions names
vim.cmd [[highlight link LspSemantic_enumMember Number]]   -- Data constructors
vim.cmd [[highlight LspSemantic_variable guifg=gray]] -- Bound variables
vim.cmd [[highlight link LspSemantic_keyword Structure]]  -- Keywords
vim.cmd [[highlight link LspSemantic_namespace Identifier]] -- Explicit namespaces
vim.cmd [[highlight link LspSemantic_postulate Define]] -- Postulates
vim.cmd [[highlight link LspSemantic_module Identifier]] -- Module identifiers
")



(fn init-lsp [lsp-name ?opts]
  "initialise a language server with defaults"
  (let [merged-opts (a.merge {:on_attach on_attach}
                             : capabilities
                            (or ?opts {}))]
    (if (~= merged_opts.fts nil)
        (let [fts (if (= (type merged_opts.fts) :table)
                    (table.concat  merged_opts.fts ",") merged_opts.fts)]
          (a.assoc merged_opts :fts)
         (vim.cmd (vlua-format (.. "au FileType " fts " ++once call %s()") (fn [] ((. lsp lsp-name :setup) merged-opts) (vim.cmd ::LspStart)))))
      ((. lsp lsp-name :setup) merged-opts))))

(def-augroup :LspAuGroup
  (init-lsp :tsserver {:fts [:typescript :javascript]})
  (init-lsp :hls {:fts [:haskell] :settings {:haskell {:formattingProvider :fourmolu}}})
  (init-lsp :gopls {:fts :go})
  (def-autocmd-fn [:FileType] [:rust]
      (do (rust-tools.setup {:server {: capabilities
                                      :settings
                                      {:rust-analyzer
                                       {:checkOnSave {:command :clippy}
                                        :procMacro {:enable true}}}
                                      :on_attach (fn [client bufnr]  
                                                    (on_attach client bufnr) 
                                                    ((. (require "rust-tools.inlay_hints") :set_inlay_hints)))}}) (vim.cmd ::LspStart)))
  (init-lsp :clangd {:fts [:cpp :c]})
  (init-lsp :rnix)
  (init-lsp :ocamllsp)
  (init-lsp :pylsp)
  (init-lsp :zls)
  (init-lsp :metals)
  (init-lsp :dhall_lsp_server)
  (init-lsp :purescriptls)
  (init-lsp :powershell_es {:bundle_path "~/packages/PowershellEditorServices"}))
;(init-lsp)
; (init-lsp :idris2_lsp)

