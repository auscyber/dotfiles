(module plugins.nvim_lsp
  {autoload {lsp lspconfig
             a aniseed.core
             nvim aniseed.nvim
             completion completion
             utils utils
             lspkind lspkind
             npairs nvim-autopairs
             rust-tools rust-tools
;             cmp cmp
             configs lspconfig.configs}

   require-macros [macros zest.macros]})

;(when (not lsp.idris2_lsp)
;  (set configs.idris2_lsp {
;    :default_config  {
;                      :cmd  ["idris2-lsp"]; -- if not available in PATH, provide the absolute path
;                      :filetypes ["idris2"]
;                      }
;      :on_new_config  (fn [new_config  new_root_dir]
;        {
;          :new_config
;          {:cmd  ["idris2-lsp"]
;            :capabilities {
;                       "workspace"
;                          {"semanticTokens" {:refreshSupport true} }}}}
;      )
;      :root_dir  (fn [fname]
;        (let [scandir  (require "plenary.scandir")
;             find_ipkg_ancestor (fn [fname]
;            (lsp.util.search_ancestors fname (fn [path]
;              (local res  (scandir.scan_dir path {:depth 1 :search_pattern ".+%.ipkg"}))
;            (when (not (vim.tbl_isempty res))
;              path
;            )
;          )
;        ))]
;        (or (find_ipkg_ancestor fname) (lsp.util.find_git_ancestor fname) (vim.loop.os_homedir))
;      ))
;      :settings []
;    }
;))

;(error "hi")


;(cmp.setup {
;            :sources { :name :nvim_lsp}})
            
           

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
    (imap :<c-space> "<Plug>(completion_trigger)")
    (imap :<tab> "<Plug>(completion_smart_tab)")
    (imap :<s-tab> "<Plug>(completion_smart_s_tab)")
    (inoremap "<Tab>" "pumvisible() ? \"\\<C-n>\" : \"\\<Tab>\"")
    (inoremap "<S-Tab>" "pumvisible() ? \"\\<C-p>\" : \"\\<S-Tab>\"")
    (map :<space>a "<cmd>lua require 'telescope.builtin'.lsp_workspace_diagnostics {}<CR>")
    (map :ff "<cmd>lua vim.lsp.buf.formatting()<CR>")
    (map :<leader>a "<cmd>lua require'telescope.builtin'.lsp_code_actions{}<CR>")
    (inoremap :<CR> "v:lua.LOL.completion_confirm()"))

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
           autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
\"           autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting()
         augroup END"
                                                         false))))

(lua "
     local lspconfig = require('lspconfig')
local configs = require('lspconfig/configs')
if not lspconfig.idris2_lsp then
  configs.idris2_lsp = {
    default_config = {
      cmd = {'idris2-lsp'}; -- if not available in PATH, provide the absolute path
      filetypes = {'idris2'};
      on_new_config = function(new_config, new_root_dir)
        new_config.cmd = {'idris2-lsp'}
        new_config.capabilities['workspace']['semanticTokens'] = {refreshSupport = true}
      end;
      root_dir = function(fname)
        local scandir = require('plenary.scandir')
        local find_ipkg_ancestor = function(fname)
          return lspconfig.util.search_ancestors(fname, function(path)
            local res = scandir.scan_dir(path, {depth=1; search_pattern='.+%.ipkg'})
            if not vim.tbl_isempty(res) then
              return path
            end
          end)
        end
        return find_ipkg_ancestor(fname) or lspconfig.util.find_git_ancestor(fname) or vim.loop.os_homedir()
      end;
      settings = {};
    };
  }
end
-- Flag to enable semantic highlightning on start, if false you have to issue a first command manually
local autostart_semantic_highlightning = true
lspconfig.idris2_lsp.setup {
  on_init = custom_init,
  on_attach = function(client)
    if autostart_semantic_highlightning then
      vim.lsp.buf_request(0, 'textDocument/semanticTokens/full',
        { textDocument = vim.lsp.util.make_text_document_params() }, nil)
    end
    on_attach(client)
  end,
  autostart = true,
  handlers = {
    ['workspace/semanticTokens/refresh'] = function(err, method, params, client_id, bufnr, config)
      if autostart_semantic_highlightning then
        vim.lsp.buf_request(0, 'textDocument/semanticTokens/full',
          { textDocument = vim.lsp.util.make_text_document_params() }, nil)
      end
      return vim.NIL
    end,
    ['textDocument/semanticTokens/full'] = function(err, method, result, client_id, bufnr, config)
      -- temporary handler until native support lands
      local client = vim.lsp.get_client_by_id(client_id)
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
        vim.api.nvim_buf_add_highlight(bufnr, ns, 'LspSemantic_' .. token_type, prev_line, prev_start, prev_start + data[i + 2])
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
")


(fn init-lsp [lsp-name ?opts]
  "initialize a language server with defaults"
  (let [merged-opts (a.merge {:on_attach on_attach} 
                               ;:capabilties ((. (require "cmp_nvim_lsp") :update_capabilities) (vim.lsp.protocol.make_client_capabilities))} 
                            (or ?opts {}))]
    ((. lsp lsp-name :setup) merged-opts)))

(init-lsp :tsserver)
(init-lsp :hls {:settings {:haskell {:formattingProvider :fourmolu}}})
(init-lsp :gopls)
(rust-tools.setup {})
(init-lsp :rust_analyzer {:settings
                          {:rust-analyzer
                           {:checkOnSave {:command :clippy}
                            :procMacro {:enable true}}}})
(init-lsp :clangd)
(init-lsp :rnix)
;(init-lsp :denols)
(init-lsp :ocamllsp)
(init-lsp :pylsp)
(init-lsp :zls)
(init-lsp :metals)
(init-lsp :dhall_lsp_server)
(init-lsp :purescriptls)
(init-lsp :powershell_es {:bundle_path "~/packages/PowershellEditorServices"})
;(init-lsp)
; (init-lsp :idris2_lsp)
