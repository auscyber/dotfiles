(module plugins.lsp
  {require {
            plugins-cmp plugins.cmp}
   autoload {idris2 idris2
             lsp_installer nvim-lsp-installer
             lsp_installer_servers nvim-lsp-installer.servers
             lsp lspconfig
             cmp_nvim_lsp cmp_nvim_lsp
             a aniseed.core
             nvim aniseed.nvim
             cmp cmp
             utils utils
             lspkind lspkind
             rust-tools rust-tools
             lsp-status lsp-status
             configs lspconfig.configs}
     require-macros [macros zest.macros]})



;(vim.lsp.set_log_level "debug")
(set nvim.o.completeopt "menu,menuone,noselect")

(defn on_attach [client bufnr]
  (cmp.setup.buffer {:sources (a.concat sources [{:name :nvim_lsp}])})
  (lsp-status.on_attach client)
  (lspkind.init {})
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
    (map :<space>d "<cmd>lua require 'telescope.builtin'.lsp_workspace_diagnostics {}<CR>")
    (map :<space>a "<cmd>lua require'telescope.builtin'.lsp_code_actions {}<CR>")
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


(a.assoc _G :enabled_servers {})

(fn au_ft_once [fts once]
  (let [_fts (if (= (type fts) :table)
               (table.concat fts ",")
               fts)]
      (vim.cmd (vlua-format (.. "au FileType " _fts " ++once call %s()") (fn [] (once) (vim.cmd ::LspStart))))))


(fn init-lsp [lsp-name ?opts]
  "initialise a language server with defaults"
  (let [merged-opts (a.merge {: on_attach
                              : capabilities}
                            (or ?opts {}))
        (server_available requested_server) (lsp_installer_servers.get_server lsp-name)
        launch-f #(when (not (. _G.enabled_servers lsp-name))
                        (requested_server:setup merged-opts)
                        (a.assoc _G.enabled_servers lsp-name true))
        test-server #(if server_available
                      (do (requested_server:on_ready launch-f)
                          (if (not (requested_server:is_installed))
                            (requested_server:install)))
                      launch-f)]
      (if merged_opts.fts
          (let [fts merged_opts.fts]
            (a.assoc merged_opts :fts)
            (au_ft_once fts test-server))
          (test-server))))



(def-augroup :LspAuGroup
  (init-lsp :tsserver {:fts [:typescript :javascript]})
  (init-lsp :hls {:fts [:haskell] :settings {:haskell {:formattingProvider :fourmolu}}})
  (init-lsp :gopls {:fts :go})
  (init-lsp :sumneko_lua {:fts :lua})
  (au_ft_once :rust
      (fn [] (let [(server_available requested_server) (lsp_installer_servers.get_server "rust_analyzer")]
               (when server_available
                (requested_server:on_ready #(rust-tools.setup {:server {: capabilities
                                                                        :settings
                                                                        {:rust-analyzer
                                                                         {:checkOnSave {:command :clippy}
                                                                          :procMacro {:enable true}}}
                                                                        :on_attach (fn [client bufnr]
                                                                                      (on_attach client bufnr)
                                                                                      ((. (require "rust-tools.inlay_hints") :set_inlay_hints)))}}))
                (when (not (requested_server:is_installed))
                  (requested_server:install))))))
  (init-lsp :clangd {:fts [:cpp :c]})
  (init-lsp :rnix {:fts :nix})
  (init-lsp :ocamlls {:fts :ocaml})
  (init-lsp :pylsp {:fts :python})
  (init-lsp :zls {:fts :zig})
  (init-lsp :metals {:fts :scala})
  (init-lsp :dhall_lsp_server)
  (init-lsp :purescriptls {:fts :purescript})
  (init-lsp :powershell_es {:bundle_path "~/packages/PowershellEditorServices"})
  (init-lsp :jdtls {:fts :java :cmd [:jdtls] :root_dir (fn [fname] (or (((. (require :lspconfig) :util :root_pattern) "pom.xml" "gradle.build" ".git") fname) (vim.fn.getcwd)))})
  (au_ft_once :idris2 (fn []
                        (idris2.setup {:server {: capabilities : on_attach}}))))
;(lsp_installer.on_server_ready #(: $1 :setup {: capabilities : on_attach}))
