(module plugins.lsp
  {require {
            plugins-cmp plugins.cmp}
   autoload {idris2 idris2
             lsp-signature lsp_signature
             virtualtypes virtualtypes
             renamer renamer
             lsp_installer nvim-lsp-installer
             lsp_installer_servers nvim-lsp-installer.servers
             lsp lspconfig
             cmp_nvim_lsp cmp_nvim_lsp
             a aniseed.core
             nvim aniseed.nvim
             wk which-key
             cmp cmp
             utils utils
             lspkind lspkind
             rust-tools rust-tools
             lsp-status lsp-status
             configs lspconfig.configs}
     require-macros [macros zest.macros]})



(vim.lsp.set_log_level "debug")
(set nvim.o.completeopt "menu,menuone,noselect")


(defn on_attach [client bufnr]
  (renamer.setup {})
  (cmp.setup.buffer {:sources (a.concat [{:name :nvim_lsp :max_item_count 1000}] sources)})
  (lsp-cap codeLens
    (virtualtypes.on_attach client bufnr))
  (lsp-status.on_attach client bufnr)
  (lsp-signature.on_attach
    {
     :bind true
     :handler_opts
       {
        :border :rounded}}
    bufnr)
  (let [opts {:noremap true :silent true}
        basem (fn [mode key command commen]
                (nvim.buf_set_keymap bufnr mode key command opts)
                (wk.register {
                              key
                              commen}
                             (a.merge opts {: mode :buffer bufnr})))
        map (fn [...] (basem :n ...))
        rangemap (fn [key command] (nvim.buf_set_keymap bufnr :v key command opts))]
    (nvim.buf_set_option bufnr :omnifunc :v:lua.vim.lsp.omnifunc)
    (map :gD "<Cmd>lua vim.lsp.buf.declaration()<CR>" "Go to declaration")
    (map :gd "<Cmd>lua vim.lsp.buf.definition()<CR>" "Go to definition")
    (lsp-cap signature_help
      (map :K "<Cmd>lua vim.lsp.buf.signature_help()<CR>" "Signature help"))
    (map :gi "<cmd>lua vim.lsp.buf.implementation()<CR>" "Go to implementations")
    (map :<space>r "<cmd>lua vim.lsp.buf.references()<CR>" "See references")
;    (map :<C-K> "<cmd>lua vim.lsp.buf.signature_help()<CR>" "")
    (map :<leader>rn "<cmd>lua require(\"renamer\").rename()<cr>" "Rename symbol under cursor")
    (map :<space>d "<cmd>lua require 'telescope.builtin'.diagnostics {}<CR>" "See workspace diagnostics")
    (map :<space>a "<cmd>lua vim.lsp.buf.code_action ()<CR>" "See code actions under cursor")
    (map :ff "<cmd>lua vim.lsp.buf.formatting()<CR>" "format file")
    (rangemap :ff "<cmd>lua vim.lsp.buf.range_formatting()<CR>" "format selected")
    (def-autocmd [:BufWritePre] :<buffer> "lua vim.lsp.buf.format()"))
  (lsp-cap hover
    (def-augroup :lsp_hover
      (def-autocmd :CursorHold :* "lua vim.lsp.buf.hover()")))


  (lsp-cap document_highlight
    (utils.highlight "LspReferenceRead"  {:gui "underline"})
    (utils.highlight "LspReferenceText"  {:gui "underline"})
    (utils.highlight "LspReferenceWrite" {:gui "underline"})
    (vim.api.nvim_exec "augroup lsp_document_highlight
           autocmd! * <buffer>
           autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
            augroup END" false))
  (vim.api.nvim_exec
     "augroup lsp_references
           autocmd! * <buffer>
           autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
           autocmd CursorHold,CursorHoldI <buffer> lua require'nvim-lightbulb'.update_lightbulb()
         augroup END"
    false))
(lspkind.init {
               :preset :default})


(lsp-status.config {
                    :status_symbol " "
                    :kind_labels lspkind.symbol_map
                    :indicator_errors "  "
                    :indicator_warnings "  "
                    :indicator_hint "  "
                    :indicator_info "  "})

(lsp-status.register_progress)

(local capabilities (cmp_nvim_lsp.default_capabilities lsp-status.capabilities))

(set lsp.util.default_config
     (vim.tbl_extend
       :force
        lsp.util.default_config
          {
           : capabilities
           :handlers {
                      :window/showMessage (fn [err result ctx config]
                                            (let
                                              [MessageType vim.lsp.protocol.MessageType
                                               levels vim.log.levels
                                               mat {MessageType.Error levels.ERROR
                                                    MessageType.Warning levels.WARN
                                                    MessageType.Info levels.INFO
                                                    MessageType.Log levels.TRACE}]
                                              (vim.notify result.message (. mat result.type))))}}))



(var lsp_server_count 0)
(var lsp_server_table [])

(fn au_ft_once [fts once]
  (var index nil)
  (let [_fts (if (= (type fts) :table)
               (do
                (set index lsp_server_count)
                (set lsp_server_count (+ lsp_server_count 1))
                (tset lsp_server_table index true)
                (table.concat fts ","))
               fts)]

    (vim.cmd (vlua-format (.. "au FileType " _fts " ++once call %s()")
                          (if index
                            (fn []
                              (when (. lsp_server_table index)
                                  (tset lsp_server_table index false)
                                  (once)
                                  (vim.cmd ::LspStart)))
                            (fn []
                              (once)
                              (vim.cmd ::LspStart)))))))


(fn init-lsp [lsp-name ?opts]
  "initialise a language server with defaults"
  (let [merged-opts (a.merge {: on_attach}
;                              : capabilities}
                            (or ?opts {}))
        run-server #(let
                      [(server_available requested_server) (lsp_installer_servers.get_server lsp-name)]
                     (if (and (not ?opts.no-install) server_available)
                      (do
                        (requested_server:on_ready #(requested_server:setup merged-opts))
                        (if (not (requested_server:is_installed))
                          (requested_server:install)))
                      ((. lsp lsp-name :setup) merged-opts)))]
      (if merged_opts.fts
          (let [fts merged_opts.fts]
            (a.assoc merged_opts :fts)
            (au_ft_once fts run-server))
          (run-server))))


(def-augroup :LspAuGroup
  (init-lsp :tsserver {:fts [:typescriptreact :typescript :javascript] :autostart false})
  (init-lsp :denols {:fts [:typescript] :autostart false})
  (init-lsp :hls {:no-install true :fts [:haskell] :settings {:haskell {:formattingProvider :fourmolu}}})
  (init-lsp :gopls {:fts :go})
  (init-lsp :sumneko_lua {:fts :lua})
  (au_ft_once :rust
      (fn []
        (let [(server_available requested_server) (lsp_installer_servers.get_server "rust_analyzer")
              opts {: capabilities
                    :settings
                      {:rust-analyzer
                       {:checkOnSave {:command :clippy}
                        :procMacro {:enable true}}}
                    :on_attach (fn [client bufnr]
                                  ((. (require "rust-tools.inlay_hints") :set_inlay_hints))
                                  (on_attach client bufnr))}]
          (when server_available
           (requested_server:on_ready (fn [server]
                                         (rust-tools.setup {:server (vim.tbl_deep_extend :force (server:get_default_options) opts)})
                                        (server:attach_buffers)))
           (when (not (requested_server:is_installed))
             (requested_server:install))))))
  (init-lsp :clangd {:fts [:cpp :c] :init_options {:clangdFileStatus true} :handlers (lsp-status.extensions.clangd.setup)})
;  (init-lsp :ccls {:fts [:cpp :c]})
  (init-lsp :rnix {:fts :nix})
  (init-lsp :ocamlls {:fts :ocaml})
  (init-lsp :pyright {:fts :python
                      :handlers (lsp-status.extensions.pyls_ms.setup)
                      :settings
                        {:python {:workspaceSymbols {:enable true}}}})
  (init-lsp :zls {:fts :zig})
  (init-lsp :metals {:fts :scala})
  (init-lsp :dhall_lsp_server {:fts :dhall})
  (init-lsp :purescriptls {:fts :purescript})
  (init-lsp :powershell_es {:fts :ps1}) ;:bundle_path "~/packages/PowershellEditorServices"})
  (init-lsp :kotlin_language_server {:fts :kotlin})
  (init-lsp :omnisharp {:fts :cs})
  (init-lsp :als {:fts :ada})
  (init-lsp :jdtls {:fts :java :cmd [:jdtls] :root_dir
                    (fn [fname] (or (((. (require :lspconfig) :util :root_pattern) "pom.xml" "gradle.build" ".git") fname) (vim.fn.getcwd)))})
  (au_ft_once :idris2 (fn []
                        (idris2.setup {:server {: capabilities : on_attach}}))))

