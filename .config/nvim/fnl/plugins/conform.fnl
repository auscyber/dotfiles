(module plugins.confirm
  {require {conform conform}

    require-macros [macros zest.macros]})



(def-autocmd-fn [:BufWritePre] :<buffer>
  (fn [args]
    (let [conform (require "conform")]
        (conform.format {:bufnr args.buf}))))




(conform.setup {
                :formatters {
                             :injected {
                                        :options {:ignore_errors true
                                                  :lang_to_ft {
                                                               :bash  "sh"}}}}



                :formatters_by_ft  {
                                    :fish [:fish_indent]
                                    :bash [:shfmt]
                                    :sh  [:shfmt]
                                    :lua  [ "stylua"]
                                    ; Conform will run multiple formatters sequentially
                                    :rust  {1 "rustfmt" :lsp_format  "fallback"}
                                    ; Conform will run the first available formatter
                                    :nix [:injected :nixfmt]
                                    :javascript  {1 "prettierd" 2 "prettier" :stop_after_first  true}}})

