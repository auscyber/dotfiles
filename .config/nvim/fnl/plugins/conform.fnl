(module plugins.confirm
  {require {conform conform}

    require-macros [macros zest.macros]})


(vim.api.nvim_create_user_command :Format
                                  (fn  [args]
                                    (let [range (if (~= args.count 1)
                                                  (let [end_line (vim.api.nvim_buf_get_lines 0 (- args.line2 1) args.line2 true)]
                                                    {:start [args.line1 0]
                                                     :end [ args.line2 (: (. end_line 1) :len)]}))]
                                      (conform.format {:async true :lsp_format :fallback : range}))) { :range true})




(conform.setup {
                :format_on_save true
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

