(module plugins.feline
        {require {feline feline
                  colors colors
                  lsp feline.providers.lsp
                  vi_mode_utils feline.providers.vi_mode}
         autoload {lsp-status lsp-status}})

(local b vim.b)
(local fnn vim.fn)

(set vim.o.termguicolors true)
(local vi_mode_colors {:NORMAL colors.white
                       :OP :green
                       :INSERT colors.dark_cyan
                       :VISUAL colors.blue
                       :BLOCK colors.blue
                       :REPLACE colors.red
                       :V-REPLACE :violet
                       :ENTER :cyan
                       :MORE :cyan
                       :SELECT :orange
                       :COMMAND colors.black
                       :SHELL :green
                       :TERM :green
                       :NONE :yellow})

(fn nc [...]
  (accumulate [str "" _ v (ipairs [...])]
    (if v
        (.. str v)
        str)))

(local spinner_frames ["⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"])

(fn lsp-statusline-short []
  (local buf_messages (lsp-status.messages))
  (local outputs {})
  (each [_ msg (ipairs buf_messages)]
    (when msg.progress
      (table.insert outputs
                    (. spinner_frames
                       (+ 1 (% msg.spinner (length spinner_frames)))))))
  (table.concat outputs " "))

(fn lsp-statusline []
  (local buf_messages (lsp-status.messages))
  (local msgs {})
  (each [_ msg (ipairs buf_messages)]
    (let [client_name (.. "[" msg.name "]")]
      (table.insert msgs (nc " "
                             (if msg.progress
                                 ; add spinner if the spinner exists
                                 (nc (when msg.spinner
                                       (.. (. spinner_frames
                                              (+ 1
                                                 (% msg.spinner
                                                    (length spinner_frames))))
                                           " "))
                                     ; add the title
                                     msg.title
                                     ; if there is a message add the message
                                     (nc " " msg.message) ; add the percentage
                                     (when msg.percentage
                                       (string.format " (%.0f%%%%)"
                                                      msg.percentage)))
                                 msg.content)))))
  (or (table.concat msgs " ") ""))

(local components {:active [{} {} {}] :inactive [{} {} {}]})

(tset components.active 1
      [{:provider " 󰊠  "
        :left_sep ["  " {:str " " :hl {:fg colors.cyan :bg :NONE}}]
        :hl #{:bg colors.cyan
              :style :bold
              :name (vi_mode_utils.get_mode_highlight_name)
              :fg (vi_mode_utils.get_mode_color)}}
       {:provider :file_info
        :type :relative-short
        :hl {:fg :white :bg colors.dark_cyan :style :bold}
        :left_sep {:str " " :hl {:bg colors.dark_cyan}}
        ;                                             "slant_left_2" {:str  " " :hl  {:bg colors.grey :fg  "NONE"}}]
        :right_sep [{:str " " :hl {:bg colors.dark_cyan}}]}
       {:provider :file_size
        :hl {:bg :black}
        :enabled (fn []
                   (> (fnn.getfsize (fnn.expand "%:p")) 0))
        :left_sep {:str " " :hl {:bg :black}}
        :right_sep [{:str " " :hl {:bg :black}}
                    {:str :right_rounded :hl {:fg :black :bg :NONE}}]}])

(tset components.active 2
      [{:provider :lsp_client_names
        :hl {:fg :white :bg "#3f3f3f"}
        ;        :enabled (vim.lsp.buf.server_ready)
        :enabled #(> (length (vim.lsp.buf_get_clients)) 0)
        :right_sep [{:str :right_rounded :hl {:fg "#3f3f3f" :bg :NONE}}]
        :left_sep [{:str :left_rounded :hl {:fg "#3f3f3f" :bg :NONE}}]}])

(tset components.active 3 [{:provider :position
                            :hl {:bg colors.red :fg colors.white :style :bold}
                            :left_sep [{:str :left_rounded
                                        :hl {:fg colors.red :bg :NONE}}]
                            :right_sep [{:str " " :hl {:bg colors.red}}
                                        {:str " " :hl {:bg :black}}]}
                           {:provider :git_branch
                            :hl {:fg :white :bg :black :style :bold}
                            :right_sep (fn []
                                         (local val
                                                {:hl {:fg :NONE :bg :black}})
                                         (if b.gitsigns_status_dict
                                             (set val.str " ")
                                             (set val.str ""))
                                         val)}
                           {:provider :git_diff_added
                            :hl {:fg :green :bg :black}}
                           {:provider :git_diff_changed
                            :hl {:fg :orange :bg :black}}
                           {:provider :git_diff_removed
                            :hl {:fg :red :bg :black}
                            :right_sep (fn []
                                         (local val
                                                {:hl {:fg :NONE :bg :black}})
                                         (if b.gitsigns_status_dict
                                             (set val.str " ")
                                             (set val.str ""))
                                         val)}
                           {:provider :line_percentage
                            :hl {:bg :black :style :bold}
                            :left_sep {:str " " :hl {:bg :black}}
                            :right_sep {:str " " :hl {:bg :black}}}
                           {:provider :diagnostic_errors
                            :enabled #(lsp.diagnostics_exist :error)
                            :right_sep {:str " " :hl {:bg :black}}
                            :hl {:bg :black :fg :red}}
                           {:provider :diagnostic_warnings
                            :enabled #(lsp.diagnostics_exist :warn)
                            :right_sep {:str " " :hl {:bg :black}}
                            :hl {:bg :black :fg :yellow}}
                           {:provider :diagnostic_hints
                            :enabled #(lsp.diagnostics_exist :hint)
                            :right_sep {:str " " :hl {:bg :black}}
                            :hl {:bg :black :fg :cyan}}
                           {:provider :diagnostic_info
                            :enabled #(lsp.diagnostics_exist :info)
                            :hl {:fg :skyblue :bg :black}}
                           {:provider #(lsp-statusline)
                            :short_provider #(lsp-statusline-short)
                            :enabled #(> (length (vim.lsp.buf_get_clients)) 0)
                            :hl {:bg :black}
                            :right_sep {:str " " :hl {:bg :black}}}
                           {:provider :scroll_bar
                            :hl {:bg :black :fg colors.red :style :bold}
                            :right_sep [{:str " " :hl {:bg :black}}
                                        {:str :right_rounded
                                         :hl {:fg :black :bg :NONE}}
                                        " "]}])

(tset components.inactive 1
      [{:provider :file_type
        :hl {:fg :white :bg colors.cyan}
        :left_sep [{:str " " :hl {:fg :NONE :bg :NONE}}
                   :left_rounded
                   {:str " " :hl {:fg :NONE :bg colors.cyan}}]
        :right_sep {:str " " :hl {:bg colors.cyan}}}
       {:provider :file_info
        :hl {:fg :white :bg colors.dark_cyan :style :bold}
        :left_sep {:str " " :hl {:bg colors.dark_cyan}}
        :right_sep [{:str " " :hl {:fg :NONE :bg colors.dark_cyan}}
                    {:str :right_rounded :hl {:bg :NONE :fg colors.dark_cyan}}]}])

--

This

table

is

equal

to

the

default

separators

table
(local separators {:vertical_bar "┃"
                   :vertical_bar_thin "│"
                   :left ""
                   :right ""
                   :block "█"
                   :left_filled ""
                   :right_filled ""
                   :slant_left ""
                   :slant_left_thin ""
                   :slant_right ""
                   :slant_right_thin ""
                   :slant_left_2 ""
                   :slant_left_2_thin ""
                   :slant_right_2 ""
                   :slant_right_2_thin ""
                   :left_rounded ""
                   :left_rounded_thin ""
                   :right_rounded ""
                   :right_rounded_thin ""
                   :circle "●"})

;(feline.winbar.setup)

(feline.setup {: colors
               : separators
               :force_inactive {:filetypes [:NvimTree
                                            :dbui
                                            :packer
                                            :startify
                                            :NeogitStatus
                                            :fugitive
                                            :fugitiveblame
                                            :telescope]
                                :buftypes [:terminal]
                                :bufnames []}
               : components
               : vi_mode_colors})
