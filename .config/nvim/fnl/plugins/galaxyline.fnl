(module plugins.galaxyline {require {nvim aniseed.nvim gl galaxyline}
                            autoload {color colors}
                            require-macros [macros]})

;(tset gl.section.left 0)

(local gls gl.section)
(set gl.short_line_list [:LuaTree :vista :dbui])

(local colors {:bg "#282c34"
               :yellow "#fabd2f"
               :cyan "#008080"
               :darkblue "#081633"
               :green "#afd700"
               :orange "#FF8800"
               :purple "#5d4d7a"
               :magenta "#d16d9e"
               :grey "#c0c0c0"
               :blue "#0087d7"
               :red "#ec5f67"})

(local buffer_not_empty (fn []
                          (if (not= 1 (vim.fn.empty (vim.fn.expand "%:t")))
                              true
                              false)))

(tset gls.left 1
      {:FirstElement {:provider (fn [] "  ") :highlight [color.light_red]}})

(tset gls.left 2
      {:ViMode {:provider (fn []
                            (local mode_color
                                   {:n color.white
                                    :i colors.cyan
                                    :v colors.blue
                                    "\022" colors.blue
                                    :V colors.blue
                                    :c color.purple
                                    :no colors.red
                                    :s colors.orange
                                    :S colors.orange
                                    "\019" colors.orange
                                    :ic colors.yellow
                                    :R colors.violet
                                    :Rv colors.violet
                                    :cv colors.red
                                    :ce colors.red
                                    :r colors.cyan
                                    :rm colors.cyan
                                    [:r?] colors.cyan
                                    ["!"] colors.red
                                    :t colors.red})
                            ;(_: hi (.. "GalaxyViMode guifg=" (. mode_color (vim.fn.mode))))
                            " ")
                ;                                                  (fn [] (if (not (buffer_not_empty)) color.dark_cyan color.cyan))]
                :separator "  "
                :separator_highlight [nil color.purple]
                :highlight [color.grey color.light_red :bold]}})

(tset gls.left 3
      {:FFileIcon {:provider :FileIcon
                   :highlight [:NONE color.purple]
                   :separator " "
                   :separator_highlight [color.purple]}})

(tset gls.left 4 {:FileNameSepFirst {:provider (fn [] "  ")}})

(tset gls.left 5 {:FileName {:provider :FileName}})

(tset gls.right 1 {:LspStatus {:condition (fn [] (vim.lsp.buf_is_attached 0))
                               :provider :GetLspClient}})

;(tset gls.right 2 {
;                   :LineNumber {
;                                :condition}})
;

(tset gls.short_line_left 1
      {:BufferFirstElement {:provider (fn [] "") :highlight [color.red]}})

(tset gls.short_line_left 2
      {:BufferFileName {:provider :FileName :hightlight [nil color.red]}})

(tset gls.short_line_left 3
      {:BufferType {:provider :FileTypeName
                    :separator ""
                    :separator_highlight [color.red nil]
                    :highlight [nil color.red :bold]}})
