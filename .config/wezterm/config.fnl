(local wezterm (require :wezterm))
(local util (require :util))

(local os (let [(p _) (io.popen "uname -o 2>/dev/null" :r)]
            (var res (p:read))
            (p:close)
            res))
(local base {
              :color_schemes  {
                               "Pink Ocean" 
                               {
                                :foreground  "#d0d0d0"
                                :cursor_bg  "#eeeeee"
                                :cursor_border  "#eeeeee"
                                :cursor_fg  "#eeeeee"
                                :selection_bg  "#005f5f"
                                :selection_fg  "#eeeeee"
                                :ansi  ["#080808" "#ff5f5f" "#87d7af" "#d7d787" "#5fafd7" "#afafff" "#5fd7d7" "#dadada"]
                                :brights  ["#8a8a8a" "#d75f5f" "#afd7af" "#d7d7af" "#87afd7" "#afafd7" "#87d7d7" "#dadada"]
;                                :background "#282828"}}
                                :background "#1c1c1c"}}

              :enable_scroll_bar (~= os "GNU/Linux")
              :enable_wayland false
              :window_padding  {
                                :left 10
                               ; This will become the scrollbar width if you have enabled the scrollbar!
                                :right  10

                                :top  5
                                :bottom  5}

              :window_background_opacity  0.8
              :font  (wezterm.font "Hasklug Nerd Font")
              :font_size  10
              :color_scheme "Pink Ocean"})
;             :color_scheme  "Guezwhoz"})
(util.merge_files base :tab_bar)
;base
