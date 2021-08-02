(local wezterm (require :wezterm))
(local util (require :util))
(local base {
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
             :color_scheme  "Guezwhoz"})
(util.merge_files base :tab_bar)
;base
