(local wezterm (require :wezterm))
(local util (require :util))

(local os (let [(p err) (pcall #(io.popen "uname -o 2>/dev/null" :r))]
            (if err
              "Windows"
              (do (var res (p:read))
                ((p:close))
                (res)))))


(fn ?windows [...]
  (if (= os "Windows")
    ...))

(local base {
              :color_schemes  {
                               "Pink Ocean"
                               (require "pink_ocean")}
              :enable_scroll_bar true ;(~= os "GNU/Linux")
              :enable_wayland true
              :window_padding  {
                                :left 10
                               ; This will become the scrollbar width if you have enabled the scrollbar!
                                :right  10

                                :top  5
                                :bottom  5}
              :default_prog (?windows [:powershell :pwsh])
              :window_background_opacity  0.8
              :font  (?windows (wezterm.font "FiraCode NF") (wezterm.font_with_fallback [ "Hasklug Nerd Font"  "codicon"]))
              :font_size  15
              :color_scheme "Pink Ocean"})
;             :color_scheme  "Guezwhoz"})
(util.merge_files base :tab_bar)
;base
