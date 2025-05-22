  (local wezterm (require :wezterm))
  (local lume (require :lume))
  (local util (require :util))


  (local target_triples (lume.split wezterm.target_triple "-"))
  (local acos (. target_triples 3))

  (fn ?windows [a b]
      (if (= acos "windows")
        a)
      b)

  (local base {
                :color_schemes  {
                                 "Pink Ocean"
                                 (require "pink_ocean")}
                :front_end  "WebGpu"
                :webgpu_preferred_adapter (. (wezterm.gui.enumerate_gpus) 1)
                :max_fps 120
                :webgpu_power_preference   "HighPerformance"
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
                  :font  (?windows (wezterm.font "FiraCode NF") (wezterm.font_with_fallback [ "Hasklug Nerd Font Mono"  "codicon"]))
                  :font_size  15
                  :color_scheme "Pink Ocean"})
;             :color_scheme  "Guezwhoz"})
(util.merge_files base :tab_bar)
;base
