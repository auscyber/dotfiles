(module animate)


(fn get-animate-fn [str] (. vim.fn (.. "animate#" str)))

(def delta-height
  "change window height by var"
  (get-animate-fn "delta-height"))

(def delta_width
  "change window width by var"
  (get-animate-fn "delta-width"))
