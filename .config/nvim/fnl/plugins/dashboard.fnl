(module plugins.dashboard {require-macros [zest.macros]})

(set vim.g.dashboard_default_executive :telescope)
(set vim.g.dashboard_session_directory
     (vim.fn.expand (.. (vim.fn.stdpath :config) :/sessions)))

;(set vim.g.dashboard_custom_header
;    [
;;      "     ${W_FG}▄▄▄▄▄${NORMAL}"
;      "     ﱢﱢﱢﱢﱢﱢﱢﱢ"])
