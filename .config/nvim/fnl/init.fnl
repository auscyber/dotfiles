(module init {
              require {core core
                       plugins plugins}
              autoload {a aniseed.core
                        fennel aniseed.fennel}})
;(require :core)
;(local file (io.open "plugins.fnl" "r"))
;(local res (file:read "*all"))
;;(print ((. (require "fennel") :compileString) res {:allowedGlobals false :unfriendly true}))
;(file:close)


