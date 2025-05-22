{
 :lsp-cap
 (fn [name ...]
   `(when (. client.server_capabilities ,(tostring name))
     ,...))


 :packer-use
 (fn [...]
   "use packer plugins
   Takes a list of package names and a table of the configuration for it
   "
   (let [a (require "aniseed.core")
         sep (package.config:sub 1 1)
         args [...]
         use-statements []]
        (for [i 1 (a.count args) 2]
          (let [name (. args i)
                block (. args (+ i 1))]
            (a.assoc block 1 name)
           ; (when (. block :mod)
           ;   (a.assoc block :config `(fn []
           ;                               (require ,(.. :plugins. (tostring (. block :mod))))
           ;                               (,(-?> block (. :config))))))
;            (a.assoc block :mod)
            (when (. block :config)
              (a.assoc block :config `(fn []
                                        (let [(ok?# res#) (pcall ,(. block :config) ,name)]
                                          (when (not ok?#)
                                            (vim.notify (string.format "Failure loading config for %s: %s" ,(tostring name) res#)))))))
            (table.insert use-statements block)))

    (let [use-sym (gensym)]
      `(let [lazy# (require "lazy")]
         (lazy#.setup {:spec
                            [,(unpack
                              (icollect [_# v# (ipairs use-statements)]
                                v#))]
                                              })))))}



