{:augroup
 (fn [name ...]
   `(do
      (nvim.ex.augroup ,(tostring name))
      (nvim.ex.autocmd_)
      ,...
      (nvim.ex.augroup :END)))

 :autocmd
 (fn [...]
   `(nvim.ex.autocmd ,...))

 :_:
 (fn [name ...]
   `((. nvim.ex ,(tostring name)) ,...))

 :packer-use
 (fn [...]
   (let [a (require "aniseed.core")
         args [...]
         use-statements []]

        (for [i 1 (a.count args) 2]
          (let [name (. args i)
                block (. args (+ i 1))]
            (a.assoc block 1 name)
            (when (. block :mod)
              ;(a.assoc block :config `#((. (require "utils") :safe-require) ,(. block :mod)))
              (a.assoc block :config `(fn []
                                          (require ,(.. :plugins. (tostring (. block :mod)))))))
                                          ;(,(-?> block (. :config))))))
            (a.assoc block :mod)
            (when (. block :config)
              (a.assoc block :config `(fn []
                                        (let [(ok?# res#) (pcall ,(. block :config) ,name)]
                                          (when (not ok?#)
                                            (print "Failure loading config for" ,(tostring name) res#))))))
            (table.insert use-statements block)))

    (let [use-sym (gensym)]
      `(let [packer# (require "packer")]
         (packer#.startup {1
                              (fn [,use-sym]
                                ,(unpack
                                  (icollect [_# v# (ipairs use-statements)]
                                      `(,use-sym ,v#))))})))))

 :viml->fn
 (fn [name]
   `(.. "lua require('" *module-name* "')['" ,(tostring name) "']()"))}
