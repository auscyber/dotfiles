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
      `(let [packer# (require "packer")]
         (packer#.startup {1
                              (fn [,use-sym]
                                ,(unpack
                                  (icollect [_# v# (ipairs use-statements)]
                                      `(,use-sym ,v#))))
                              :config {:compile_path (.. (vim.fn.stdpath "config") ,sep "lua" ,sep "packer_compiled.lua")
                                       :display {:open_fn (fn [] (let [(b# win# buf#) ((. (require "packer.util") :float) {:border :rounded})]
                                                                   (vim.api.nvim_win_set_option win# :winhighlight "PmenuThumb:Normal,FloatBorder:Normal,Normal:Normal,StatusLine:Normal")
                                                                   (vim.api.nvim_buf_set_name buf# :Packer)
                                                                   (values b# win# buf#)))}}})))))}


