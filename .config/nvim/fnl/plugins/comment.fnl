(module plugins.comment {require {Comment Comment}})

(Comment.setup {:pre_hook (fn [ctx]
                            (let [U (require :Comment.utils)
                                  ctype (or (and (= ctx.ctype U.ctype.line)
                                                 :__default)
                                            :__multiline)
                                  location (if (= ctx.ctype U.ctype.block)
                                               ((. (require :ts_context_commentstring.utils)
                                                   :get_cursor_location))
                                               (if (or (= ctx.cmotion
                                                          U.cmotion.v)
                                                       (= ctx.cmotion
                                                          U.cmotion.V))
                                                   ((. (require :ts_context_commentstring.utils)
                                                       :get_visual_start_location))))]
                              ((. (require :ts_context_commentstring.internal)
                                  :calculate_commentstring) {:key ctype
                                                             : location})))})
