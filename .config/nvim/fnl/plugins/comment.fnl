(module plugins.comment
  {require {Comment Comment}})
(Comment.setup
  {:pre_hook
   (fn [ctx])})

