(module plugins.indent
  {require {indent_blankline indent_blankline}})

(set vim.opt.list true)
;(vim.opt.listchars:append "space:⋅")
(vim.opt.listchars:append "eol:↴")
(indent_blankline.setup
  {:show_end_of_line true :show_current_context true :show_current_context_start true :filetype_exclude [:packer :dashboard :telescope]})
