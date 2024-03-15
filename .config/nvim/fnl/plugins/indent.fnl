(module plugins.indent
  {require {ibl ibl}})

(set vim.opt.list true)
;(vim.opt.listchars:append "space:⋅")
(vim.opt.listchars:append "eol:↴")
(ibl.setup {
             :scope {:enabled true
                     :show_end false
                     :exclude {:language [:packer :dashboard :telescope]}}})
