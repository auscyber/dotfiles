(module plugins.gitsigns
  {require {gitsigns gitsigns}})
(gitsigns.setup {:signs  {
                          :add  {:hl  "DiffAdd" :text  "│" :numhl  "GitSignsAddNr"}
                          :change  {:hl  "DiffChange" :text  "│" :numhl  "GitSignsChangeNr"}
                          :changedelete  {:hl  "DiffChange" :text  "~" :numhl  "GitSignsChangeNr"}
                          :delete  {:hl  "DiffDelete" :text  "_" :numhl  "GitSignsDeleteNr"}
                          :topdelete  {:hl  "DiffDelete" :text  "‾" :numhl  "GitSignsDeleteNr"}}})
