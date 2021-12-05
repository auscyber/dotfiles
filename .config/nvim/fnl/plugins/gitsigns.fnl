(module plugins.gitsigns
  {require {gitsigns gitsigns}})
(gitsigns.setup {:signs  {
                          :add  {:hl  "GitSignsDiffAdd" :text  "│" :numhl  "GitSignsAddNr"}
                          :change  {:hl  "GitSignsDiffChange" :text  "│" :numhl  "GitSignsChangeNr"}
                          :changedelete  {:hl  "GitSignsDiffChange" :text  "~" :numhl  "GitSignsChangeNr"}
                          :delete  {:hl  "GitSignsDiffDelete" :text  "_" :numhl  "GitSignsDeleteNr"}
                          :topdelete  {:hl  "GitSignsDiffDelete" :text  "‾" :numhl  "GitSignsDeleteNr"}}})
