(module plugins.gitsigns
  {require {gitsigns gitsigns}})
(gitsigns.setup {:signs  {
                          :add  {:text  "│"}
                          :change  { :text  "│"}
                          :changedelete  { :text  "~"}
                          :delete  { :text  "_"}
                          :topdelete  { :text  "‾"}}})
