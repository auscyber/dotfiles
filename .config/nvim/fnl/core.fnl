(module core
  {require {nvim aniseed.nvim}
   require-macros [macros]})
(local bo nvim.bo)
(local wo nvim.o)
(local o nvim.o)
(set o.mouse "a")
(set o.termguicolors true)
(_: colorscheme :pink_ocean)

