(module core
  {autoload {nvim aniseed.nvim}})
(local bo nvim.bo)
(local wo nvim.o)
(local o nvim.o)
(set o.mouse "a")
(set o.termguicolors true)
((. nvim :ex :colorscheme) "pink_ocean")

