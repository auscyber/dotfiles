(module plugins.devicons
  {require {nvim-web-devicons nvim-web-devicons}})
(nvim-web-devicons.setup
  {:default false})
(nvim-web-devicons.set_default_icon "" :#2f2f2f)
(nvim-web-devicons.set_icon {:fnl
                             {:icon ""
                              :color :#22aa00
                              :name :Fennel}})
(nvim-web-devicons.set_icon {:.links.toml
                             {:icon ""
                              :color :#2f2f2f
                              :name "DotfileLinks"}})

