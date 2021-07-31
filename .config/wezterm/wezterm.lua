local wezterm = require'wezterm'
local fennel = require'fennel'
fennel.path = (wezterm.config_dir .. "/?.fnl;" .. fennel.path)
table.insert(package.loaders or package.searchers, fennel.searcher)
local config = require('config')
return config
