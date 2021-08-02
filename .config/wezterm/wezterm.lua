local wezterm = require'wezterm'
local fennel = require'fennel'
--watch_path = wezterm.config_dir .. "config.fnl"
local value = wezterm.config_dir .. "/config.fnl"
--print("string" .. value)
wezterm.set_config_reload_watch_list(value)
fennel.path = (wezterm.config_dir .. "/?.fnl;" .. fennel.path)
--package.path = (wezterm.config_dir .. "/?.lua;" .. package.path)
table.insert(package.loaders or package.searchers, fennel.searcher)
return require('config')
