local wezterm = require("wezterm")

local fennel = require("fennel")
local main_config = wezterm.config_dir .. "/config.fnl"
wezterm.add_to_config_reload_watch_list(main_config)
fennel.path = (wezterm.config_dir .. "/?.fnl;" .. fennel.path)
--package.path = (wezterm.config_dir .. "/?.lua;" .. package.path)
table.insert(package.loaders or package.searchers, fennel.searcher)

local config = require("config")
print(config)

return config
