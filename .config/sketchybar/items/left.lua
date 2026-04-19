local colors = require("colors")
local icons = require("icons")

local function config_dir()
    return os.getenv("CONFIG_DIR") or ((os.getenv("HOME") or "") .. "/.config/sketchybar")
end

local plugin_dir = config_dir() .. "/plugins"

local function reorder_left_items(last_space)
    if not last_space or last_space == "" then
        return
    end
    sbar.exec("sketchybar --move chevron after " .. last_space)
    sbar.exec("sketchybar --move front_app after chevron")
end

_G.reorder_left_items = reorder_left_items

sbar.add("item", "chevron", {
    position = "left",
    icon = { string = icons.chevron },
    label = { drawing = false },
    padding_left = 0,
    padding_right = 0,
    click_script = "sketchybar --trigger swap_menus_and_spaces",
})

local front_app = sbar.add("item", "front_app", {
    position = "left",
    icon = { drawing = false },
    script = plugin_dir .. "/front_app.sh",
    click_script = "sketchybar --trigger swap_menus_and_spaces",
})
front_app:subscribe("front_app_switched", function(env)
    if _G.last_space_item then
        reorder_left_items(_G.last_space_item)
    end

    front_app:set({
        label = env.INFO
    })
end)



if _G.last_space_item then
    reorder_left_items(_G.last_space_item)
end
