local colors = require("colors")
local settings = require("settings")

local function shell_quote(value)
    return "'" .. string.gsub(value, "'", "'\\''") .. "'"
end

_G.icon_map = {}



local function config_dir()
    return os.getenv("CONFIG_DIR") or ((os.getenv("HOME") or "") .. "/.config/sketchybar")
end

local home_dir = os.getenv("HOME") or ""
local icon_map_path = home_dir .. "/.config/icon_map.sh"

local workspace_cache_json = nil
local workspace_cache_ts = 0
local workspace_cache_ttl = 1

local function trim(value)
    return (value or ""):gsub("^%s+", ""):gsub("%s+$", "")
end
local function get_background_style(selected)
    if selected then
        return {
            color = colors.with_alpha(colors.selection, 0.75),
            border_color = colors.with_alpha(colors.selection, 0.75),
            corner_radius = 30,
            --            height = 21,
        }
    else
        return {
            color = colors.with_alpha(colors.background or colors.black, 0.45),
            border_color = colors.with_alpha(colors.background or colors.black, 0.25),
            corner_radius = 30,
            --           height = 31,
        }
    end
end


local function get_workspace_json(callback)
    --    local now = os.time()
    --    if workspace_cache_json and (now - workspace_cache_ts) < workspace_cache_ttl then
    --        callback(workspace_cache_json)
    --        return
    --    end
    --
    sbar.exec("rift-cli query workspaces 2>/dev/null", function(output)
        local data = output or {}

        callback(data)
    end)
end

local function icon_for_app(app_name, callback)
    if app_name == "" or app_name == nil then
        callback("")
        return
    end
    if _G.icon_map[app_name] then
        callback(_G.icon_map[app_name])
        return
    end
    local icon_cmd = "bash -lc "
        .. shell_quote('. "' ..
            icon_map_path .. '"; __icon_map ' .. shell_quote(app_name) .. '; printf "%s" "$icon_result"')

    sbar.exec(icon_cmd, function(output)
        _G.icon_map[app_name] = trim(output)
        callback(trim(output))
    end)
end

local function build_icon_strip(apps, callback)
    if #apps == 0 then
        callback("")
        return
    end

    local icons = {}
    local idx = 1

    local function next_icon()
        if idx > #apps then
            callback(table.concat(icons, " "))
            return
        end

        icon_for_app(apps[idx], function(icon)
            if icon ~= "" then
                table.insert(icons, icon)
            end
            idx = idx + 1
            next_icon()
        end)
    end

    next_icon()
end

local function update_space_item(space_name, item_name, sender, space_id)
    if sender == "mouse.clicked" then
        print("Switching to workspace " .. space_name .. " with ID " .. tostring(space_id))
        sbar.exec("rift-cli execute workspace switch " .. shell_quote(space_id - 1))
        return
    end

    if sender == "mouse.entered" or sender == "mouse.exited" then
        return
    end

    get_workspace_json(function(workspace_json)
        if type(workspace_json) ~= "table" then
            return
        end

        local apps = {}
        local focused_workspace = ""

        for _, workspace in ipairs(workspace_json) do
            if type(workspace) == "table" then
                if workspace.is_active == true and type(workspace.name) == "string" then
                    focused_workspace = workspace.name
                end

                if workspace.name == space_name and type(workspace.windows) == "table" then
                    local seen = {}
                    for _, window in ipairs(workspace.windows) do
                        if type(window) == "table" and type(window.app_name) == "string" and not seen[window.app_name] then
                            seen[window.app_name] = true
                            table.insert(apps, window.app_name)
                        end
                    end
                end
            end
        end

        build_icon_strip(apps, function(icon_strip)
            local selected = focused_workspace == space_name
            local has_apps = icon_strip ~= ""

            sbar.set(item_name, {
                background = get_background_style(selected),
                icon = {
                    string = has_apps and space_name or space_name,
                    drawing = not has_apps,
                },
                label = {
                    string = has_apps and (" " .. icon_strip) or "",
                    drawing = has_apps,
                    border_color = colors.foreground,
                    padding_left = 1,
                    border_width = 1,
                },
            })
        end)
    end)
end

local dummy_space = {
    icon = {
        padding_left = 7,
        padding_right = -5,
        font = { family = settings.font.text, style = settings.font.style_map["Regular"], size = 11.0 },
        drawing = true,
    },
    label = {
        padding_right = 10,
        padding_left = 7,
        font = "sketchybar-app-font:Regular:16.0",
        drawing = false,
    },
    padding_right = 0,
    border_color = colors.selection,
    background = {
        color = colors.selection,
        border_color = colors.selection,
        drawing = true,
        corner_radius = 20,
        height = 21,
    },
}


local function sanitize_space_id(name)
    return name:gsub(" ", "__")
end

local function add_space_items(space_names)
    local item_names = {}

    for space_id, space_name in ipairs(space_names) do
        local item_name = "space." .. space_id

        local space = sbar.add("item", item_name, {
            icon = {
                string = space_name,
                padding_left = dummy_space.icon.padding_left,
                padding_right = dummy_space.icon.padding_right,
                font = dummy_space.icon.font,
                drawing = dummy_space.icon.drawing,
            },
            position = "left",
            label = dummy_space.label,
            padding_right = dummy_space.padding_right,
            border_color = dummy_space.border_color,
            background = dummy_space.background,
            drawing = true,
        })

        table.insert(item_names, item_name)

        space:subscribe(
            { "mouse.clicked", "mouse.exited", "rift_workspace_changed", "rift_windows_changed" },
            function(env)
                local sender = ""
                if type(env) == "table" then
                    sender = env.SENDER or env.sender or ""
                end
                update_space_item(space_name, item_name, sender, space_id)
            end)

        --        space:subscribe({ "mouse.entered", "mouse.exited" }, function(env)
        --            if type(env) ~= "table" then
        --                return
        --            end
        --            if env.sender == "mouse.exited" then
        --                sbar.set(item_name, {
        --                    popup = {
        --                        drawing = false,
        --                    }
        --                })
        --                return
        --            end
        --            sbar.set(item_name, {
        --                popup = {
        --                    drawing = true,
        --                    background = {
        --                        color = colors.with_alpha(colors.selection, 0.75),
        --                        border_color = colors.with_alpha(colors.selection, 0.75),
        --                        corner_radius = 10,
        --                        padding_left = 5,
        --                        padding_right = 5,
        --                    },
        --                },
        --            })
        --        end)

        update_space_item(space_name, item_name, "", space_id)
    end

    if #item_names > 0 then
        sbar.add("bracket", "spaces", item_names, {
            background = {
                color = colors.with_alpha(colors.yellow, 0.25),
                corner_radius = 20,
                border_color = colors.with_alpha(colors.yellow, 0.25),
                border_width = 1,
                height = 31,
            },
            padding_left = 0,
            padding_right = 0,
        })

        _G.last_space_item = item_names[#item_names]
        if _G.reorder_left_items then
            _G.reorder_left_items(_G.last_space_item)
        end
    end
end

sbar.exec("rift-cli query workspaces 2>/dev/null | jq -r '.[].name'", function(output)
    local names = {}
    for line in output:gmatch("[^\r\n]+") do
        if line ~= "" then
            table.insert(names, line)
        end
    end
    add_space_items(names)
end)

sbar.subscribe({ "rift_workspace_changed", "front_app_switched" }, function()
    if _G.reorder_left_items then
        _G.reorder_left_items(_G.last_space_item)
    end
end)
