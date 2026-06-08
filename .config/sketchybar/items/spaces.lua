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

local function trim(value)
	return (value or ""):gsub("^%s+", ""):gsub("%s+$", "")
end

local function get_background_style(selected)
	if selected then
		return {
			color = colors.with_alpha(colors.selection, 0.75),
			border_color = colors.with_alpha(colors.selection, 0.75),
			corner_radius = 30,
		}
	else
		return {
			color = colors.with_alpha(colors.background or colors.black, 0.45),
			border_color = colors.with_alpha(colors.background or colors.black, 0.25),
			corner_radius = 30,
		}
	end
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
		.. shell_quote(
			'. "' .. icon_map_path .. '"; __icon_map ' .. shell_quote(app_name) .. '; printf "%s" "$icon_result"'
		)

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

-- display_state[arrangement_id] = {
--   space_id      = number,       -- current active macOS space on this display
--   workspaces    = { name,... }, -- ordered workspace names
--   item_names    = { ... },      -- sketchybar item names matching workspaces
--   last_item     = "space.X.N",  -- last item (for reorder hook)
-- }
local display_state = {}

local function update_items_for_display(arrangement_id)
	local state = display_state[arrangement_id]
	if not state or not state.space_id then
		return
	end

	sbar.exec("rift-cli query workspaces --space-id " .. state.space_id .. " 2>/dev/null", function(workspaces)
		if type(workspaces) ~= "table" then
			return
		end

		local focused = ""
		for _, ws in ipairs(workspaces) do
			if type(ws) == "table" and ws.is_active == true and type(ws.name) == "string" then
				focused = ws.name
			end
		end

		for i, ws in ipairs(workspaces) do
			if type(ws) == "table" and type(ws.name) == "string" then
				local item_name = state.item_names[i]
				if item_name then
					local apps = {}
					local seen = {}
					if type(ws.windows) == "table" then
						for _, window in ipairs(ws.windows) do
							if
								type(window) == "table"
								and type(window.app_name) == "string"
								and not seen[window.app_name]
							then
								seen[window.app_name] = true
								table.insert(apps, window.app_name)
							end
						end
					end

					local space_name = ws.name
					build_icon_strip(apps, function(icon_strip)
						local selected = focused == space_name
						local has_apps = icon_strip ~= ""

						sbar.set(item_name, {
							background = get_background_style(selected),
							icon = {
								string = space_name,
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
				end
			end
		end
	end)
end

local function add_items_for_display(arrangement_id, workspace_names)
	local state = display_state[arrangement_id]
	state.workspaces = workspace_names
	state.item_names = {}

	for index, space_name in ipairs(workspace_names) do
		local item_name = "space." .. arrangement_id .. "." .. index

		local space = sbar.add("item", item_name, {
			display = tostring(arrangement_id),
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

		table.insert(state.item_names, item_name)

		local workspace_index = index - 1
		space:subscribe(
			{ "mouse.clicked", "mouse.exited", "rift_workspace_changed", "rift_windows_changed", "space_change" },
			function(env)
				local sender = ""
				if type(env) == "table" then
					sender = env.SENDER or env.sender or ""
				end

				if sender == "mouse.clicked" then
					print("Switching to workspace " .. space_name .. " (index " .. workspace_index .. ")")
					sbar.exec("rift-cli execute workspace switch " .. shell_quote(workspace_index))
					return
				end

				if sender == "mouse.entered" or sender == "mouse.exited" then
					return
				end

				update_items_for_display(arrangement_id)
			end
		)
	end

	local bracket_name = "spaces." .. arrangement_id
	sbar.add("bracket", bracket_name, state.item_names, {
		display = tostring(arrangement_id),
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

	state.last_item = state.item_names[#state.item_names]
	_G.last_space_item = state.last_item
	if _G.reorder_left_items then
		_G.reorder_left_items(state.last_item)
	end

	update_items_for_display(arrangement_id)
end

local function refresh_display_spaces()
	sbar.exec("sketchybar -m --query displays", function(sbar_displays)
		if type(sbar_displays) ~= "table" then
			return
		end
		sbar.exec("rift-cli query displays 2>/dev/null", function(rift_displays)
			if type(rift_displays) ~= "table" then
				return
			end

			for _, sd in ipairs(sbar_displays) do
				local arrangement_id = sd["arrangement-id"]
				local uuid = sd.UUID
				if arrangement_id and uuid then
					for _, rd in ipairs(rift_displays) do
						if type(rd) == "table" and rd.uuid == uuid then
							local active_space = rd.space or (rd.active_space_ids and rd.active_space_ids[1])
							if active_space then
								local existing = display_state[arrangement_id]
								if not existing then
									display_state[arrangement_id] = { space_id = active_space }
									sbar.exec(
										"rift-cli query workspaces --space-id "
											.. active_space
											.. " 2>/dev/null | jq -r '.[].name'",
										function(output)
											local names = {}
											for line in (output or ""):gmatch("[^\r\n]+") do
												if line ~= "" then
													table.insert(names, line)
												end
											end
											if #names > 0 then
												add_items_for_display(arrangement_id, names)
											end
										end
									)
								else
									existing.space_id = active_space
									update_items_for_display(arrangement_id)
								end
							end
							break
						end
					end
				end
			end
		end)
	end)
end

refresh_display_spaces()

sbar.subscribe({ "rift_workspace_changed", "front_app_switched" }, function()
	if _G.last_space_item then
		if _G.reorder_left_items then
			_G.reorder_left_items(_G.last_space_item)
		end
	end
end)

sbar.subscribe({ "space_change", "display_change" }, function()
	refresh_display_spaces()
end)
