local colors = require("colors")
local icons = require("icons")
local settings = require("settings")

local function config_dir()
	return os.getenv("CONFIG_DIR") or ((os.getenv("HOME") or "") .. "/.config/sketchybar")
end

local plugin_dir = config_dir() .. "/plugins"
local helpers_dir = config_dir() .. "/helpers"

local function shell_quote(value)
	return "'" .. value:gsub("'", "'\\''") .. "'"
end

-- Clicking a mirrored menu-bar item has to press the REAL item behind it --
-- the bar only draws a picture of it, so a click on the picture does nothing
-- on its own.
--
-- `--press <item>` is the bar's own way of doing that and takes the alias
-- item's name. helpers/menus/bin/menus (a SketchyBar-era helper binary, built
-- from helpers/menus/menus.c) did the same job by driving the Accessibility
-- API itself from outside; it is left in the tree for SketchyBar, which has no
-- `--press`.
local function press_script(alias_name)
	return "sketchybar --press " .. shell_quote(alias_name)
end

local function split_alias_metadata(alias_name)
	local owner, name, pid = alias_name:match("^([^,]+),(.+),(%d+)$")
	if owner and name and pid then
		return owner .. "," .. name, alias_name, pid
	end

	return alias_name, alias_name, pid
end

local function parse_alias_parts(alias_name)
	local owner, name, pid = alias_name:match("^([^,]+),(.+),(%d+)$")
	if owner and name and pid then
		return owner, name, pid
	end

	owner, name = alias_name:match("^([^,]+),(.+)$")
	if owner and name then
		return owner, name, nil
	end

	return nil, nil, nil
end

local control_centre_focus_modes_click_script = nil

-- macOS hosts third-party menu extras in the Control Centre process, so the
-- alias owner is "Control Centre" and not the app itself -- check a name
-- against `sketchybar --query default_menu_items` rather than guessing it, or
-- the alias resolves to nothing and the real item falls through to the
-- overflow popup instead.
local amphetamine = "Control Centre,Amphetamine"

sbar.add("alias", amphetamine, {
	position = "right",
	click_script = press_script(amphetamine),
})

sbar.add("alias", "Control Centre,FocusModes", {
	position = "right",
	padding_left = -15,
	padding_right = -5,
	alias = { color = colors.yellow },
	click_script = control_centre_focus_modes_click_script or press_script("Control Centre,FocusModes"),
})

sbar.add("item", "clock", {
	position = "right",
	update_freq = 10,
	icon = { string = icons.clock },
	script = plugin_dir .. "/clock.sh",
	padding_right = 10,
})

sbar.add("item", "volume", {
	position = "right",
	script = plugin_dir .. "/volume.sh",
	click_script = "sketchybar -m --set $NAME popup.drawing=toggle",
	popup = { background = { color = 0x40ffffff } },
})

sbar.add("item", "power", {
	position = "right",
	update_freq = 30,
	script = plugin_dir .. "/power.sh",
})

sbar.add("item", "battery", {
	position = "right",
	update_freq = 120,
	label = { drawing = false },
	script = plugin_dir .. "/battery.sh",
})

-- Fantastical will be added dynamically from overflow aliases

local overflow = sbar.add("item", "overflow", {
	position = "right",
	icon = {
		string = "...",
		font = {
			family = settings.font.text,
			style = settings.font.style_map["Bold"],
			size = 14.0,
		},
		y_offset = 0,
	},
	label = { drawing = false },
	y_offset = 0,
	padding_left = 6,
	padding_right = 6,
	--    click_script = "sketchybar --set $NAME popup.drawing=toggle",
	popup = {
		align = "right",
		horizontal = true,
		height = 0,
		background = {
			color = 0xcc000000,
			border_color = colors.yellow,
			border_width = 1,
			corner_radius = 8,
		},
	},
})

print("Overflow menu item created")
overflow:subscribe("mouse.clicked", function(env)
	print("Overflow clicked, toggling menu")
	print("Current popup state: " .. tostring(overflow:query().popup.drawing))
	overflow:set({
		popup = { drawing = overflow:query().popup.drawing == "on" and "off" or "on" },
	})
end)
print("Subscribed to overflow click")

local extra_mapping = {}

-- Anything pinned above, so it is never drawn a second time inside the
-- overflow popup. Both spellings of Amphetamine: the owner moved to Control
-- Centre, and an entry that is already gone costs nothing.
local alias_to_ignore = {
	["Control Centre,FocusModes"] = true,
	["Amphetamine,Amphetamine"] = true,
	[amphetamine] = true,
	["Fantastical Helper,Fantastical"] = true,
	["Control Centre,Clock"] = true,
	["Control Centre,BentoBox-0"] = true,
	["Control Centre,BentoBox-1"] = true,
	["LinkedNotesUIService,Window"] = true,
}

local function add_overflow_aliases(alias_list, owner_pids)
	local seen = {}
	for _, alias_name in ipairs(alias_list) do
		local display_alias, click_alias = split_alias_metadata(alias_name)
		local owner, name, pid = parse_alias_parts(alias_name)

		if seen[display_alias] or alias_to_ignore[display_alias] then
			goto continue
		end
		seen[display_alias] = true

		if owner and name and not pid then
			local resolved_pid = owner_pids and owner_pids[owner]
			if resolved_pid then
				click_alias = owner .. "," .. name .. "," .. resolved_pid
			end
		end

		local alias = sbar.add("alias", display_alias, {
			position = "popup.overflow",
			padding_left = -2,
			padding_right = -2,
			label = { drawing = false, string = display_alias },
			click_script = press_script(display_alias),
		})
		alias:subscribe("mouse.clicked", function()
			overflow:set({ popup = { drawing = "off" } })
		end)

		::continue::
	end
end

local function parse_alias_list_response(items)
	if type(items) == "table" then
		return items
	end

	if type(items) ~= "string" then
		return {}
	end

	local list = {}
	for value in items:gmatch('"([^"]+)"') do
		table.insert(list, value)
	end
	if #list == 0 then
		for line in items:gmatch("[^\r\n]+") do
			if line ~= "" then
				table.insert(list, line)
			end
		end
	end

	return list
end

-- The right-hand cluster, in the order it is meant to be drawn.
--
-- Not left to creation order: an item keeps the ordering index it was first
-- given, so anything created somewhere else first -- a pinned alias that a
-- previous generation had put in the overflow popup, say -- comes back in that
-- old place on the next reload rather than where this file adds it. Naming the
-- whole group makes the order a property of the config instead of a property
-- of the order things happened to be created in.
local right_order = {
	amphetamine,
	"Control Centre,FocusModes",
	"clock",
	"volume",
	"power",
	"battery",
	"overflow",
}

local function reorder_right_items()
	local quoted = {}
	for _, name in ipairs(right_order) do
		table.insert(quoted, shell_quote(name))
	end
	sbar.exec("sketchybar --reorder " .. table.concat(quoted, " "))
end

local function load_overflow_aliases()
	sbar.exec("sketchybar --query default_menu_items", function(fallback_items)
		local items = parse_alias_list_response(fallback_items)
		local owner_pids = {}

		for _, alias_name in ipairs(items) do
			local owner, _, pid = parse_alias_parts(alias_name)
			if owner and pid and not owner_pids[owner] then
				owner_pids[owner] = pid
			end
		end

		add_overflow_aliases(items, owner_pids)

		-- After the popup aliases, not before: adding them is what can pull a
		-- pinned item out of place, so the order is asserted once everything
		-- that touches it has run.
		reorder_right_items()
	end)
end

load_overflow_aliases()

sbar.add("slider", "volume_options", 100, {
	position = "popup.volume",
	slider = {
		highlight_color = colors.yellow,
		background = { height = 6 },
	},
	script = 'test -z "$PERCENTAGE" || osascript -e "set Volume "$(echo "scale=10; $PERCENTAGE / 10.0" | bc )""',
})

sbar.add("bracket", "all_utils", {
	"clock",
	"volume",
	"power",
	"battery",
	"Control Centre,FocusModes",
	amphetamine,
}, {
	background = {
		color = colors.yellow,
		height = 30,
		corner_radius = 20,
	},
})

sbar.add("bracket", "utils", {
	"power",
	"battery",
	"volume",
}, {
	background = {
		color = colors.yellow,
		height = 30,
		corner_radius = 15,
	},
})
