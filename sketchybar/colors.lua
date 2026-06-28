local function parse_colors()
	local colors = {}

	local int_colors = require("internal_colors")

	for key, value in pairs(int_colors) do
		colors[key] = tonumber("0x" .. "ff" .. value)
	end

	return colors
end

local colors = parse_colors()

local fallback = {
	foreground = 0xffe6e8ef,
	background = 0xff101418,
	yellow = 0xfff2c14e,
	selection = 0xff2b3544,
	black = 0xff000000,
	white = 0xffffffff,
}

for key, value in pairs(fallback) do
	if type(colors[key]) ~= "number" then
		colors[key] = value
	end
end

colors.transparent = 0x00000000

function colors.with_alpha(color, alpha)
	if type(color) ~= "number" or type(alpha) ~= "number" then
		return color
	end
	if alpha > 1.0 or alpha < 0.0 then
		return color
	end
	local base = color & 0x00ffffff
	local a = math.floor(alpha * 255.0) & 0xff
	return (a << 24) | base
end

return colors
