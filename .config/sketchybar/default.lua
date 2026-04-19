local colors = require("colors")
local settings = require("settings")

sbar.default({
    padding_left = 5,
    padding_right = 5,
    icon = {
        font = {
            family = settings.font.text,
            style = settings.font.style_map["Regular"],
            size = 17.0,
        },
        color = colors.foreground,
        padding_left = 4,
        padding_right = 2,
    },
    label = {
        font = {
            family = settings.font.text,
            style = settings.font.style_map["Bold"],
            size = 14.0,
        },
        color = colors.foreground,
        padding_right = 2,
    },
})
