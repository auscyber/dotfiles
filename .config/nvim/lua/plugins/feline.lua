local _2afile_2a = "/home/auscyber/.config/nvim/fnl/plugins/feline.fnl"
local _0_
do
  local name_0_ = "plugins.feline"
  local module_0_
  do
    local x_0_ = package.loaded[name_0_]
    if ("table" == type(x_0_)) then
      module_0_ = x_0_
    else
      module_0_ = {}
    end
  end
  module_0_["aniseed/module"] = name_0_
  module_0_["aniseed/locals"] = ((module_0_)["aniseed/locals"] or {})
  do end (module_0_)["aniseed/local-fns"] = ((module_0_)["aniseed/local-fns"] or {})
  do end (package.loaded)[name_0_] = module_0_
  _0_ = module_0_
end
local autoload
local function _1_(...)
  return (require("aniseed.autoload")).autoload(...)
end
autoload = _1_
local function _2_(...)
  local ok_3f_0_, val_0_ = nil, nil
  local function _2_()
    return {require("colors"), require("feline"), require("feline.providers.lsp"), require("feline.providers.vi_mode")}
  end
  ok_3f_0_, val_0_ = pcall(_2_)
  if ok_3f_0_ then
    _0_["aniseed/local-fns"] = {require = {colors = "colors", feline = "feline", lsp = "feline.providers.lsp", vi_mode_utils = "feline.providers.vi_mode"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _2_(...)
local colors = _local_0_[1]
local feline = _local_0_[2]
local lsp = _local_0_[3]
local vi_mode_utils = _local_0_[4]
local _2amodule_2a = _0_
local _2amodule_name_2a = "plugins.feline"
do local _ = ({nil, _0_, nil, {{}, nil, nil, nil}})[2] end
local b = vim.b
local fnn = vim.fn
local properties = {force_inactive = {bufnames = {}, buftypes = {}, filetypes = {}}}
local components = {left = {active = {}, inactive = {}}, mid = {active = {}, inactive = {}}, right = {active = {}, inactive = {}}}
properties.force_inactive.filetypes = {"NvimTree", "dbui", "packer", "startify", "fugitive", "fugitiveblame"}
properties.force_inactive.buftypes = {"terminal"}
components.left.active[1] = {hl = {bg = "NONE", fg = colors.cyan}, provider = " \238\130\182"}
local function _3_()
  local val = {bg = colors.cyan}
  val.name = vi_mode_utils.get_mode_highlight_name()
  val.fg = vi_mode_utils.get_mode_color()
  val.style = "bold"
  return val
end
local function _4_()
  return " \239\158\159 "
end
components.left.active[2] = {hl = _3_, provider = _4_, right_sep = {hl = {bg = colors.cyan}, str = " "}}
components.left.active[3] = {hl = {bg = colors.grey, fg = "white", style = "bold"}, left_sep = {{hl = {bg = colors.grey}, str = " "}}, provider = "file_info", right_sep = {" "}}
local function _5_()
  return (fnn.getfsize(fnn.expand("%:p")) > 0)
end
components.left.active[4] = {enabled = _5_, provider = "file_size", right_sep = {" ", {hl = {bg = "NONE", fg = "bg"}, str = "right_rounded"}}}
components.right.active[1] = {hl = {bg = colors.light_red, fg = colors.white, style = "bold"}, left_sep = {{hl = {bg = "NONE", fg = colors.light_red}, str = "left_rounded"}}, provider = "position", right_sep = {{hl = {bg = colors.light_red}, str = " "}}}
local function _6_()
  local val = {hl = {bg = "black", fg = "NONE"}}
  if b.gitsigns_status_dict then
    val.str = " "
  else
    val.str = ""
  end
  return val
end
components.right.active[2] = {hl = {bg = "black", fg = "white", style = "bold"}, provider = "git_branch", right_sep = _6_}
components.right.active[3] = {hl = {bg = "black", fg = "green"}, provider = "git_diff_added"}
components.right.active[4] = {hl = {bg = "black", fg = "orange"}, provider = "git_diff_changed"}
local function _7_()
  local val = {hl = {bg = "black", fg = "NONE"}}
  if b.gitsigns_status_dict then
    val.str = " "
  else
    val.str = ""
  end
  return val
end
components.right.active[5] = {hl = {bg = "black", fg = "red"}, provider = "git_diff_removed", right_sep = _7_}
components.right.active[6] = {hl = {style = "bold"}, left_sep = "  ", provider = "line_percentage", right_sep = " "}
components.right.active[7] = {hl = {fg = colors.light_red, style = "bold"}, provider = "scroll_bar"}
local function _8_()
  return lsp.diagnostics_exist("Error")
end
components.right.active[8] = {enabled = _8_, hl = {fg = "red"}, provider = "diagnostic_errors"}
local function _9_()
  return lsp.diagnostics_exist("Warning")
end
components.right.active[9] = {enabled = _9_, hl = {fg = "yellow"}, provider = "diagnostic_warnings"}
local function _10_()
  return lsp.diagnostics_exist("Hint")
end
components.right.active[10] = {enabled = _10_, hl = {fg = "cyan"}, provider = "diagnostic_hints"}
local function _11_()
  return lsp.diagnostics_exist("Information")
end
components.right.active[11] = {enabled = _11_, hl = {fg = "skyblue"}, provider = "diagnostic_info"}
components.right.active[12] = {provider = " ", right_sep = {hl = {bg = "NONE", fg = "bg"}, str = "right_rounded"}}
components.left.inactive[1] = {hl = {bg = colors.cyan, fg = "white"}, left_sep = {{hl = {bg = "NONE", fg = "NONE"}, str = " "}, "left_rounded", {hl = {bg = colors.cyan, fg = "NONE"}, str = " "}}, provider = "file_type", right_sep = {hl = {bg = colors.cyan}, str = " "}}
components.left.inactive[2] = {hl = {bg = colors.dark_cyan, fg = "white", style = "bold"}, left_sep = {hl = {bg = colors.dark_cyan}, str = " "}, provider = "file_info", right_sep = {{hl = {bg = colors.dark_cyan, fg = "NONE"}, str = " "}, {hl = {bg = "NONE", fg = colors.dark_cyan}, str = "right_rounded"}}}
local colors0 = {black = "#1B1B1B", cyan = "#009090", green = "#60A040", magenta = "#C26BDB", oceanblue = "#0066cc", orange = "#FF9000", red = "#D10000", skyblue = "#50B0F0", violet = "#9E93E8", white = "#FFFFFF", yellow = "#E1E120"}
local separators = {block = "\226\150\136", circle = "\226\151\143", left = "\238\130\179", left_filled = "\238\130\178", left_rounded = "\238\130\182", left_rounded_thin = "\238\130\183", right = "\238\130\177", right_filled = "\238\130\176", right_rounded = "\238\130\180", right_rounded_thin = "\238\130\181", slant_left = "\238\130\186", slant_left_2 = "\238\130\190", slant_left_2_thin = "\238\130\191", slant_left_thin = "\238\130\187", slant_right = "\238\130\184", slant_right_2 = "\238\130\188", slant_right_2_thin = "\238\130\189", slant_right_thin = "\238\130\185", vertical_bar = "\226\148\131", vertical_bar_thin = "\226\148\130"}
local vi_mode_colors = {BLOCK = colors0.blue, COMMAND = colors0.cyan, ENTER = "cyan", INSERT = colors0.dark_cyan, MORE = "cyan", NONE = "yellow", NORMAL = colors0.white, OP = "green", REPLACE = colors0.red, SELECT = "orange", SHELL = "green", TERM = "green", VISUAL = colors0.blue, [{"V-REPLACE"}] = "violet"}
return feline.setup({colors = colors0, components = components, default_bg = colors0.purple, default_fg = "#D0D0D0", properties = properties, separators = separators, vi_mode_colors = vi_mode_colors})