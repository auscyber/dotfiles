local _2afile_2a = "/home/auscyber/.config/nvim/fnl/plugins/galaxyline.fnl"
local _0_
do
  local name_0_ = "plugins.galaxyline"
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
    return {autoload("colors"), require("galaxyline"), require("aniseed.nvim")}
  end
  ok_3f_0_, val_0_ = pcall(_2_)
  if ok_3f_0_ then
    _0_["aniseed/local-fns"] = {["require-macros"] = {macros = true}, autoload = {color = "colors"}, require = {gl = "galaxyline", nvim = "aniseed.nvim"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _2_(...)
local color = _local_0_[1]
local gl = _local_0_[2]
local nvim = _local_0_[3]
local _2amodule_2a = _0_
local _2amodule_name_2a = "plugins.galaxyline"
do local _ = ({nil, _0_, nil, {{nil}, nil, nil, nil}})[2] end
local gls = gl.section
gl.short_line_list = {"LuaTree", "vista", "dbui"}
local colors = {bg = "#282c34", blue = "#0087d7", cyan = "#008080", darkblue = "#081633", green = "#afd700", grey = "#c0c0c0", magenta = "#d16d9e", orange = "#FF8800", purple = "#5d4d7a", red = "#ec5f67", yellow = "#fabd2f"}
local buffer_not_empty
local function _3_()
  if (1 ~= vim.fn.empty(vim.fn.expand("%:t"))) then
    return true
  else
    return false
  end
end
buffer_not_empty = _3_
local function _4_()
  return "  \238\130\182"
end
gls.left[1] = {FirstElement = {highlight = {color.light_red}, provider = _4_}}
local function _5_()
  local mode_color = {R = colors.violet, Rv = colors.violet, S = colors.orange, V = colors.blue, ["\19"] = colors.orange, ["\22"] = colors.blue, [{"!"}] = colors.red, [{"r?"}] = colors.cyan, c = color.purple, ce = colors.red, cv = colors.red, i = colors.cyan, ic = colors.yellow, n = color.white, no = colors.red, r = colors.cyan, rm = colors.cyan, s = colors.orange, t = colors.red, v = colors.blue}
  nvim.ex.hi(("GalaxyViMode guifg=" .. mode_color[vim.fn.mode()]))
  return "\239\158\159 "
end
gls.left[2] = {ViMode = {highlight = {color.grey, color.light_red, "bold"}, provider = _5_, separator = "  ", separator_highlight = {nil, color.purple}}}
gls.left[3] = {FFileIcon = {highlight = {"NONE", color.purple}, provider = "FileIcon", separator = "\238\130\180 ", separator_highlight = {color.purple}}}
local function _6_()
  return "  \238\130\182"
end
gls.left[4] = {FileNameSepFirst = {provider = _6_}}
gls.left[5] = {FileName = {provider = "FileName"}}
local function _7_()
  return vim.lsp.buf_is_attached(0)
end
gls.right[1] = {LspStatus = {condition = _7_, provider = "GetLspClient"}}
local function _8_()
  return "\238\130\182"
end
gls.short_line_left[1] = {BufferFirstElement = {highlight = {color.red}, provider = _8_}}
gls.short_line_left[2] = {BufferFileName = {hightlight = {nil, color.red}, provider = "FileName"}}
gls.short_line_left[3] = {BufferType = {highlight = {nil, color.red, "bold"}, provider = "FileTypeName", separator = "\238\130\180", separator_highlight = {color.red, nil}}}
return nil