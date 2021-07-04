local _2afile_2a = "/home/auscyber/.config/nvim/fnl/plugins/animate.fnl"
local _0_
do
  local name_0_ = "plugins.animate"
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
    return {require("aniseed.nvim"), require("utils")}
  end
  ok_3f_0_, val_0_ = pcall(_2_)
  if ok_3f_0_ then
    _0_["aniseed/local-fns"] = {["require-macros"] = {macros = true}, require = {nvim = "aniseed.nvim", utils = "utils"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _2_(...)
local nvim = _local_0_[1]
local utils = _local_0_[2]
local _2amodule_2a = _0_
local _2amodule_name_2a = "plugins.animate"
do local _ = ({nil, _0_, nil, {{nil}, nil, nil, nil}})[2] end
local function animate_fn(sub_f, ...)
  return vim.fn[("animate#" .. sub_f)](...)
end
local function window_percent_width(...)
  return animate_fn("window_percent_width", ...)
end
local function animated_split()
  nvim.command("new | wincmd L | vertical resize 0 ")
  return window_percent_width(0.5)
end
return {animated_split = animated_split}