local _2afile_2a = "/home/auscyber/.config/nvim/fnl/core.fnl"
local _0_0
do
  local name_0_ = "core"
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
  module_0_["aniseed/local-fns"] = ((module_0_)["aniseed/local-fns"] or {})
  package.loaded[name_0_] = module_0_
  _0_0 = module_0_
end
local autoload = (require("aniseed.autoload")).autoload
local function _1_(...)
  local ok_3f_0_, val_0_ = nil, nil
  local function _1_()
    return {require("aniseed.nvim")}
  end
  ok_3f_0_, val_0_ = pcall(_1_)
  if ok_3f_0_ then
    _0_0["aniseed/local-fns"] = {["require-macros"] = {macros = true}, require = {nvim = "aniseed.nvim"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _1_(...)
local nvim = _local_0_[1]
local _2amodule_2a = _0_0
local _2amodule_name_2a = "core"
do local _ = ({nil, _0_0, nil, {{nil}, nil, nil, nil}})[2] end
local bo = nvim.bo
local wo = nvim.o
local o = nvim.o
o.mouse = "a"
o.guifont = "Hasklug Nerd Font:12"
o.termguicolors = true
nvim.ex.colorscheme("pink_ocean")
bo.tabstop = 4
bo.shiftwidth = 4
bo.expandtab = true
o.tabstop = 4
o.shiftwidth = 4
o.hidden = true
o.updatetime = 300
o.signcolumn = "yes"
wo.rnu = true
wo.nu = true
nvim.command("set rnu nu")
local function switch_fullscreen()
  local nvim0 = require("aniseed.nvim")
  if vim.g.neovide_fullscreen then
    return nvim0.command("let g:neovide_fullscreen=v:false")
  else
    return nvim0.command("let g:neovide_fullscreen=v:true")
  end
end
nvim.ex.nmap("<F11>", "<cmd> lua require 'core'.switch_fullscreen() <Cr>")
return {switch_fullscreen = switch_fullscreen}