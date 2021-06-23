local _2afile_2a = "/home/auscyber/.config/nvim/fnl/colors.fnl"
local _0_
do
  local name_0_ = "colors"
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
    return {(_0_)["aniseed/locals"].cyan, (_0_)["aniseed/locals"].dark_cyan, (_0_)["aniseed/locals"].grey, (_0_)["aniseed/locals"].light_red, (_0_)["aniseed/locals"].pink, (_0_)["aniseed/locals"].purple, (_0_)["aniseed/locals"].red, (_0_)["aniseed/locals"].white}
  end
  ok_3f_0_, val_0_ = pcall(_2_)
  if ok_3f_0_ then
    _0_["aniseed/local-fns"] = {}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _2_(...)
local cyan = _local_0_[1]
local dark_cyan = _local_0_[2]
local grey = _local_0_[3]
local light_red = _local_0_[4]
local pink = _local_0_[5]
local purple = _local_0_[6]
local red = _local_0_[7]
local white = _local_0_[8]
local _2amodule_2a = _0_
local _2amodule_name_2a = "colors"
do local _ = ({nil, _0_, nil, {{}, nil, nil, nil}})[2] end
local white0
do
  local v_0_
  do
    local v_0_0 = "#FFFFFF"
    _0_["white"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["white"] = v_0_
  white0 = v_0_
end
local pink0
do
  local v_0_
  do
    local v_0_0 = "#ffd1dc"
    _0_["pink"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["pink"] = v_0_
  pink0 = v_0_
end
local light_red0
do
  local v_0_
  do
    local v_0_0 = "#ffa0a0"
    _0_["light_red"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["light_red"] = v_0_
  light_red0 = v_0_
end
local purple0
do
  local v_0_
  do
    local v_0_0 = "#A04668"
    _0_["purple"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["purple"] = v_0_
  purple0 = v_0_
end
local dark_cyan0
do
  local v_0_
  do
    local v_0_0 = "#738290"
    _0_["dark_cyan"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["dark_cyan"] = v_0_
  dark_cyan0 = v_0_
end
local grey0
do
  local v_0_
  do
    local v_0_0 = "#707078"
    _0_["grey"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["grey"] = v_0_
  grey0 = v_0_
end
local red0
do
  local v_0_
  do
    local v_0_0 = "#DB5461"
    _0_["red"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["red"] = v_0_
  red0 = v_0_
end
local cyan0
do
  local v_0_
  do
    local v_0_0 = "#8BB2C1"
    _0_["cyan"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["cyan"] = v_0_
  cyan0 = v_0_
end
return nil