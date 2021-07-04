local _2afile_2a = "/home/auscyber/.config/nvim/fnl/utils.fnl"
local _0_
do
  local name_0_ = "utils"
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
    return {autoload("aniseed.core"), autoload("aniseed.fennel"), autoload("aniseed.nvim"), autoload("aniseed.string"), (_0_)["aniseed/locals"].all, (_0_)["aniseed/locals"]["buffer-content"], (_0_)["aniseed/locals"].comp, (_0_)["aniseed/locals"]["contains?"], (_0_)["aniseed/locals"]["del-keymap"], (_0_)["aniseed/locals"]["filter-table"], (_0_)["aniseed/locals"]["find-map"], (_0_)["aniseed/locals"]["find-where"], (_0_)["aniseed/locals"].highlight, (_0_)["aniseed/locals"]["highlight-add"], (_0_)["aniseed/locals"]["keep-if"], (_0_)["aniseed/locals"].keymap, (_0_)["aniseed/locals"]["plugin-installed?"], (_0_)["aniseed/locals"]["safe-require"], (_0_)["aniseed/locals"]["shorten-path"], (_0_)["aniseed/locals"]["single-to-list"], (_0_)["aniseed/locals"]["split-last"], (_0_)["aniseed/locals"]["surround-if-present"], (_0_)["aniseed/locals"]["without-keys"]}
  end
  ok_3f_0_, val_0_ = pcall(_2_)
  if ok_3f_0_ then
    _0_["aniseed/local-fns"] = {["require-macros"] = {macros = true}, autoload = {a = "aniseed.core", fennel = "aniseed.fennel", nvim = "aniseed.nvim", str = "aniseed.string"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _2_(...)
local a = _local_0_[1]
local filter_table = _local_0_[10]
local find_map = _local_0_[11]
local find_where = _local_0_[12]
local highlight = _local_0_[13]
local highlight_add = _local_0_[14]
local keep_if = _local_0_[15]
local keymap = _local_0_[16]
local plugin_installed_3f = _local_0_[17]
local safe_require = _local_0_[18]
local shorten_path = _local_0_[19]
local fennel = _local_0_[2]
local single_to_list = _local_0_[20]
local split_last = _local_0_[21]
local surround_if_present = _local_0_[22]
local without_keys = _local_0_[23]
local nvim = _local_0_[3]
local str = _local_0_[4]
local all = _local_0_[5]
local buffer_content = _local_0_[6]
local comp = _local_0_[7]
local contains_3f = _local_0_[8]
local del_keymap = _local_0_[9]
local _2amodule_2a = _0_
local _2amodule_name_2a = "utils"
do local _ = ({nil, _0_, nil, {{nil}, nil, nil, nil}})[2] end
local plugin_installed_3f0
do
  local v_0_
  do
    local v_0_0
    local function plugin_installed_3f1(name)
      return (nil ~= packer_plugins[name])
    end
    v_0_0 = plugin_installed_3f1
    _0_["plugin-installed?"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["plugin-installed?"] = v_0_
  plugin_installed_3f0 = v_0_
end
local all0
do
  local v_0_
  do
    local v_0_0
    local function all1(f, xs)
      local function _3_(_241)
        return not f(_241)
      end
      return not a.some(_3_)
    end
    v_0_0 = all1
    _0_["all"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["all"] = v_0_
  all0 = v_0_
end
local single_to_list0
do
  local v_0_
  do
    local v_0_0
    local function single_to_list1(x)
      if a["table?"](x) then
        return x
      else
        return {x}
      end
    end
    v_0_0 = single_to_list1
    _0_["single-to-list"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["single-to-list"] = v_0_
  single_to_list0 = v_0_
end
local contains_3f0
do
  local v_0_
  do
    local v_0_0
    local function contains_3f1(list, elem)
      local function _3_(_241)
        return (elem == _241)
      end
      do local _ = a.some(_3_, list) end
      return false
    end
    v_0_0 = contains_3f1
    _0_["contains?"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["contains?"] = v_0_
  contains_3f0 = v_0_
end
local filter_table0
do
  local v_0_
  do
    local v_0_0
    local function filter_table1(f, t)
      local tbl_0_ = {}
      for k, v in pairs(t) do
        local _3_, _4_ = nil, nil
        if f(k, v) then
          _3_, _4_ = k, v
        else
        _3_, _4_ = nil
        end
        if ((nil ~= _3_) and (nil ~= _4_)) then
          local k_0_ = _3_
          local v_0_1 = _4_
          tbl_0_[k_0_] = v_0_1
        end
      end
      return tbl_0_
    end
    v_0_0 = filter_table1
    _0_["filter-table"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["filter-table"] = v_0_
  filter_table0 = v_0_
end
local split_last0
do
  local v_0_
  do
    local v_0_0
    local function split_last1(s, sep)
      for i = #s, 1, -1 do
        local c = s:sub(i, i)
        if (sep == c) then
          local left = s:sub(1, (i - 1))
          local right = s:sub((i + 1))
          return { left, right }
        end
      end
      return {s}
    end
    v_0_0 = split_last1
    _0_["split-last"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["split-last"] = v_0_
  split_last0 = v_0_
end
local find_where0
do
  local v_0_
  do
    local v_0_0
    local function find_where1(pred, xs)
      for _, x in ipairs(xs) do
        if pred(x) then
          return x
        end
      end
      return nil
    end
    v_0_0 = find_where1
    _0_["find-where"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["find-where"] = v_0_
  find_where0 = v_0_
end
local find_map0
do
  local v_0_
  do
    local v_0_0
    local function find_map1(f, xs)
      for _, x in ipairs(xs) do
        local res = f(x)
        if (nil ~= res) then
          return res
        end
      end
      return nil
    end
    v_0_0 = find_map1
    _0_["find-map"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["find-map"] = v_0_
  find_map0 = v_0_
end
local keep_if0
do
  local v_0_
  do
    local v_0_0
    local function keep_if1(f, x)
      if f(x) then
        return x
      end
    end
    v_0_0 = keep_if1
    _0_["keep-if"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["keep-if"] = v_0_
  keep_if0 = v_0_
end
local without_keys0
do
  local v_0_
  do
    local v_0_0
    local function without_keys1(keys, t)
      local function _3_(_241)
        return not contains_3f0(keys, _241)
      end
      return filter_table0(_3_, t)
    end
    v_0_0 = without_keys1
    _0_["without-keys"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["without-keys"] = v_0_
  without_keys0 = v_0_
end
local keymap0
do
  local v_0_
  do
    local v_0_0
    local function keymap1(modes, from, to, _3fopts)
      local full_opts = without_keys0({"buffer"}, a.merge({noremap = true, silent = true}, (_3fopts or {})))
      for _, mode in ipairs(single_to_list0(modes)) do
        local _4_
        do
          local _3_ = _3fopts
          if _3_ then
            _4_ = (_3_).buffer
          else
            _4_ = _3_
          end
        end
        if _4_ then
          nvim.buf_set_keymap(0, mode, from, to, full_opts)
        else
          nvim.set_keymap(mode, from, to, full_opts)
        end
      end
      return nil
    end
    v_0_0 = keymap1
    _0_["keymap"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["keymap"] = v_0_
  keymap0 = v_0_
end
local del_keymap0
do
  local v_0_
  do
    local v_0_0
    local function del_keymap1(mode, from, _3fbuf_local)
      if _3fbuf_local then
        return nvim.buf_del_keymap(0, mode, from)
      else
        return nvim.del_keymap(mode, from)
      end
    end
    v_0_0 = del_keymap1
    _0_["del-keymap"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["del-keymap"] = v_0_
  del_keymap0 = v_0_
end
local safe_require0
do
  local v_0_
  do
    local v_0_0
    local function safe_require1(name)
      local function _3_()
        return require(name)
      end
      local function _4_(_241)
        return a.println(("Error sourcing " .. name .. ":\n" .. fennel.traceback(_241)))
      end
      return xpcall(_3_, _4_)
    end
    v_0_0 = safe_require1
    _0_["safe-require"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["safe-require"] = v_0_
  safe_require0 = v_0_
end
local buffer_content0
do
  local v_0_
  do
    local v_0_0
    local function buffer_content1(bufnr)
      return vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    end
    v_0_0 = buffer_content1
    _0_["buffer-content"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["buffer-content"] = v_0_
  buffer_content0 = v_0_
end
local surround_if_present0
do
  local v_0_
  do
    local v_0_0
    local function surround_if_present1(a0, mid, b)
      if mid then
        return (a0 .. mid .. b)
      else
        return ""
      end
    end
    v_0_0 = surround_if_present1
    _0_["surround-if-present"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["surround-if-present"] = v_0_
  surround_if_present0 = v_0_
end
local highlight0
do
  local v_0_
  do
    local v_0_0
    local function highlight1(group_arg, colset)
      local default = {bg = "NONE", fg = "NONE", gui = "NONE"}
      local opts = a.merge(default, colset)
      for _, group in ipairs(single_to_list0(group_arg)) do
        nvim.command(("hi! " .. group .. " guifg='" .. opts.fg .. "' guibg='" .. opts.bg .. "' gui='" .. opts.gui .. "'"))
      end
      return nil
    end
    v_0_0 = highlight1
    _0_["highlight"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["highlight"] = v_0_
  highlight0 = v_0_
end
local highlight_add0
do
  local v_0_
  do
    local v_0_0
    local function highlight_add1(group_arg, colset)
      for _, group in ipairs(single_to_list0(group_arg)) do
        nvim.command(("hi! " .. group .. surround_if_present0(" guibg='", colset.bg, "'") .. surround_if_present0(" guifg='", colset.fg, "'") .. surround_if_present0(" gui='", colset.gui, "'")))
      end
      return nil
    end
    v_0_0 = highlight_add1
    _0_["highlight-add"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["highlight-add"] = v_0_
  highlight_add0 = v_0_
end
local shorten_path0
do
  local v_0_
  do
    local v_0_0
    local function shorten_path1(path, seg_length, shorten_after)
      local segments = str.split(path, "/")
      if ((shorten_after > #path) or (2 > #segments)) then
        return path
      else
        local init = a.butlast(segments)
        local filename = a.last(segments)
        local shortened_segs
        local function _3_(_241)
          return string.sub(_241, 1, seg_length)
        end
        shortened_segs = a.map(_3_, init)
        return (str.join("/", shortened_segs) .. "/" .. filename)
      end
    end
    v_0_0 = shorten_path1
    _0_["shorten-path"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["shorten-path"] = v_0_
  shorten_path0 = v_0_
end
local comp0
do
  local v_0_
  do
    local v_0_0
    local function comp1(f, g)
      local function _3_(...)
        return f(g(...))
      end
      return _3_
    end
    v_0_0 = comp1
    _0_["comp"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_)["aniseed/locals"]
  t_0_["comp"] = v_0_
  comp0 = v_0_
end
return nil