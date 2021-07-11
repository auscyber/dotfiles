local _2afile_2a = "/home/auscyber/.config/nvim/fnl/plugins/treesitter.fnl"
local _0_
do
  local name_0_ = "plugins.treesitter"
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
    return {require("nvim-treesitter.parsers"), require("nvim-treesitter.configs")}
  end
  ok_3f_0_, val_0_ = pcall(_2_)
  if ok_3f_0_ then
    _0_["aniseed/local-fns"] = {require = {parsers = "nvim-treesitter.parsers", treesitter = "nvim-treesitter.configs"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _2_(...)
local parsers = _local_0_[1]
local treesitter = _local_0_[2]
local _2amodule_2a = _0_
local _2amodule_name_2a = "plugins.treesitter"
do local _ = ({nil, _0_, nil, {{}, nil, nil, nil}})[2] end
parsers.get_parser_configs()["agda"] = {filetype = "agda", install_info = {files = {"src/parser.c", "src/scanner.cc"}, url = "~/packages/tree-sitter-agda"}}
return treesitter.setup({autopairs = {enable = true}, ensure_installed = {"rust", "haskell", "agda", "javascript", "c", "fennel", "go", "zig", "query", "nix"}, highlight = {enable = true}, playground = {enable = true}, query_linter = {enable = true}})