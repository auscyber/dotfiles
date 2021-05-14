local _0_0
do
  local name_0_ = "plugins.nvim_lsp"
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
    return {autoload("aniseed.core"), autoload("completion"), autoload("lspconfig"), autoload("aniseed.nvim"), autoload("util")}
  end
  ok_3f_0_, val_0_ = pcall(_1_)
  if ok_3f_0_ then
    _0_0["aniseed/local-fns"] = {autoload = {a = "aniseed.core", completion = "completion", lsp = "lspconfig", nvim = "aniseed.nvim", utils = "util"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _1_(...)
local a = _local_0_[1]
local completion = _local_0_[2]
local lsp = _local_0_[3]
local nvim = _local_0_[4]
local utils = _local_0_[5]
local _2amodule_2a = _0_0
local _2amodule_name_2a = "plugins.nvim_lsp"
do local _ = ({nil, _0_0, nil, {{}, nil, nil, nil}})[2] end
local function on_attach(client, bufnr)
  if client.resolved_capabilities.document_highlight then
    utils.highlight("LspReferenceRead", {gui = "underline"})
    utils.highlight("LspReferenceText", {gui = "underline"})
    utils.highlight("LspReferenceWrite", {gui = "underline"})
    return nvim.api.nvim_exec("augroup lsp_document_highlight\n           autocmd! * <buffer> \n           autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight() \n           autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()\n         augroup END", false)
  end
end
local function init_lsp(lsp_name, _3fopts)
  local merged_opts = a.merge({on_attach = on_attach}, (_3fopts or {}))
  return lsp[lsp_name].setup(merged_opts)
end
return init_lsp("tsserver")