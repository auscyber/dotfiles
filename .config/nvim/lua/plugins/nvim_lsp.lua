local _2afile_2a = "/home/auscyber/.config/nvim/fnl/plugins/nvim_lsp.fnl"
local _0_
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
    return {autoload("aniseed.core"), autoload("completion"), autoload("lspconfig"), autoload("lspkind"), autoload("nvim-autopairs"), autoload("aniseed.nvim"), autoload("utils")}
  end
  ok_3f_0_, val_0_ = pcall(_2_)
  if ok_3f_0_ then
    _0_["aniseed/local-fns"] = {["require-macros"] = {macros = true}, autoload = {a = "aniseed.core", completion = "completion", lsp = "lspconfig", lspkind = "lspkind", npairs = "nvim-autopairs", nvim = "aniseed.nvim", utils = "utils"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _2_(...)
local a = _local_0_[1]
local completion = _local_0_[2]
local lsp = _local_0_[3]
local lspkind = _local_0_[4]
local npairs = _local_0_[5]
local nvim = _local_0_[6]
local utils = _local_0_[7]
local _2amodule_2a = _0_
local _2amodule_name_2a = "plugins.nvim_lsp"
do local _ = ({nil, _0_, nil, {{nil}, nil, nil, nil}})[2] end
vim.lsp.set_log_level("debug")
nvim.o.completeopt = "menuone,noinsert,noselect"
nvim.g.completion_enable_auto_popup = 1
_G.LOL = {}
local function _3_()
  
   if vim.fn.pumvisible() ~= 0  then
      if vim.fn.complete_info()["selected"] ~= -1 then
        completion.confirmCompletion()
        return npairs.esc("<c-y>")
      else
        vim.api.nvim_select_popupmenu_item(0 , false , false ,{})
        completion.confirmCompletion()
        return npairs.esc("<c-n><c-y>")
      end
    else
      return npairs.autopairs_cr()
    end
  
                                    
  return nil
end
_G.LOL.completion_confirm = _3_
local function on_attach(client, bufnr)
  completion.on_attach(client, bufnr)
  vim.g.completion_enable_snippet = "snippets.nvim"
  lspkind.init({})
  npairs.setup({})
  do
    local opts = {noremap = true, silent = true}
    local map
    local function _4_(key, command)
      return nvim.buf_set_keymap(bufnr, "n", key, command, opts)
    end
    map = _4_
    local imap
    local function _5_(key, command)
      return nvim.buf_set_keymap(bufnr, "i", key, command, {noremap = false, silent = true})
    end
    imap = _5_
    local inoremap
    local function _6_(key, command)
      return nvim.buf_set_keymap(bufnr, "i", key, command, a.merge(opts, {expr = true}))
    end
    inoremap = _6_
    nvim.buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
    map("gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>")
    map("gd", "<Cmd>lua vim.lsp.buf.definition()<CR>")
    map("K", "<Cmd>lua vim.lsp.buf.hover()<CR>")
    map("gi", "<cmd>lua vim.lsp.buf.implementation()<CR>")
    map("<C-K>", "<cmd>lua vim.lsp.buf.signature_help()<CR>")
    map("<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>")
    imap("<c-space>", "<Plug>(completion_trigger)")
    imap("<tab>", "<Plug>(completion_smart_tab)")
    imap("<s-tab>", "<Plug>(completion_smart_s_tab)")
    inoremap("<Tab>", "pumvisible() ? \"\\<C-n>\" : \"\\<Tab>\"")
    inoremap("<S-Tab>", "pumvisible() ? \"\\<C-p>\" : \"\\<S-Tab>\"")
    map("<space>a", "<cmd>lua require 'telescope.builtin'.lsp_workspace_diagnostics {}<CR>")
    map("ff", "<cmd>lua vim.lsp.buf.formatting()<CR>")
    map("<leader>a", "<cmd>lua require'telescope.builtin'.lsp_code_actions{}<CR>")
    inoremap("<CR>", "v:lua.LOL.completion_confirm()")
  end
  if client.resolved_capabilities.document_highlight then
    utils.highlight("LspReferenceRead", {gui = "underline"})
    utils.highlight("LspReferenceText", {gui = "underline"})
    utils.highlight("LspReferenceWrite", {gui = "underline"})
    return vim.api.nvim_exec("augroup lsp_document_highlight\n           autocmd! * <buffer>\n           autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()\n           autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()\n           autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting()\n         augroup END", false)
  end
end
local function init_lsp(lsp_name, _3fopts)
  local merged_opts = a.merge({on_attach = on_attach}, (_3fopts or {}))
  return lsp[lsp_name].setup(merged_opts)
end
init_lsp("tsserver")
init_lsp("hls", {settings = {languageServerHaskell = {formattingProvider = "stylish-haskell"}}})
init_lsp("gopls")
init_lsp("rust_analyzer")
init_lsp("clangd")
init_lsp("rnix")
init_lsp("ocamllsp")
init_lsp("pyls")
init_lsp("zls")
return init_lsp("metals")