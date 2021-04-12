M = {'neovim/nvim-lspconfig'}

M.config = function ()
    local nvim_lsp = require('lspconfig')
    local comp = require 'completion'
local on_attach = function (client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
  comp.on_attach(client,bufnr)
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>a', [[<cmd>lua require'telescope.builtin'.lsp_code_actions{}<CR> ]], opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>a', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)


  -- Set some keybinds conditional on server capabilities
  if client.resolved_capabilities.document_formatting then
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  elseif client.resolved_capabilities.document_range_formatting then
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  end

  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]], false)
  end
end


    -- Use a loop to conveniently both setup defined servers
    -- and map buffer local keybindings when the language server attaches
    local servers = { "pyright","ocamlls","rust_analyzer", "tsserver", "hls", "zls"}
    for _, lsp in ipairs(servers) do
      nvim_lsp[lsp].setup { on_attach = on_attach }
    end
    local system_name = "Linux"

	-- set the path to the sumneko installation; if you previously installed via the now deprecated :LspInstall, use
	local sumneko_root_path = '$HOME/lua-language-server'
	local sumneko_binary = sumneko_root_path.."/bin/"..system_name.."/lua-language-server"
--	nvim_lsp.sumneko_lua.setup {
--	  on_attach = on_attach,
--	  cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"};
--	  settings = {
--	    Lua = {
--	      runtime = {
--		-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
--		version = 'LuaJIT',
--		-- Setup your lua path
--		path = vim.split(package.path, ';'),
--	      },
--	      diagnostics = {
--		-- Get the language server to recognize the `vim` global
--		globals = {'vim'},
--	      },
--	      workspace = {
--		-- Make the server aware of Neovim runtime files
--		library = {
--		  [vim.fn.expand('$VIMRUNTIME/lua')] = true,
--		  [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
--		},
--	      },
--	      -- Do not send telemetry data containing a randomized but unique identifier
--	      telemetry = {
--		enable = false,
--	      },
--	    },
--	  },
--	}
vim.o.completeopt = "menuone,noinsert,noselect"

vim.g.completion_enable_auto_popup = 1
end
M.ft = {"haskell","rust", "lua", "python"}

return M
