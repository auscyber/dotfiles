" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif

packadd packer.nvim

try

lua << END
local package_path_str = "/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?/init.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    print('Error running ' .. component .. ' for ' .. name)
    error(result)
  end
  return result
end

_G.packer_plugins = {
  ["agda-vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/agda-vim"
  },
  ["animate.vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/animate.vim"
  },
  ["auto-pairs"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/auto-pairs"
  },
  ["coc.nvim"] = {
    config = { "\27LJ\1\2P\0\0\2\0\4\0\0064\0\0\0007\0\1\0007\0\2\0%\1\3\0>\0\2\1G\0\1\0  so ~/.config/nvim/coc.vim \17nvim_command\bapi\bvim\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/coc.nvim"
  },
  ["completion-nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/completion-nvim"
  },
  ["fzf.vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/fzf.vim"
  },
  indentLine = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/indentLine"
  },
  ["kotlin-vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/kotlin-vim"
  },
  nerdtree = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nerdtree"
  },
  ["nvim-bufferline.lua"] = {
    config = { "\27LJ\1\2p\0\0\4\0\b\0\v4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\6\0003\2\4\0003\3\3\0:\3\5\2:\2\a\1>\0\2\1G\0\1\0\foptions\1\0\0\20separator_style\1\0\0\1\3\0\0\5\5\nsetup\15bufferline\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-bufferline.lua"
  },
  ["nvim-lspconfig"] = {
    config = { "\27LJ\1\2A\2\0\3\1\3\0\a4\0\0\0007\0\1\0007\0\2\0+\1\0\0C\2\0\0=\0\1\1G\0\1\0\1À\24nvim_buf_set_keymap\bapi\bvimA\2\0\3\1\3\0\a4\0\0\0007\0\1\0007\0\2\0+\1\0\0C\2\0\0=\0\1\1G\0\1\0\1À\24nvim_buf_set_option\bapi\bvim·\16\1\2\n\0012\0\0011\2\0\0001\3\1\0+\4\0\0007\4\2\4\16\5\0\0\16\6\1\0>\4\3\1\16\4\3\0%\5\3\0%\6\4\0>\4\3\0013\4\5\0\16\5\2\0%\6\6\0%\a\a\0%\b\b\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\t\0%\b\n\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\v\0%\b\f\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\r\0%\b\14\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\15\0%\b\16\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\17\0%\b\18\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\19\0%\b\20\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\21\0%\b\22\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\23\0%\b\24\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\25\0%\b\26\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\27\0%\b\28\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\29\0%\b\30\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a\31\0%\b \0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a!\0%\b\"\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a#\0%\b$\0\16\t\4\0>\5\5\1\16\5\2\0%\6\6\0%\a%\0%\b&\0\16\t\4\0>\5\5\0017\5'\0007\5(\5\15\0\5\0T\6\a€\16\5\2\0%\6\6\0%\a)\0%\b*\0\16\t\4\0>\5\5\1T\5\n€7\5'\0007\5+\5\15\0\5\0T\6\6€\16\5\2\0%\6\6\0%\a)\0%\b,\0\16\t\4\0>\5\5\0017\5'\0007\5-\5\15\0\5\0T\6\6€4\5.\0007\5/\0057\0050\5%\0061\0)\a\1\0>\5\3\0010\0\0€G\0\1\0\1À·\3      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow\n      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow\n      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow\n      augroup lsp_document_highlight\n        autocmd! * <buffer>\n        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()\n        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()\n      augroup END\n    \14nvim_exec\bapi\bvim\23document_highlight0<cmd>lua vim.lsp.buf.range_formatting()<CR>\30document_range_formatting*<cmd>lua vim.lsp.buf.formatting()<CR>\r<space>f\24document_formatting\26resolved_capabilities2<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>\r<space>q0<cmd>lua vim.lsp.diagnostic.goto_next()<CR>\a]d0<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>\a[d<<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>\r<space>a*<cmd>lua vim.lsp.buf.references()<CR>\agr@<cmd>lua require'telescope.builtin'.lsp_code_actions{}<CR> \14<leader>a&<cmd>lua vim.lsp.buf.rename()<CR>\14<space>rn/<cmd>lua vim.lsp.buf.type_definition()<CR>\r<space>DJ<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>\14<space>wl7<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>\14<space>wr4<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>\14<space>wa.<cmd>lua vim.lsp.buf.signature_help()<CR>\n<C-k>.<cmd>lua vim.lsp.buf.implementation()<CR>\agi%<Cmd>lua vim.lsp.buf.hover()<CR>\6K*<Cmd>lua vim.lsp.buf.definition()<CR>\agd+<Cmd>lua vim.lsp.buf.declaration()<CR>\agD\6n\1\0\2\vsilent\2\fnoremap\2\27v:lua.vim.lsp.omnifunc\romnifunc\14on_attach\0\0ê\4\1\0\f\0\27\00184\0\0\0%\1\1\0>\0\2\0024\1\0\0%\2\2\0>\1\2\0021\2\3\0003\3\4\0004\4\5\0\16\5\3\0>\4\2\4T\a\5€6\t\b\0007\t\6\t3\n\a\0:\2\b\n>\t\2\1A\a\3\3N\aù%\4\t\0004\5\n\0007\5\v\0057\5\f\5>\5\1\2%\6\r\0004\a\0\0%\b\1\0>\a\2\0027\a\14\a7\a\6\a3\b\17\0003\t\15\0;\6\1\t4\n\16\0\16\v\5\0>\n\2\0<\n\0\0:\t\18\b:\2\b\b>\a\2\1%\a\19\0\16\b\a\0%\t\20\0\16\n\4\0%\v\21\0$\b\v\b4\t\n\0007\t\22\t%\n\24\0:\n\23\t4\t\n\0007\t\25\t'\n\1\0:\n\26\t0\0\0€G\0\1\0!completion_enable_auto_popup\6g\30menuone,noinsert,noselect\16completeopt\6o\25/lua-language-server\n/bin/\30$HOME/lua-language-server\bcmd\1\0\0\rtostring\1\4\0\0\0\21--languageserver\14--hostPID\14omnisharpZ/home/auscyber/.vscode/extensions/ms-dotnettools.csharp-1.23.11/.omnisharp/1.37.8/run\vgetpid\afn\bvim\nLinux\14on_attach\1\0\0\nsetup\vipairs\1\6\0\0\fpyright\focamlls\rtsserver\bhls\bzls\0\15completion\14lspconfig\frequire\t€€À™\4\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/opt/nvim-lspconfig"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    config = { "\27LJ\1\2`\0\0\2\0\4\0\0064\0\0\0007\0\1\0007\0\2\0%\1\3\0>\0\2\1G\0\1\0000 autocmd BufWrite plugins.lua PackerCompile\17nvim_command\bapi\bvim\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["purescript-vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/purescript-vim"
  },
  ["rust.vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/rust.vim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\1\2;\0\0\2\0\3\0\a4\0\0\0%\1\1\0>\0\2\0027\0\2\0002\1\0\0>\0\2\1G\0\1\0\nsetup\14telescope\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["tender.vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/tender.vim"
  },
  ["vim-airline"] = {
    config = { "\27LJ\1\0026\0\0\2\0\4\0\0054\0\0\0007\0\1\0%\1\3\0:\1\2\0G\0\1\0\vtender\18airline_theme\6g\bvim\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-airline"
  },
  ["vim-airline-themes"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-airline-themes"
  },
  ["vim-better-whitespace"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-better-whitespace"
  },
  ["vim-devicons"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-devicons"
  },
  ["vim-fish"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-fish"
  },
  ["vim-indent-guides"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-indent-guides"
  },
  ["vim-tmux-navigator"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-tmux-navigator"
  },
  vimsence = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vimsence"
  },
  ["zig.vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/zig.vim"
  }
}

-- Config for: nvim-bufferline.lua
try_loadstring("\27LJ\1\2p\0\0\4\0\b\0\v4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\6\0003\2\4\0003\3\3\0:\3\5\2:\2\a\1>\0\2\1G\0\1\0\foptions\1\0\0\20separator_style\1\0\0\1\3\0\0\5\5\nsetup\15bufferline\frequire\0", "config", "nvim-bufferline.lua")
-- Config for: telescope.nvim
try_loadstring("\27LJ\1\2;\0\0\2\0\3\0\a4\0\0\0%\1\1\0>\0\2\0027\0\2\0002\1\0\0>\0\2\1G\0\1\0\nsetup\14telescope\frequire\0", "config", "telescope.nvim")
-- Config for: packer.nvim
try_loadstring("\27LJ\1\2`\0\0\2\0\4\0\0064\0\0\0007\0\1\0007\0\2\0%\1\3\0>\0\2\1G\0\1\0000 autocmd BufWrite plugins.lua PackerCompile\17nvim_command\bapi\bvim\0", "config", "packer.nvim")
-- Config for: vim-airline
try_loadstring("\27LJ\1\0026\0\0\2\0\4\0\0054\0\0\0007\0\1\0%\1\3\0:\1\2\0G\0\1\0\vtender\18airline_theme\6g\bvim\0", "config", "vim-airline")
-- Config for: coc.nvim
try_loadstring("\27LJ\1\2P\0\0\2\0\4\0\0064\0\0\0007\0\1\0007\0\2\0%\1\3\0>\0\2\1G\0\1\0  so ~/.config/nvim/coc.vim \17nvim_command\bapi\bvim\0", "config", "coc.nvim")
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
vim.cmd [[au FileType lua ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "lua" }, _G.packer_plugins)]]
vim.cmd [[au FileType python ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "python" }, _G.packer_plugins)]]
vim.cmd [[au FileType cs ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "cs" }, _G.packer_plugins)]]
vim.cmd [[au FileType haskell ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "haskell" }, _G.packer_plugins)]]
vim.cmd [[au FileType rust ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "rust" }, _G.packer_plugins)]]
vim.cmd("augroup END")
END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
