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
  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time("Luarocks path setup", true)
local package_path_str = "/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?/init.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/auscyber/.cache/nvim/packer_hererocks/2.0.5/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time("Luarocks path setup", false)
time("try_loadstring definition", true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    print('Error running ' .. component .. ' for ' .. name)
    error(result)
  end
  return result
end

time("try_loadstring definition", false)
time("Defining packer_plugins", true)
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
    loaded = false,
    needs_bufread = false,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/opt/coc.nvim"
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
  ["nlua.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nlua.nvim"
  },
  ["nvim-bufferline.lua"] = {
    config = { "\27LJ\1\2p\0\0\4\0\b\0\v4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\6\0003\2\4\0003\3\3\0:\3\5\2:\2\a\1>\0\2\1G\0\1\0\foptions\1\0\0\20separator_style\1\0\0\1\3\0\0\5\5\nsetup\15bufferline\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-bufferline.lua"
  },
  ["nvim-lspconfig"] = {
    config = { "\27LJ\1\2A\2\0\3\1\3\0\a4\0\0\0007\0\1\0007\0\2\0+\1\0\0C\2\0\0=\0\1\1G\0\1\0\1À\24nvim_buf_set_keymap\bapi\bvimA\2\0\3\1\3\0\a4\0\0\0007\0\1\0007\0\2\0+\1\0\0C\2\0\0=\0\1\1G\0\1\0\1À\24nvim_buf_set_option\bapi\bvimÐ\16\1\2\v\0004\0\0014\2\0\0%\3\1\0>\2\2\0021\3\2\0001\4\3\0007\5\4\2\16\6\0\0\16\a\1\0>\5\3\1\16\5\4\0%\6\5\0%\a\6\0>\5\3\0013\5\a\0\16\6\3\0%\a\b\0%\b\t\0%\t\n\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\v\0%\t\f\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\r\0%\t\14\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\15\0%\t\16\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\17\0%\t\18\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\19\0%\t\20\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\21\0%\t\22\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\23\0%\t\24\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\25\0%\t\26\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\27\0%\t\28\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\29\0%\t\30\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b\31\0%\t \0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b!\0%\t\"\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b#\0%\t$\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b%\0%\t&\0\16\n\5\0>\6\5\1\16\6\3\0%\a\b\0%\b'\0%\t(\0\16\n\5\0>\6\5\0017\6)\0007\6*\6\15\0\6\0T\a\a€\16\6\3\0%\a\b\0%\b+\0%\t,\0\16\n\5\0>\6\5\1T\6\n€7\6)\0007\6-\6\15\0\6\0T\a\6€\16\6\3\0%\a\b\0%\b+\0%\t.\0\16\n\5\0>\6\5\0017\6)\0007\6/\6\15\0\6\0T\a\6€4\0060\0007\0061\0067\0062\6%\a3\0)\b\1\0>\6\3\0010\0\0€G\0\1\0·\3      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow\n      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow\n      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow\n      augroup lsp_document_highlight\n        autocmd! * <buffer>\n        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()\n        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()\n      augroup END\n    \14nvim_exec\bapi\bvim\23document_highlight0<cmd>lua vim.lsp.buf.range_formatting()<CR>\30document_range_formatting*<cmd>lua vim.lsp.buf.formatting()<CR>\r<space>f\24document_formatting\26resolved_capabilities2<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>\r<space>q0<cmd>lua vim.lsp.diagnostic.goto_next()<CR>\a]d0<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>\a[d<<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>\r<space>a*<cmd>lua vim.lsp.buf.references()<CR>\agr@<cmd>lua require'telescope.builtin'.lsp_code_actions{}<CR> \14<leader>a&<cmd>lua vim.lsp.buf.rename()<CR>\14<space>rn/<cmd>lua vim.lsp.buf.type_definition()<CR>\r<space>DJ<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>\14<space>wl7<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>\14<space>wr4<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>\14<space>wa.<cmd>lua vim.lsp.buf.signature_help()<CR>\n<C-k>.<cmd>lua vim.lsp.buf.implementation()<CR>\agi%<Cmd>lua vim.lsp.buf.hover()<CR>\6K*<Cmd>lua vim.lsp.buf.definition()<CR>\agd+<Cmd>lua vim.lsp.buf.declaration()<CR>\agD\6n\1\0\2\vsilent\2\fnoremap\2\27v:lua.vim.lsp.omnifunc\romnifunc\14on_attach\0\0\15completion\frequireÀ\5\1\0\v\0 \1@4\0\0\0%\1\1\0>\0\2\0021\1\2\0003\2\3\0004\3\4\0\16\4\2\0>\3\2\4T\6\5€6\b\a\0007\b\5\b3\t\6\0:\1\a\t>\b\2\1A\6\3\3N\6ù%\3\b\0004\4\t\0007\4\n\0047\4\v\4>\4\1\2%\5\f\0007\6\r\0007\6\5\0063\a\16\0003\b\14\0;\5\1\b4\t\15\0\16\n\4\0>\t\2\0<\t\0\0:\b\17\a:\1\a\a>\6\2\0014\6\0\0%\a\18\0>\6\2\0027\6\5\0064\a\0\0%\b\1\0>\a\2\0023\b\19\0:\1\a\b3\t\20\0:\t\21\b>\6\3\1%\6\22\0\16\a\6\0%\b\23\0\16\t\3\0%\n\24\0$\a\n\a4\b\25\0%\t\26\0>\b\2\0014\b\t\0007\b\27\b%\t\29\0:\t\28\b4\b\t\0007\b\30\b'\t\1\0:\t\31\bG\0\1\0!completion_enable_auto_popup\6g\30menuone,noinsert,noselect\16completeopt\6o\ahi\nprint\25/lua-language-server\n/bin/\30$HOME/lua-language-server\fglobals\1\6\0\0\nColor\6c\nGroup\6g\6s\1\0\0\18nlua.lsp.nvim\bcmd\1\0\0\rtostring\1\4\0\0\0\21--languageserver\14--hostPID\14omnisharpZ/home/auscyber/.vscode/extensions/ms-dotnettools.csharp-1.23.11/.omnisharp/1.37.8/run\vgetpid\afn\bvim\nLinux\14on_attach\1\0\0\nsetup\vipairs\1\a\0\0\fpyright\focamlls\rtsserver\bhls\bzls\rocamllsp\0\14lspconfig\frequire\t€€À™\4\0" },
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

time("Defining packer_plugins", false)
-- Config for: packer.nvim
time("Config for packer.nvim", true)
try_loadstring("\27LJ\1\2`\0\0\2\0\4\0\0064\0\0\0007\0\1\0007\0\2\0%\1\3\0>\0\2\1G\0\1\0000 autocmd BufWrite plugins.lua PackerCompile\17nvim_command\bapi\bvim\0", "config", "packer.nvim")
time("Config for packer.nvim", false)
-- Config for: telescope.nvim
time("Config for telescope.nvim", true)
try_loadstring("\27LJ\1\2;\0\0\2\0\3\0\a4\0\0\0%\1\1\0>\0\2\0027\0\2\0002\1\0\0>\0\2\1G\0\1\0\nsetup\14telescope\frequire\0", "config", "telescope.nvim")
time("Config for telescope.nvim", false)
-- Config for: vim-airline
time("Config for vim-airline", true)
try_loadstring("\27LJ\1\0026\0\0\2\0\4\0\0054\0\0\0007\0\1\0%\1\3\0:\1\2\0G\0\1\0\vtender\18airline_theme\6g\bvim\0", "config", "vim-airline")
time("Config for vim-airline", false)
-- Config for: nvim-bufferline.lua
time("Config for nvim-bufferline.lua", true)
try_loadstring("\27LJ\1\2p\0\0\4\0\b\0\v4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\6\0003\2\4\0003\3\3\0:\3\5\2:\2\a\1>\0\2\1G\0\1\0\foptions\1\0\0\20separator_style\1\0\0\1\3\0\0\5\5\nsetup\15bufferline\frequire\0", "config", "nvim-bufferline.lua")
time("Config for nvim-bufferline.lua", false)
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time("Defining lazy-load filetype autocommands", true)
vim.cmd [[au FileType lua ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "lua" }, _G.packer_plugins)]]
vim.cmd [[au FileType python ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "python" }, _G.packer_plugins)]]
vim.cmd [[au FileType cs ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "cs" }, _G.packer_plugins)]]
vim.cmd [[au FileType haskell ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "haskell" }, _G.packer_plugins)]]
vim.cmd [[au FileType rust ++once lua require("packer.load")({'nvim-lspconfig', 'coc.nvim'}, { ft = "rust" }, _G.packer_plugins)]]
vim.cmd [[au FileType ocaml ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "ocaml" }, _G.packer_plugins)]]
time("Defining lazy-load filetype autocommands", false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
