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
  ["coc-nvim-lua"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/coc-nvim-lua"
  },
  ["coc.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/coc.nvim"
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
    config = { "\27LJ\1\2˜\6\0\0\r\1-\0R4\0\0\0%\1\1\0>\0\2\0023\1\2\0004\2\3\0\16\3\1\0>\2\2\4T\5\6€6\a\6\0007\a\4\a3\b\5\0+\t\0\0:\t\6\b>\a\2\1A\5\3\3N\5ø%\2\a\0%\3\b\0\16\4\3\0%\5\t\0\16\6\2\0%\a\n\0$\4\a\0047\5\v\0007\5\4\0053\6\f\0+\a\0\0:\a\6\0063\a\r\0;\4\1\a\16\b\3\0%\t\14\0$\b\t\b;\b\3\a:\a\15\0063\a%\0003\b\22\0003\t\16\0004\n\17\0007\n\18\n4\v\19\0007\v\20\v%\f\21\0>\n\3\2:\n\20\t:\t\23\b3\t\25\0003\n\24\0:\n\26\t:\t\27\b3\t \0002\n\0\b4\v\17\0007\v\28\v7\v\29\v%\f\30\0>\v\2\2)\f\2\0009\f\v\n4\v\17\0007\v\28\v7\v\29\v%\f\31\0>\v\2\2)\f\2\0009\f\v\n:\n!\t:\t\"\b3\t#\0:\t$\b:\b&\a:\a'\6>\5\2\0014\5\17\0007\5(\5%\6*\0:\6)\0054\5\17\0007\5+\5'\6\1\0:\6,\5G\0\1\0\0À!completion_enable_auto_popup\6g\30menuone,noinsert,noselect\16completeopt\6o\rsettings\bLua\1\0\0\14telemetry\1\0\1\venable\1\14workspace\flibrary\1\0\0\28$VIMRUNTIME/lua/vim/lsp\20$VIMRUNTIME/lua\vexpand\afn\16diagnostics\fglobals\1\0\0\1\2\0\0\bvim\fruntime\1\0\0\6;\tpath\fpackage\nsplit\bvim\1\0\1\fversion\vLuaJIT\bcmd\14/main.lua\1\3\0\0\0\a-E\1\0\0\16sumneko_lua\25/lua-language-server\n/bin/\30$HOME/lua-language-server\nLinux\14on_attach\1\0\0\nsetup\vipairs\1\a\0\0\fpyright\focamlls\18rust-analyzer\rtsserver\bhls\bzls\14lspconfig\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/opt/nvim-lspconfig"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
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
-- Config for: vim-airline
try_loadstring("\27LJ\1\0026\0\0\2\0\4\0\0054\0\0\0007\0\1\0%\1\3\0:\1\2\0G\0\1\0\vtender\18airline_theme\6g\bvim\0", "config", "vim-airline")
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
vim.cmd [[au FileType rust ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "rust" }, _G.packer_plugins)]]
vim.cmd [[au FileType haskell ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "haskell" }, _G.packer_plugins)]]
vim.cmd("augroup END")
END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
