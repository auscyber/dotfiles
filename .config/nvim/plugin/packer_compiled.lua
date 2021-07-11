-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

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

time([[Luarocks path setup]], true)
local package_path_str = "/home/auscyber/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/auscyber/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/auscyber/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  aniseed = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/aniseed"
  },
  ["completion-nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/completion-nvim"
  },
  conjure = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/conjure"
  },
  ["deoplete.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/deoplete.nvim"
  },
  ["feline.nvim"] = {
    config = { "\27LJ\2\n*\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\19plugins.feline\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/feline.nvim"
  },
  fzf = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/fzf"
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
    loaded = false,
    needs_bufread = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/opt/kotlin-vim"
  },
  ["lsp_extensions.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/lsp_extensions.nvim"
  },
  ["lspkind-nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/lspkind-nvim"
  },
  ["nvim-autopairs"] = {
    config = { "\27LJ\2\n-\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\22plugins.autopairs\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-autopairs"
  },
  ["nvim-bufferline.lua"] = {
    config = { "\27LJ\2\n.\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\23plugins.bufferline\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-bufferline.lua"
  },
  ["nvim-gehzu"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-gehzu"
  },
  ["nvim-idris2"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-idris2"
  },
  ["nvim-lightbulb"] = {
    config = { "\27LJ\2\n“\1\0\0\6\0\a\0\t6\0\0\0'\2\1\0B\0\2\0029\1\2\0009\1\3\1'\3\4\0'\4\5\0'\5\6\0D\1\4\0003lua require'nvim-lightbulb'.update_lightbulb()\6*\27CursorHold,CursorHoldI\fautocmd\aex\17aniseed.nvim\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-lightbulb"
  },
  ["nvim-lspconfig"] = {
    config = { "\27LJ\2\n,\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\21plugins.nvim_lsp\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/opt/nvim-lspconfig"
  },
  ["nvim-metals"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-metals"
  },
  ["nvim-tree.lua"] = {
    config = { "\27LJ\2\n(\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\17plugins.tree\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\n.\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\23plugins.treesitter\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  playground = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/playground"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["presence.nvim"] = {
    config = { "\27LJ\2\n,\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\21plugins.presence\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/presence.nvim"
  },
  ["purescript-vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/opt/purescript-vim"
  },
  ["rust.vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/opt/rust.vim"
  },
  ["snippets.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/snippets.nvim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\n-\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\22plugins.telescope\frequire\0" },
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["tender.vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/tender.vim"
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
  ["vim-fugitive"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-fugitive"
  },
  ["vim-indent-guides"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-indent-guides"
  },
  ["vim-nix"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-nix"
  },
  ["vim-tmux-navigator"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vim-tmux-navigator"
  },
  ["zig.vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/opt/zig.vim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: feline.nvim
time([[Config for feline.nvim]], true)
try_loadstring("\27LJ\2\n*\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\19plugins.feline\frequire\0", "config", "feline.nvim")
time([[Config for feline.nvim]], false)
-- Config for: nvim-autopairs
time([[Config for nvim-autopairs]], true)
try_loadstring("\27LJ\2\n-\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\22plugins.autopairs\frequire\0", "config", "nvim-autopairs")
time([[Config for nvim-autopairs]], false)
-- Config for: presence.nvim
time([[Config for presence.nvim]], true)
try_loadstring("\27LJ\2\n,\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\21plugins.presence\frequire\0", "config", "presence.nvim")
time([[Config for presence.nvim]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
try_loadstring("\27LJ\2\n-\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\22plugins.telescope\frequire\0", "config", "telescope.nvim")
time([[Config for telescope.nvim]], false)
-- Config for: nvim-lightbulb
time([[Config for nvim-lightbulb]], true)
try_loadstring("\27LJ\2\n“\1\0\0\6\0\a\0\t6\0\0\0'\2\1\0B\0\2\0029\1\2\0009\1\3\1'\3\4\0'\4\5\0'\5\6\0D\1\4\0003lua require'nvim-lightbulb'.update_lightbulb()\6*\27CursorHold,CursorHoldI\fautocmd\aex\17aniseed.nvim\frequire\0", "config", "nvim-lightbulb")
time([[Config for nvim-lightbulb]], false)
-- Config for: nvim-bufferline.lua
time([[Config for nvim-bufferline.lua]], true)
try_loadstring("\27LJ\2\n.\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\23plugins.bufferline\frequire\0", "config", "nvim-bufferline.lua")
time([[Config for nvim-bufferline.lua]], false)
-- Config for: nvim-tree.lua
time([[Config for nvim-tree.lua]], true)
try_loadstring("\27LJ\2\n(\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\17plugins.tree\frequire\0", "config", "nvim-tree.lua")
time([[Config for nvim-tree.lua]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\n.\0\0\3\0\2\0\0036\0\0\0'\2\1\0D\0\2\0\23plugins.treesitter\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType scala ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "scala" }, _G.packer_plugins)]]
vim.cmd [[au FileType nix ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "nix" }, _G.packer_plugins)]]
vim.cmd [[au FileType kotlin ++once lua require("packer.load")({'kotlin-vim'}, { ft = "kotlin" }, _G.packer_plugins)]]
vim.cmd [[au FileType table: 0x7faacb69f8f0 ++once lua require("packer.load")({'purescript-vim'}, { ft = "table: 0x7faacb69f8f0" }, _G.packer_plugins)]]
vim.cmd [[au FileType ft ++once lua require("packer.load")({'purescript-vim'}, { ft = "ft" }, _G.packer_plugins)]]
vim.cmd [[au FileType rust ++once lua require("packer.load")({'nvim-lspconfig', 'rust.vim'}, { ft = "rust" }, _G.packer_plugins)]]
vim.cmd [[au FileType haskell ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "haskell" }, _G.packer_plugins)]]
vim.cmd [[au FileType typescript ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "typescript" }, _G.packer_plugins)]]
vim.cmd [[au FileType zig ++once lua require("packer.load")({'nvim-lspconfig', 'zig.vim'}, { ft = "zig" }, _G.packer_plugins)]]
vim.cmd [[au FileType c ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "c" }, _G.packer_plugins)]]
vim.cmd [[au FileType typescriptreact ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "typescriptreact" }, _G.packer_plugins)]]
vim.cmd [[au FileType javascript ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "javascript" }, _G.packer_plugins)]]
vim.cmd [[au FileType cpp ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "cpp" }, _G.packer_plugins)]]
vim.cmd [[au FileType lua ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "lua" }, _G.packer_plugins)]]
vim.cmd [[au FileType go ++once lua require("packer.load")({'nvim-lspconfig'}, { ft = "go" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/auscyber/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]], true)
vim.cmd [[source /home/auscyber/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]]
time([[Sourcing ftdetect script at: /home/auscyber/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]], false)
time([[Sourcing ftdetect script at: /home/auscyber/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim]], true)
vim.cmd [[source /home/auscyber/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim]]
time([[Sourcing ftdetect script at: /home/auscyber/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim]], false)
time([[Sourcing ftdetect script at: /home/auscyber/.local/share/nvim/site/pack/packer/opt/kotlin-vim/ftdetect/kotlin.vim]], true)
vim.cmd [[source /home/auscyber/.local/share/nvim/site/pack/packer/opt/kotlin-vim/ftdetect/kotlin.vim]]
time([[Sourcing ftdetect script at: /home/auscyber/.local/share/nvim/site/pack/packer/opt/kotlin-vim/ftdetect/kotlin.vim]], false)
time([[Sourcing ftdetect script at: /home/auscyber/.local/share/nvim/site/pack/packer/opt/zig.vim/ftdetect/zig.vim]], true)
vim.cmd [[source /home/auscyber/.local/share/nvim/site/pack/packer/opt/zig.vim/ftdetect/zig.vim]]
time([[Sourcing ftdetect script at: /home/auscyber/.local/share/nvim/site/pack/packer/opt/zig.vim/ftdetect/zig.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
