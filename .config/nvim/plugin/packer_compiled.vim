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
local package_path_str = "/home/will/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/will/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/will/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/will/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/will/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
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
    path = "/home/will/.local/share/nvim/site/pack/packer/start/agda-vim"
  },
  ["animate.vim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/animate.vim"
  },
  aniseed = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/aniseed"
  },
  ["auto-pairs"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/auto-pairs"
  },
  ["coc-nvim-lua"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/coc-nvim-lua"
  },
  ["completion-nvim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/completion-nvim"
  },
  conjure = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/conjure"
  },
  ["deoplete.nvim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/deoplete.nvim"
  },
  fzf = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/fzf"
  },
  ["fzf.vim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/fzf.vim"
  },
  indentLine = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/indentLine"
  },
  ["kotlin-vim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/kotlin-vim"
  },
  ["lsp_extensions.nvim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/lsp_extensions.nvim"
  },
  ["lspkind-nvim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/lspkind-nvim"
  },
  nerdtree = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/nerdtree"
  },
  ["nvim-bufferline.lua"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/nvim-bufferline.lua"
  },
  ["nvim-lightbulb"] = {
    config = { "\27LJ\2\n“\1\0\0\6\0\a\0\t6\0\0\0'\2\1\0B\0\2\0029\1\2\0009\1\3\1'\3\4\0'\4\5\0'\5\6\0D\1\4\0003lua require'nvim-lightbulb'.update_lightbulb()\6*\27CursorHold,CursorHoldI\fautocmd\aex\17aniseed.nvim\frequire\0" },
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/nvim-lightbulb"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-nonicons"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/nvim-nonicons"
  },
  ["nvim-tree.lua"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["parinfer-rust"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/parinfer-rust"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["purescript-vim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/purescript-vim"
  },
  ["rust.vim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/rust.vim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["tender.vim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/tender.vim"
  },
  ["vim-airline"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/vim-airline"
  },
  ["vim-airline-themes"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/vim-airline-themes"
  },
  ["vim-better-whitespace"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/vim-better-whitespace"
  },
  ["vim-devicons"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/vim-devicons"
  },
  ["vim-fish"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/vim-fish"
  },
  ["vim-indent-guides"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/vim-indent-guides"
  },
  ["vim-tmux-navigator"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/vim-tmux-navigator"
  },
  vimsence = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/vimsence"
  },
  ["zig.vim"] = {
    loaded = true,
    path = "/home/will/.local/share/nvim/site/pack/packer/start/zig.vim"
  }
}

time("Defining packer_plugins", false)
-- Config for: nvim-lightbulb
time("Config for nvim-lightbulb", true)
try_loadstring("\27LJ\2\n“\1\0\0\6\0\a\0\t6\0\0\0'\2\1\0B\0\2\0029\1\2\0009\1\3\1'\3\4\0'\4\5\0'\5\6\0D\1\4\0003lua require'nvim-lightbulb'.update_lightbulb()\6*\27CursorHold,CursorHoldI\fautocmd\aex\17aniseed.nvim\frequire\0", "config", "nvim-lightbulb")
time("Config for nvim-lightbulb", false)
if should_profile then save_profiles() end

END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
