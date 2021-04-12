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
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-bufferline.lua"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["purescript-vim"] = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/purescript-vim"
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
  vimsence = {
    loaded = true,
    path = "/home/auscyber/.local/share/nvim/site/pack/packer/start/vimsence"
  }
}

-- Config for: vim-airline
try_loadstring("\27LJ\1\0026\0\0\2\0\4\0\0054\0\0\0007\0\1\0%\1\3\0:\1\2\0G\0\1\0\vtender\18airline_theme\6g\bvim\0", "config", "vim-airline")
END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
