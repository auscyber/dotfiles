local _2afile_2a = "/home/auscyber/.config/nvim/fnl/plugin.fnl"
local _0_0
do
  local name_0_ = "plugin"
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
    return {require("aniseed.core"), require("aniseed.nvim")}
  end
  ok_3f_0_, val_0_ = pcall(_1_)
  if ok_3f_0_ then
    _0_0["aniseed/local-fns"] = {["require-macros"] = {macros = true}, require = {a = "aniseed.core", nvim = "aniseed.nvim"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _1_(...)
local a = _local_0_[1]
local nvim = _local_0_[2]
local _2amodule_2a = _0_0
local _2amodule_name_2a = "plugin"
do local _ = ({nil, _0_0, nil, {{nil}, nil, nil, nil}})[2] end
local packer_0_ = require("packer")
local function _3_(_2_0)
  _2_0({"wbthomason/packer.nvim"})
  _2_0({"junegunn/fzf"})
  _2_0({"junegunn/fzf.vim"})
  local function _4_()
    return require("plugins.autopairs")
  end
  _2_0({"windwp/nvim-autopairs", config = _4_})
  _2_0({"Olical/conjure"})
  _2_0({"ntpeters/vim-better-whitespace"})
  _2_0({"kyazdani42/nvim-web-devicons"})
  _2_0({"ryanoasis/vim-devicons"})
  local function _5_()
    return require("plugins.tree")
  end
  _2_0({"kyazdani42/nvim-tree.lua", config = _5_})
  local function _6_()
    return require("plugins.bufferline")
  end
  _2_0({"akinsho/nvim-bufferline.lua", config = _6_})
  local function _7_()
    return vim.api.nvim_command("let g:deoplete#enable_at_startup = 1")
  end
  _2_0({"shougo/deoplete.nvim", post_ = _7_})
  _2_0({"nathanaelkane/vim-indent-guides"})
  _2_0({"Yggdroot/indentLine"})
  _2_0({"jacoborus/tender.vim"})
  local function _8_()
    return require("plugins.airline-theme")
  end
  _2_0({"vim-airline/vim-airline", config = _8_, requires = {"vim-airline/vim-airline-themes", "jacoborus/tender.vim"}})
  _2_0({"christoomey/vim-tmux-navigator"})
  local function _9_()
    return require("plugins.telescope")
  end
  _2_0({"nvim-telescope/telescope.nvim", config = _9_, requires = {{"nvim-lua/popup.nvim"}, {"nvim-lua/plenary.nvim"}}})
  _2_0({"onsails/lspkind-nvim"})
  _2_0({"Olical/aniseed"})
  _2_0({"ziglang/zig.vim", ft = {"zig"}})
  _2_0({"nvim-lua/completion-nvim"})
  _2_0({"nvim-lua/lsp_extensions.nvim"})
  local function _10_()
    return require("plugins.nvim_lsp")
  end
  _2_0({"neovim/nvim-lspconfig", config = _10_, ft = {"haskell", "rust", "typescript", "javascript", "lua"}})
  local function _11_()
    local nvim0 = require("aniseed.nvim")
    return nvim0.ex.autocmd("CursorHold,CursorHoldI", "*", "lua require'nvim-lightbulb'.update_lightbulb()")
  end
  _2_0({"kosayoda/nvim-lightbulb", config = _11_})
  _2_0({"rust-lang/rust.vim", ft = {"rust"}})
  _2_0({"udalov/kotlin-vim", ft = {"kotlin"}})
  local function _12_()
    return vim.api.nvim_command("let maplocalleader = \",\"")
  end
  _2_0({"derekelkins/agda-vim", config = _12_, ft = {"agda"}})
  _2_0({"dag/vim-fish"})
  _2_0({"purescript-contrib/purescript-vim", ft = {"ft", {"purescript"}}})
  _2_0({"eraserhd/parinfer-rust", ft = {"fennel"}, run = "cargo build --release"})
  _2_0({"nvim-treesitter/nvim-treesitter", ["do"] = "TSUpdate"})
  _2_0({"elkowar/nvim-gehzu"})
  local function _13_()
    return require("plugins.presence")
  end
  return _2_0({"andweeb/presence.nvim", config = _13_})
end
return packer_0_.startup(_3_)