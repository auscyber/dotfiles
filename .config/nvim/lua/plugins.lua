local _2afile_2a = "/home/auscyber/.config/nvim/fnl/plugins.fnl"
local _0_
do
  local name_0_ = "plugins"
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
    return {require("aniseed.core"), require("aniseed.nvim")}
  end
  ok_3f_0_, val_0_ = pcall(_2_)
  if ok_3f_0_ then
    _0_["aniseed/local-fns"] = {["require-macros"] = {macros = true}, require = {a = "aniseed.core", nvim = "aniseed.nvim"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _2_(...)
local a = _local_0_[1]
local nvim = _local_0_[2]
local _2amodule_2a = _0_
local _2amodule_name_2a = "plugins"
do local _ = ({nil, _0_, nil, {{nil}, nil, nil, nil}})[2] end
local packer_0_ = require("packer")
local function _4_(_3_)
  _3_({"wbthomason/packer.nvim"})
  _3_({"junegunn/fzf"})
  _3_({"junegunn/fzf.vim"})
  local function _5_()
    return require("plugins.autopairs")
  end
  _3_({"windwp/nvim-autopairs", config = _5_})
  _3_({"Olical/conjure"})
  _3_({"ntpeters/vim-better-whitespace"})
  _3_({"kyazdani42/nvim-web-devicons"})
  _3_({"ryanoasis/vim-devicons"})
  local function _6_()
    return require("plugins.tree")
  end
  _3_({"kyazdani42/nvim-tree.lua", config = _6_})
  local function _7_()
    return require("plugins.bufferline")
  end
  _3_({"akinsho/nvim-bufferline.lua", config = _7_})
  local function _8_()
    return vim.api.nvim_command("let g:deoplete#enable_at_startup = 1")
  end
  _3_({"shougo/deoplete.nvim", post_ = _8_})
  _3_({"nathanaelkane/vim-indent-guides"})
  _3_({"Yggdroot/indentLine"})
  _3_({"jacoborus/tender.vim"})
  _3_({"christoomey/vim-tmux-navigator"})
  local function _9_()
    return require("plugins.telescope")
  end
  _3_({"nvim-telescope/telescope.nvim", config = _9_, requires = {{"nvim-lua/popup.nvim"}, {"nvim-lua/plenary.nvim"}}})
  _3_({"onsails/lspkind-nvim"})
  _3_({"Olical/aniseed"})
  _3_({"ziglang/zig.vim", ft = {"zig"}})
  _3_({"norcalli/snippets.nvim"})
  local function _10_()
    return require("plugins.nvim_lsp")
  end
  _3_({"neovim/nvim-lspconfig", config = _10_, ft = {"haskell", "rust", "typescript", "javascript", "lua", "zig", "go", "c", "cpp", "typescriptreact", "scala", "nix"}, requires = {"nvim-lua/completion-nvim", "nvim-lua/lsp_extensions.nvim", "scalameta/nvim-metals"}})
  local function _11_()
    local nvim0 = require("aniseed.nvim")
    return nvim0.ex.autocmd("CursorHold,CursorHoldI", "*", "lua require'nvim-lightbulb'.update_lightbulb()")
  end
  _3_({"kosayoda/nvim-lightbulb", config = _11_})
  _3_({"rust-lang/rust.vim", ft = {"rust"}})
  _3_({"udalov/kotlin-vim", ft = {"kotlin"}})
  _3_({"dag/vim-fish"})
  _3_({"purescript-contrib/purescript-vim", ft = {"ft", {"purescript"}}})
  local function _12_()
    return require("plugins.treesitter")
  end
  _3_({"nvim-treesitter/nvim-treesitter", ["do"] = "TSUpdate", config = _12_, requires = {"nvim-treesitter/playground"}})
  _3_({"elkowar/nvim-gehzu"})
  _3_({"tpope/vim-fugitive"})
  local function _13_()
    return require("plugins.feline")
  end
  _3_({"famiu/feline.nvim", config = _13_})
  local function _14_()
    return require("plugins.presence")
  end
  _3_({"andweeb/presence.nvim", config = _14_})
  _3_({"LnL7/vim-nix"})
  return _3_({"ShinKage/nvim-idris2"})
end
return packer_0_.startup(_4_)