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
    return {autoload("aniseed.core"), autoload("packer"), autoload("aniseed.nvim")}
  end
  ok_3f_0_, val_0_ = pcall(_1_)
  if ok_3f_0_ then
    _0_0["aniseed/local-fns"] = {autoload = {a = "aniseed.core", packer = "packer", vim = "aniseed.nvim"}}
    return val_0_
  else
    return print(val_0_)
  end
end
local _local_0_ = _1_(...)
local a = _local_0_[1]
local packer = _local_0_[2]
local vim = _local_0_[3]
local _2amodule_2a = _0_0
local _2amodule_name_2a = "plugin"
do local _ = ({nil, _0_0, nil, {{}, nil, nil, nil}})[2] end
local safe_require_plugin_config
do
  local v_0_
  do
    local v_0_0
    local function safe_require_plugin_config0(name)
      local ok_3f, val_or_err = pcall(require, ("plugins." .. name))
      if not ok_3f then
        return print(("dotfiles error: " .. val_or_err))
      end
    end
    v_0_0 = safe_require_plugin_config0
    _0_0["safe-require-plugin-config"] = v_0_0
    v_0_ = v_0_0
  end
  local t_0_ = (_0_0)["aniseed/locals"]
  t_0_["safe-require-plugin-config"] = v_0_
  safe_require_plugin_config = v_0_
end
local use
do
  local v_0_
  local function use0(...)
    local pkgs = {...}
    local function _2_(use1)
      for i = 1, a.count(pkgs), 2 do
        local name = pkgs[i]
        local opts = pkgs[(i + 1)]
        do
          local _3_0 = opts.mod
          if _3_0 then
            safe_require_plugin_config(_3_0)
          else
          end
        end
        use1(a.assoc(opts, 1, name))
      end
      return nil
    end
    return packer.startup(_2_)
  end
  v_0_ = use0
  local t_0_ = (_0_0)["aniseed/locals"]
  t_0_["use"] = v_0_
  use = v_0_
end
local function _2_()
  return vim.api.nvim_command("let g:deoplete#enable_at_startup = 1")
end
local function _3_()
  return vim.api.command("so ~/.config/nvim/coc.vim")
end
return use("junegunn/fzf", {}, "junegunn/fzf.vim", {}, "preservim/nerdtree", {}, "hugolgst/vimsence", {}, "jiangmiao/auto-pairs", {}, "Olical/conjure", {}, "ntpeters/vim-better-whitespace", {}, "kyazdani42/nvim-web-devicons", {}, "ryanoasis/vim-devicons", {}, "kyazdani42/nvim-tree.lua", {}, "akinsho/nvim-bufferline.lua", {mod = "bufferline"}, "shougo/deoplete.nvim", {post_ = _2_}, "nathanaelkane/vim-indent-guides", {}, "Yggdroot/indentLine", {}, "jacoborus/tender.vim", {}, "vim-airline/vim-airline", {mod = "airline-theme", requires = {"vim-airline/vim-airline-themes", "jacoborus/tender.vim"}}, "christoomey/vim-tmux-navigator", {}, "onsails/lspkind-nvim", {}, "Olical/aniseed", {}, "ziglang/zig.vim", {}, "rafcamlet/coc-nvim-lua", {}, "neoclide/coc.nvim", {config = _3_, ft = {"rust"}, requires = {"antoinemadec/coc-fzf"}}, "nvim-lua/completion-nvim", {}, "nvim-lua/lsp_extensions.nvim", {}, "neovim/nvim-lspconfig", {mod = "nvim_lsp"}, "rust-lang/rust.vim", {}, "udalov/kotlin-vim", {}, "derekelkins/agda-vim", {}, "dag/vim-fish", {}, "purescript-contrib/purescript-vim", {}, "wbthomason/packer.nvim", {}, "eraserhd/parinfer-rust", {run = "cargo build --release"})