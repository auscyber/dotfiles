
-- Entrypoint for my Neovim configuration!
-- We simply bootstrap packer and Aniseed here.
-- It's then up to Aniseed to compile and load fnl/init.fnl


local execute = vim.api.nvim_command
local fn = vim.fn
local pack_path = fn.stdpath("data") .. "/site/pack"
local fmt = string.format

function ensure (user, repo)
  -- Ensures a given github.com/USER/REPO is cloned in the pack/packer/start directory.
  local install_path = fmt("%s/packer/start/%s", pack_path, repo, repo)
  if fn.empty(fn.glob(install_path)) > 0 then
    execute(fmt("!git clone https://github.com/%s/%s %s", user, repo, install_path))
    execute(fmt("packadd %s", repo))
    if user == "wbthomason" and repo == "packer.nvim" then
	    packer_bootstrap = true
    end
--      if user == "wbthomason" && repo == "packer.nvim" then
--          require("packer").sync()
--      end
  end
end

if fn.has('windows') then
vim.o.shellslash = true
end




-- Bootstrap essential plugins required for installing and loading the rest.
ensure("wbthomason", "packer.nvim")
ensure("Olical", "aniseed")

vim.o.termguicolors = true

-- Enable Aniseed's automatic compilation and loading of Fennel source code.
vim.g["aniseed#env"] = {
  module = "init",
  compile = true
}


