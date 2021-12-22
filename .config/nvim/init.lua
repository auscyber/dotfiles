local execute = vim.api.nvim_command
local fn = vim.fn
local pack_path = fn.stdpath("data") .. "/site/pack"
local fmt = string.format
function ensure (user, repo)
  local install_path = fmt("%s/packer/start/%s", pack_path, repo, repo)
  if fn.empty(fn.glob(install_path)) > 0 then
    execute(fmt("!git clone https://github.com/%s/%s %s", user, repo, install_path))
    execute(fmt("packadd %s",repo))
    if user == "wbthomason" and repo == "packer.nvim" then
        packer_bootstrap = true
    end
  end
end

if fn.has("win32") > 0 then
	--	vim.o.shellslash = true
end


ensure("Olical", "aniseed")
ensure("lewis6991","impatient.nvim")
ensure("wbthomason", "packer.nvim")
ensure("tsbohc","zest.nvim")

require "zest".setup()
require"impatient"

vim.o.termguicolors = true

vim.g["aniseed#env"] = {
  module = "core",
  compile = true,
  correlate = false
}
