local packages_path = vim.fn.stdpath("data") .. "/lazy"

local function install_package(name, alias)
  ---@type unknown, unknown, string, string
  local _, _, owner, repo = name:find("(.+)/(.+)")
  local path = ("%s/%s"):format(packages_path, alias or repo)

  if not vim.uv.fs_stat(path) then
    vim.notify(("Installing %s/%s..."):format(owner, repo), vim.log.levels.INFO)

    vim
      .system({
        "git",
        "clone",
        "--filter=blob:none",
        "--single-branch",
        ("https://github.com/%s/%s.git"):format(owner, repo),
        path,
      })
      :wait()
  end

  vim.opt.runtimepath:prepend(path)
end
install_package("folke/lazy.nvim")
install_package("Olical/aniseed")
install_package("tsbohc/zest.nvim")



vim.g.mapleader = " "
vim.g.maplocalleader = "\\"



vim.loader.enable()
-- ensure("rktjmp","hotpot.nvim")
--
--
--require ("hotpot")
require "zest".setup()

vim.o.termguicolors = true

vim.g["aniseed#env"] = {
  module = "core",
  compile = true,
  correlate = false
}
