(module plugins.telescope
  {require {utils utils
            telescope telescope
            actions telescope.actions}})

(when (> (vim.fn.has :win32) 0)
  (let [fmt string.format
        exec vim.api.nvim_command
        destpath (.. (vim.fn.stdpath "data") "\\site\\sqlite")
        path (.. destpath "\\sqlite3.dll")]
    (if (> (vim.fn.empty (vim.fn.glob path)) 0)
      (let [url "https://www.sqlite.org/2021/sqlite-dll-win64-x64-3360000.zip"
            downloadpath (.. (vim.fn.stdpath :cache) "\\sqlitewindows.zip")]
          (exec (fmt "!mkdir -Force %s" destpath))
          (exec (fmt "!powershell wget %s -out %s" url downloadpath))
          (exec (fmt "!powershell Expand-Archive -Force -Path %s -DestinationPath %s" downloadpath destpath))))
    (set vim.g.sqlite_clib_path path)))


(telescope.setup
  {:defaults {:mappings {:i {:<esc> actions.close}}}
   :extensions {:ui-select [((. (require "telescope.themes") :get_dropdown) {})]}})
(telescope.load_extension "frecency")
(telescope.load_extension "notify")
(telescope.load_extension "ui-select")


(utils.keymap :n :<C-f> "<cmd> lua require 'telescope.builtin'.find_files()<CR>")
(utils.keymap :n :<C-b> "<cmd> lua require'telescope.builtin'.buffers()<CR>")
