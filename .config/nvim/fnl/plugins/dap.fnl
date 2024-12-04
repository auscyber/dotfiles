(module plugins.dap
  {require {dapui dapui
            dap dap}
   require-macros [zest.macros]})

(def-keymap [n]
  {"<leader>dct"  "<cmd>lua require\"dap\".continue()<CR>"
   "<leader>dsv" "<cmd>lua require\"dap\".step_over()<CR>"
   "<leader>dsi"  "<cmd>lua require\"dap\".step_into()<CR>"
   "<leader>dso"  "<cmd>lua require\"dap\".step_out()<CR>"
   "<leader>dtb"  "<cmd>lua require\"dap\".toggle_breakpoint()<CR>"

   "<leader>dsc"  "<cmd>lua require\"dap.ui.variables\".scopes()<CR>"
   "<leader>dhh"  "<cmd>lua require\"dap.ui.variables\".hover()<CR>"})

(def-keymap "<leader>dhv" [v]
          "<cmd>lua require\"dap.ui.variables\".visual_hover()<CR>")

(def-keymap "<leader>duh" [n] "<cmd>lua require\"dap.ui.widgets\".hover()<CR>")
(def-keymap "<leader>duf" [n]
          "<cmd>lua local widgets=require'dap.ui.widgets');widgets.centered_float(widgets.scopes)<CR>")

(def-keymap "<leader>dsbr" [n]
          "<cmd>lua require\"dap\".set_breakpoint(vim.fn.input(\"Breakpoint condition: \"))<CR>")
(def-keymap "<leader>dsbm" [n]
          "<cmd>lua require\"dap\".set_breakpoint(nil nil vim.fn.input(\"Log point message: \"))<CR>")
(def-keymap "<leader>dro" [n] "<cmd>lua require\"dap\".repl.open()<CR>")
(def-keymap "<leader>drl" [n] "<cmd>lua require\"dap\".repl.run_last()<CR>")

; (dap-install.config "codelldb" {})


(dapui.setup)
