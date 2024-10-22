(module plugins.dap
  {require {dapui dapui
            dap dap}})
(set dap.adapters.lldb {
                        :type "executable"
                        :command "/usr/bin/lldb-vscode" ; adjust as needed
                        :name "lldb"})
(set dap.configurations.rust
                            {
                              :name "Launch"
                              :type "lldb"
                              :request "launch"
                              :program (fn []
                                         (vim.fn.input "Path to executable: " (.. (vim.fn.getcwd) "/") "file"))
                              :cwd "${workspaceFolder}"
                              :stopOnEntry false
                              :args {}

                              ;-- if you change `runInTerminal` to true, you might need to change the yama/ptrace_scope setting:
                              ;--
                              ;--    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
                              ;--
                              ;-- Otherwise you might get the following error:
                              ;--
                              ;--    Error on launch: Failed to attach to the target process
                              ;--
                              ;-- But you should be aware of the implications:
                              ;-- https://www.kernel.org/doc/html/latest/admin-guide/LSM/Yama.html
                              :runInTerminal  false})


(dapui.setup)
