(module plugins.neorg {require {neorg neorg cmp cmp}})

(neorg.setup {:load {:core.defaults {}
                     ; Load all the default modules
                     :core.norg.concealer {}
                     ; Allows for use of icons
                     :core.norg.completion {:config {:engine :nvim-cmp}}
                     ;         "core.integrations.treesitter" {}
                     :core.norg.dirman {; Manage your directories with Neorg
                                        :config {:workspaces {:my_workspace "~/neorg"}}}}})
