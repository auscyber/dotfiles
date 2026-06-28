{ den, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.nvim = den.lib.nixvim.mkPackage { inherit pkgs; };
    };

  den.aspects.nixvim.includes = [ (den.batteries.unfree [ "cmp-nvim-lsp-document-symbol" ]) ];
  den.aspects.nixvim.unfreeAllowed = [
    "cmp-copilot"
    "copilot.vim"
    "idris2-vim"
    "presence.nvim"
    "copilot-lua"
  ];
  den.aspects.nixvim.nvim =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {

      globals = {
        mapleader = "\\";
        maplocalleader = "\\";

        better_whitespace_filetypes_blacklist = [
          "dashboard"
          "diff"
          "git"
          "gitcommit"
          "unite"
          "qf"
          "help"
          "markdown"
          "fugitive"
        ];
        strip_whitespace_confirm = 0;
        strip_whitespace_on_save = 1;
      };
      withPython3 = true;
      withRuby = false;

      opts = {
        mouse = "a";
        mousemoveevent = true;
        termguicolors = true;
        showmode = false;

        tabstop = 4;
        shiftwidth = 4;
        hidden = true;
        updatetime = 400;
        signcolumn = "yes";
        cursorline = true;
        cursorlineopt = "number";
        conceallevel = 3;
        number = true;
        relativenumber = true;

        completeopt = "menu,menuone,noselect";
      };

      colorscheme = "pink_ocean";

      keymaps = [
        {
          mode = [
            "n"
            "v"
            "i"
          ];
          key = "<F11>";
          action = "<cmd>let g:neovide_fullscreen=!g:neovide_fullscreen<cr>";
          options.noremap = true;
        }

        {
          mode = "n";
          key = "<C-f>";
          action = "<cmd> lua require 'telescope.builtin'.find_files()<CR>";
          options = {
            noremap = true;
            silent = true;
          };
        }
        {
          mode = "n";
          key = "<C-b>";
          action = "<cmd> lua require'telescope.builtin'.buffers()<CR>";
          options = {
            noremap = true;
            silent = true;
          };
        }
        {
          mode = "n";
          key = "gb";
          action = "BufferLinePick<CR>";
          options = {
            noremap = true;
            silent = true;
          };
        }

        # DAP mappings
        {
          mode = "n";
          key = "<leader>dhh";
          action = "<cmd>lua require\"dap.ui.variables\".hover()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dso";
          action = "<cmd>lua require\"dap\".step_out()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dct";
          action = "<cmd>lua require\"dap\".continue()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dsc";
          action = "<cmd>lua require\"dap.ui.variables\".scopes()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dsi";
          action = "<cmd>lua require\"dap\".step_into()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dtb";
          action = "<cmd>lua require\"dap\".toggle_breakpoint()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dsv";
          action = "<cmd>lua require\"dap\".step_over()<CR>";
          options.noremap = true;
        }
        {
          mode = "v";
          key = "<leader>dhv";
          action = "<cmd>lua require\"dap.ui.variables\".visual_hover()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>duh";
          action = "<cmd>lua require\"dap.ui.widgets\".hover()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>duf";
          action = "<cmd>lua local widgets=require'dap.ui.widgets');widgets.centered_float(widgets.scopes)<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dsbr";
          action = "<cmd>lua require\"dap\".set_breakpoint(vim.fn.input(\"Breakpoint condition: \") )<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dsbm";
          action = "<cmd>lua require\"dap\".set_breakpoint(nil, nil, vim.fn.input(\"Log point message: \") )<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>dro";
          action = "<cmd>lua require\"dap\".repl.open()<CR>";
          options.noremap = true;
        }
        {
          mode = "n";
          key = "<leader>drl";
          action = "<cmd>lua require\"dap\".repl.run_last()<CR>";
          options.noremap = true;
        }
      ];

      vimAlias = true;
      withNodeJs = true;

      plugins = {
        lz-n = {
          enable = true;
        };

        lzn-auto-require = {
          enable = true;
        };
        mini = {
          enable = true;
          mockDevIcons = true;
          modules.icons = {
            style = "glyph";
          };
        };

        telescope = {
          enable = true;
          settings = {
            defaults = {
              mappings = {
                i = {
                  "<esc>" = {
                    __raw = "require('telescope.actions').close";
                  };
                };
              };
            };
            extensions = { };
          };
        };
        telescope.extensions = {
          frecency.enable = true;
          ui-select.enable = true;
        };

        bufferline = {
          enable = true;
          settings = {
            options.diagnostics = "nvim_lsp";
            highlights.indicator_selected.fg = "#8BB2C1";
          };
        };

        conform-nvim = {
          enable = true;
          settings = {
            format_on_save = # Lua
              ''

                	       function(bufnr)
                	if not _G.slow_format_filetypes then
                	           _G.slow_format_filetypes = {}
                	         end
                	         if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
                	           return
                	         end

                	         if slow_format_filetypes[vim.bo[bufnr].filetype] then
                	           return
                	         end

                	         local function on_format(err)
                	           if err and err:match("timeout$") then
                	             slow_format_filetypes[vim.bo[bufnr].filetype] = true
                	           end
                	         end

                	         return { timeout_ms = 200, lsp_fallback = true }, on_format
                	        end
              '';
            formatters = {
              injected = {
                options = {
                  ignore_errors = true;
                  lang_to_ft = {
                    bash = "sh";
                  };
                  interpolation_queries = {
                    nix = "((interpolation) @interp)";
                  };

                };
              };
              shellcheck = {
                command = lib.getExe pkgs.shellcheck;

              };
              shfmt = {
                command = lib.getExe pkgs.shfmt;
              };
              stylua = {
                command = lib.getExe pkgs.stylua;
              };
              codespell = {
                command = lib.getExe pkgs.codespell;
              };
              nixfmt = {
                command = lib.getExe pkgs.nixfmt;
              };
              prettier = {
                command = lib.getExe pkgs.prettier;
              };
            };
            formatters_by_ft = {
              fish = [ "fish_indent" ];
              bash = [ "shfmt" ];
              sh = [ "shfmt" ];
              lua = [ "stylua" ];
              nix = [
                "injected"
                "nixfmt"
              ];
              javascript = [
                "prettierd"
                "prettier"
              ];
            };
          };
        };

        which-key.enable = true;
        gitsigns = {
          enable = true;
          settings.signs = {
            add.text = "│";
            change.text = "│";
            changedelete.text = "~";
            delete.text = "_";
            topdelete.text = "‾";
          };
        };
        neogit.enable = true;
        colorizer.enable = true;
        notify.enable = true;
        todo-comments.enable = true;
        tmux-navigator.enable = true;
        nvim-autopairs = {
          enable = true;
          settings.disable_filetype = [
            "TelescopePrompt"
            "vim"
            "haskell"
            "ps1"
          ];
        };
        snacks = {
          enable = true;
          settings = {
            bigfile = {
              enabled = true;
            };
            notifier = {
              enabled = true;
              timeout = 3000;
            };
            quickfile = {
              enabled = false;
            };
            statuscolumn = {
              enabled = false;
            };
            words = {
              debounce = 100;
              enabled = true;
            };
          };
        };
        indent-blankline = {
          enable = true;
          settings = {
            scope = {
              enabled = true;
              show_end = false;
              exclude.language = [
                "packer"
                "dashboard"
                "telescope"
              ];
            };
          };
        };
        distant.enable = true;
        presence = {
          enable = true;
          settings.main_image = "file";
        };
        wakatime.enable = true;

        cmp = {
          enable = true;
          autoEnableSources = true;
          settings = {
            experimental = {
              ghost_text = true;
              native_menu = false;
            };
            snippet.expand.__raw = "function(args) require('luasnip').lsp_expand(args.body) end";
            sources = [
              { name = "luasnip"; }
              { name = "buffer"; }
              { name = "path"; }
              { name = "nvim_lua"; }
              { name = "copilot"; }
            ];
          };
        };

        treesitter = {
          enable = true;

          languageRegister = {
            "commonlisp" = [ "lisp" ];
          };
          settings = {
            highlight.enable = true;
            autopairs.enable = true;
            query_linter.enable = true;
            textobjects = {
              enable = true;
              lookahead = true;
              keymaps = {
                af = "@function.outer";
                "if" = "@function.inner";
                ac = "@class.outer";
                ic = "@class.inner";
              };
              move = {
                enable = true;
                set_jumps = true;
              };
            };
          };
        };
        twilight.enable = true;
        lualine = {
          enable = true;
          settings = {
            options = {
              theme = "auto";
              globalstatus = true;

              component_separators = {
                left = "";
                right = "";
              };

              section_separators = {
                left = "";
                right = "";
              };

              disabled_filetypes = {
                statusline = [
                  "NvimTree"
                  "dbui"
                  "packer"
                  "startify"
                  "NeogitStatus"
                  "fugitive"
                  "fugitiveblame"
                  "telescope"
                ];
              };
            };

            sections = {
              lualine_a = [
                {
                  __unkeyed-1 = "mode";

                  icon = "󰊠";

                  separator = {
                    left = "";
                    right = "";
                  };
                }
              ];

              lualine_b = [
                {
                  __unkeyed-1 = "filename";

                  path = 1;

                  symbols = {
                    modified = "[+]";
                    readonly = "[-]";
                    unnamed = "[No Name]";
                  };
                }

                {
                  __unkeyed-1 = "filesize";

                  cond = {
                    __raw = ''
                      function()
                        return vim.fn.getfsize(vim.fn.expand("%:p")) > 0
                      end
                    '';
                  };

                  separator = {
                    right = "";
                  };
                }
              ];

              lualine_x = [
                {
                  __unkeyed-1 = "diagnostics";

                  sources = [ "nvim_diagnostic" ];

                  sections = [
                    "error"
                    "warn"
                    "info"
                    "hint"
                  ];

                  symbols = {
                    error = " ";
                    warn = " ";
                    info = " ";
                    hint = "󰌵 ";
                  };
                }

                {
                  __unkeyed-1 = "lsp_status";
                }
              ];

              lualine_y = [
                {
                  __unkeyed-1 = "branch";
                }

                {
                  __unkeyed-1 = "diff";

                  symbols = {
                    added = " ";
                    modified = " ";
                    removed = " ";
                  };
                }
              ];

              lualine_z = [
                {
                  __unkeyed-1 = "progress";
                }

                {
                  __unkeyed-1 = "location";

                  separator = {
                    left = "";
                    right = "";
                  };
                }
              ];
            };

            inactive_sections = {
              lualine_a = [ ];

              lualine_b = [
                {
                  __unkeyed-1 = "filetype";
                }
              ];

              lualine_c = [
                {
                  __unkeyed-1 = "filename";
                  path = 1;
                }
              ];

              lualine_x = [ ];
              lualine_y = [ ];
              lualine_z = [ ];
            };

            extensions = [
              "fugitive"
              "nvim-tree"
              "quickfix"
              "toggleterm"
            ];
          };
        };
        nvim-lightbulb.enable = true;

        dap = {
          enable = true;
        };
        dap-ui = {
          enable = true;
        };
        lsp.enable = true;
      };
      luaLoader.enable = true;

      extraPlugins = with pkgs.vimPlugins; [
        plenary-nvim
        nui-nvim
        sqlite-lua

        cmp_luasnip
        cmp-buffer
        cmp-path
        cmp-cmdline
        cmp-nvim-lua
        cmp-nvim-lsp
        cmp-dictionary
        cmp-copilot
        crates-nvim
        luasnip

        vim-better-whitespace
        mkdir-nvim
        copilot-lua
        vim-wakatime

        vim-nix
        vim-fish
        vim-glsl
        yuck-vim
        purescript-vim
        dhall-vim
        zig-vim
        idris2-vim
        idris2-nvim
        kotlin-vim
        agda-vim
        conjure
        vim-racket

        parinfer-rust
      ];

      extraConfigLua = ''
        -- Keep fold settings aligned with previous treesitter config
        vim.wo.foldmethod = "expr"
        vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
        vim.wo.foldlevel = 3

        -- Match previous guifont behavior (Windows vs other)
        if vim.fn.has("win32") > 0 then
        	vim.opt.guifont = "FiraCode Nerd Font:h13"
        else
        	vim.opt.guifont = "FiraCode Nerd Font:h10"
        end

        -- Telescope: load extensions used previously
        -- Conform: keep :Format user command with range support

        -- DAP UI setup (your config calls dapui.setup())
      '';

      extraFiles = {
        "colors/pink_ocean.vim".source = builtins.path {
          path = ../../../../.config/nvim/colors/pink_ocean.vim;
          name = "pink_ocean.vim";
        };
      };
    };
}
