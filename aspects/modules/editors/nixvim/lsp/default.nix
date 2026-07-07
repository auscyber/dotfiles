{

  den.aspects.nixvim.nvim = { config, pkgs, ... }: {

    plugins = {
      lsp = {
        enable = true;
        servers = {
          tailwindcss = {
            enable = true;
            filetypes = [
              "html"
              "scss"
              "htmldjango"
              "css"
              "typescriptreact"
              "javascriptreact"
              "javascript"
              "typescript"
              "svelte"
              "vue"
            ];
          };

          ts_ls = {
            enable = true;
            autostart = true;
            filetypes = [
              "typescriptreact"
              "typescript"
              "javascript"
            ];
          };

          #      denols = {
          #        enable = true;
          #        autostart = false;
          #        filetypes = [ "typescript" ];
          #      };
          #
          hls = {
            enable = true;
            filetypes = [ "haskell" ];
            installGhc = false;

            settings = {
              haskell = {
                formattingProvider = "fourmolu";
              };
            };
          };

          gopls.enable = true;

          lua_ls = {
            enable = true;
            filetypes = [ "lua" ];
            settings = {
              runtime = {
                version = "LuaJIT";
              };
              workspace = {
                preloadFileSize = 10000;
                library = [
                  { __raw = "vim.env.VIMRUNTIME"; }
                  { __raw = ''vim.api.nvim_get_runtime_file("lua/lspconfig", true)''; }
                ];
              };
            };
          };

          clangd = {
            enable = true;

            filetypes = [
              "c"
              "cpp"
            ];

            settings = {
              initOptions = {
                clangdFileStatus = true;
              };
            };
          };

          nil_ls = {
            enable = true;

            filetypes = [ "nix" ];

            settings = {
              nil = {
                nix = {
                  flake = {
                    autoArchive = true;
                  };
                };
              };
            };
          };

          ocamllsp.enable = true;

          pyright = {
            enable = true;

            settings = {
              python = {
                workspaceSymbols = {
                  enable = true;
                };
              };
            };
          };

          docker_compose_language_service.enable = true;
          zls.enable = true;
          metals.enable = true;
          dhall_lsp_server.enable = true;
          #        purescriptls.enable = true;
          #        powershell_es.enable = true;
          kotlin_language_server.enable = true;
          #        omnisharp.enable = true;
          #        als.enable = true;

          jdtls = {
            enable = true;

            cmd = [ "jdtls" ];

            #          rootDir = # lua
            #
            #            ''
            #              require("lspconfig.util").root_pattern(
            #                "pom.xml",
            #                "gradle.build",
            #                ".git"
            #              )(fname) or vim.fn.getcwd()
            #            '';
          };
        };
        inlayHints = true;

        keymaps = {
          silent = true;
          diagnostic = {
            "<space>d" = "open_float";
          };

          lspBuf = {
            "gd" = "definition";
            "gD" = "declaration";
            "gi" = "implementation";
            "K" = "signature_help";
            "KK" = "hover";
            "<space>r" = "references";
            "<leader>qf" = "code_action";
            "<leader>rn" = "rename";
          };

        };

      };
      otter.enable = true;

      lsp-status.enable = true;
      lsp-progress.enable = true;

      cmp = {
        enable = true;
        autoEnableSources = true;

        settings = {
          mapping = {
            "<C-Space>" = # lua
              "cmp.mapping.complete()";
            "<C-d>" =
              # lua
              "cmp.mapping.scroll_docs(-4)";
            "<C-e>" = "cmp.mapping.close()";
            "<C-f>" = "cmp.mapping.scroll_docs(4)";
            "<Up>" = # lua
              "cmp.mapping.select_prev_item()";
            "<Down>" = "cmp.mapping.select_next_item()";
            "<CR>" = "cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace,  select = true })";
            "<S-Tab>" = # lua
              ''
                  function (fallback)
                local luasnip = require("luasnip")
                  if cmp.visible() then
                    cmp.select_prev_item()
                  elseif luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                  else
                    fallback()
                  end
                  end
              '';
            "<Tab>" = # lua
              ''
                function (fallback)
                local luasnip = require("luasnip")
                  if cmp.visible() then
                    cmp.select_next_item()
                  elseif luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                  else
                    fallback()
                end
                end
              '';
          };
          completion = {
            completeopt = "menu,menuone,noselect";
          };

          sources = [
            {
              name = "nvim_lsp";
              max_item_count = 1000;
            }
            { name = "path"; }
            { name = "nvim_lsp_signature_help"; }
            { name = "nvim_lsp_document_symbol"; }
            { name = "buffer"; }
          ];
        };
      };

      lspkind = {
        enable = true;
        cmp.enable = true;
      };

      which-key.enable = true;

      lsp-signature.enable = true;
      eagle = {
        enable = true;
        settings = {
          keyboard_mode = true;
        };
      };

      none-ls.enable = true;
    };

    extraPlugins = with pkgs.vimPlugins; [
      renamer-nvim
      #    virtualtypes-nvim
      nvim-lightbulb
    ];

    extraConfigLua = # lua
      ''
        -- lua
        vim.lsp.handlers["window/showMessage"] = function(err, result, ctx, config)
        	local MessageType = vim.lsp.protocol.MessageType

        	local map = {
        		[MessageType.Error] = vim.log.levels.ERROR,
        		[MessageType.Warning] = vim.log.levels.WARN,
        		[MessageType.Info] = vim.log.levels.INFO,
        		[MessageType.Log] = vim.log.levels.TRACE,
        	}

        	vim.notify(result.message, map[result.type])
        end
      '';
  };
}
