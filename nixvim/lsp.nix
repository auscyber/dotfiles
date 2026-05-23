{ config, pkgs, ... }:
{

  plugins = {
    lsp-status.enable = true;

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
          autostart = false;
          filetypes = [
            "typescriptreact"
            "typescript"
            "javascript"
          ];
        };

        denols = {
          enable = true;
          autostart = false;
          filetypes = [ "typescript" ];
        };

        hls = {
          enable = true;
          filetypes = [ "haskell" ];
installGhc = true;

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
          "<space>r" = "references";
          "<leader>qf" = "code_action";
        };
        extra = [
          {
            key = "<leader>rn";
            action = {
              __raw = "require('renamer').rename";
            };
          }
        ];
      };

      onAttach =
        # lua
        ''
                      require("renamer").setup({})




                      if client.server_capabilities.documentHighlightProvider then
                        vim.api.nvim_create_augroup(
                          "lsp_document_highlight",
                          { clear = true }
                        )

                        vim.api.nvim_create_autocmd(
                          "CursorHold",
                          {
                            buffer = bufnr,
                            callback = vim.lsp.buf.document_highlight
                          }
                        )

                        vim.api.nvim_create_autocmd(
                          "CursorMoved",
                          {
                            buffer = bufnr,
                            callback = vim.lsp.buf.clear_references
                          }
                        )
                      end
        '';

    };

    cmp = {
      enable = true;

      settings = {
        completion = {
          completeopt = "menu,menuone,noselect";
        };

        sources = [
          {
            name = "nvim_lsp";
            max_item_count = 1000;
          }
        ];
      };
    };

    lspkind = {
      enable = true;
      cmp.enable = true;
    };

    which-key.enable = true;

    lsp-signature.enable = true;

    rustaceanvim = {
      enable = true;

      settings = {
        server = {
          default_settings = {
            rust-analyzer = {
              checkOnSave.command = "clippy";

              procMacro.enable = true;
            };
          };
        };
      };
    };

    none-ls.enable = true;
  };

  extraPlugins = with pkgs.vimPlugins; [
    renamer-nvim
    #    virtualtypes-nvim
    lsp-status-nvim
    nvim-lightbulb
  ];

  extraConfigLua = ''
    vim.lsp.set_log_level("debug")

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
}
