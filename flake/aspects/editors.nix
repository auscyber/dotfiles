{ den, ... }:
{
  # ── Editors aspect ────────────────────────────────────────────────────────────
  # Home-manager: Neovim, Zed, Emacs, Kakoune.
  # Include in a user aspect; enable individual editors via the option flags.
  den.aspects.editors = {
    homeManager =
      { config, lib, pkgs, ... }:
      {
        options.auscybernix.programs.neovim.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Neovim with custom configuration.";
        };
        options.auscybernix.editors.zed.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Zed editor.";
        };
        options.auscybernix.editors.emacs.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Emacs editor.";
        };
        options.auscybernix.editors.kakoune.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Kakoune editor.";
        };

        config = lib.mkMerge [
          # ── Neovim ──────────────────────────────────────────────────────
          (lib.mkIf config.auscybernix.programs.neovim.enable {
            age.secrets."wakatime_config" = {
              rekeyFile = ../../secrets/wakatime_config.age;
              path = "${config.home.homeDirectory}/.wakatime.cfg";
            };
            home.file.".config/nvim".source =
              config.lib.file.linkLocalPath ../../.config/nvim;
            home.packages = with pkgs; [
              (wrapNeovimUnstable neovim {
                wrapRc = false;
                vimAlias = true;
                withNodeJs = true;
              })
            ];
            home.sessionVariables = {
              vim = "nvim";
              EDITOR = "nvim";
              editor = "$EDITOR";
            };
            home.file."${config.auscybernix.flakeConfig.flakeFolder}/.config/nvim/lua/treesitter_compiler.lua".text = ''
              return "${pkgs.stdenv.cc}/bin/cc"
            '';
          })

          # ── Zed ─────────────────────────────────────────────────────────
          (lib.mkIf config.auscybernix.editors.zed.enable {
            programs.zed-editor = {
              enable = true;
              extensions = [ "wakatime" "nix" "toml" "lua" "make" ];
              extraPackages = with pkgs; [
                nodejs yarn rust-analyzer ripgrep nil
              ];
              userSettings = {
                assistant = {
                  enabled = true;
                  version = "2";
                  default_open_ai_model = null;
                  default_model = {
                    provider = "copilot_chat";
                    model = "claude-3-5-sonnet-latest";
                  };
                };
                node = {
                  path = lib.getExe pkgs.nodejs;
                  npm_path = lib.getExe' pkgs.nodejs "npm";
                };
                hour_format = "hour24";
                auto_update = false;
                terminal = {
                  alternate_scroll = "off";
                  blinking = "off";
                  copy_on_select = false;
                  dock = "bottom";
                  detect_venv.on = {
                    directories = [ ".env" "env" ".venv" "venv" ];
                    activate_script = "default";
                  };
                  env.TERM = "alacritty";
                  font_family = "FiraCode Nerd Font";
                  font_size = null;
                  line_height = "comfortable";
                  option_as_meta = false;
                  button = false;
                  shell = "system";
                  inlay_hints.enabled = true;
                  toolbar.title = true;
                  working_directory = "current_project_directory";
                };
                lsp = {
                  rust-analyzer.binary.path_lookup = true;
                  nix.binary.path_lookup = true;
                };
                vim_mode = true;
                load_direnv = "shell_hook";
                base_keymap = "VSCode";
                show_whitespaces = "all";
              };
            };
          })

          # ── Emacs ────────────────────────────────────────────────────────
          (lib.mkIf config.auscybernix.editors.emacs.enable {
            programs.emacs = {
              enable = true;
              package = pkgs.emacsNativeComp;
            };
            services.emacs.enable = true;
          })

          # ── Kakoune ──────────────────────────────────────────────────────
          (lib.mkIf config.auscybernix.editors.kakoune.enable {
            programs.kakoune = {
              enable = true;
              plugins = with pkgs.kakounePlugins; [
                kak-lsp
                parinfer-rust
                kakoune-extra-filetypes
                powerline-kak
              ];
            };
          })
        ];
      };
  };
}
