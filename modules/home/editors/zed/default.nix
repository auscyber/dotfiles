{
  config,
  pkgs,
  lib,
  ...
}:

with lib;
let
  cfg = config.auscybernix.editors.zed;
in
{
  options.auscybernix.editors.zed = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable Zed editor.";
    };
  };

  config = mkIf cfg.enable {
    sops.secrets."wakatime_config" = {
      path = "${config.home.homeDirectory}/.wakatime.cfg";
    };
    programs.zed-editor = {
      enable = true;

      ## This populates the userSettings "auto_install_extensions"
      extensions = [
        "wakatime"
        "nix"
        "toml"
        "lua"
        "make"
      ];
      extraPackages = with pkgs; [
        nodejs
        yarn
        rust-analyzer
        ripgrep
        nil

      ];

      ## everything inside of these brackets are Zed options.
      userSettings = {

        assistant = {
          enabled = true;
          version = "2";
          default_open_ai_model = null;
          ### PROVIDER OPTIONS
          ### zed.dev models { claude-3-5-sonnet-latest } requires github connected
          ### anthropic models { claude-3-5-sonnet-latest claude-3-haiku-latest claude-3-opus-latest  } requires API_KEY
          ### copilot_chat models { gpt-4o gpt-4 gpt-3.5-turbo o1-preview } requires github connected
          default_model = {
            provider = "copilot_chat";
            model = "claude-3-5-sonnet-latest";
          };

          #                inline_alternatives = [
          #                    {
          #                        provider = "copilot_chat";
          #                        model = "gpt-3.5-turbo";
          #                    }
          #                ];
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
          detect_venv = {
            on = {
              directories = [
                ".env"
                "env"
                ".venv"
                "venv"
              ];
              activate_script = "default";
            };
          };
          env = {
            TERM = "alacritty";
          };
          font_family = "FiraCode Nerd Font";
          font_size = null;
          line_height = "comfortable";
          option_as_meta = false;
          button = false;
          shell = "system";
          inlay_hints.enabled = true;
          #{
          #                    program = "zsh";
          #};
          toolbar = {
            title = true;
          };
          working_directory = "current_project_directory";
        };

        lsp = {

          rust-analyzer = {

            binary = {
              #                        path = lib.getExe pkgs.rust-analyzer;
              path_lookup = true;
            };
          };
          nix = {
            binary = {
              path_lookup = true;
            };
          };

        };

        languages = {
        };

        vim_mode = true;
        ## tell zed to use direnv and direnv can use a flake.nix enviroment.
        load_direnv = "shell_hook";
        base_keymap = "VSCode";
        show_whitespaces = "all";

      };

    };
  };
}
