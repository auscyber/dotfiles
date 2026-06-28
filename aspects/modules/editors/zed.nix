{ den, lib, ... }:
{
  den.aspects.zed = {
    homeManager =
      { pkgs, ... }:
      {
        programs.zed-editor = {
          enable = true;
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
      };
  };
}
