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
          # The language servers are deliberately absent here: `lsp.*.binary.path` below
          # points at the lspmux shims, and each shim resolves the real server off $PATH
          # at runtime. Putting the pinned servers on $PATH would win that lookup over
          # the ones a project's devshell provides.
          extraPackages = with pkgs; [
            nodejs
            yarn
            ripgrep
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
            # Spawn the servers through the lspmux shims, so zed and nvim share one
            # server instance per project instead of running one each. `path_lookup` is
            # off: the shim does the $PATH lookup itself, and unlike zed it knows to
            # skip itself and to fall back to a pinned server when the project has none.
            lsp =
              let
                inherit (pkgs) lspmuxed;
                binary = server: {
                  binary = {
                    path = lib.getExe server;
                    arguments = server.passthru.lspmux.args;
                  };
                };
              in
              {
                rust-analyzer = binary lspmuxed.rust_analyzer;
                nix = binary lspmuxed.nil_ls;
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
