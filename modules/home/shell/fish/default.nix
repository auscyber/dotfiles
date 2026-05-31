{
  config,
  pkgs,
  system,
  lib,
  modulesPath,
  ...
}:
let
  cfg = config.auscybernix.shell.fish;
in
{
  options.auscybernix.shell.fish = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Fish shell configuration.";
    };
  };

  config = lib.mkIf cfg.enable {

    home.packages = with pkgs; [
      nix-output-monitor
      bat
      starship
    ];

    programs.fish = {
      enable = true;

      interactiveShellInit =
        # fish
        ''
          function bind_bang
              switch (commandline -t)[-1]
                  case "!"
                      commandline -t -- $history[1]
                      commandline -f repaint
                  case "*"
                      commandline -i !
              end
          end

          function bind_dollar
              switch (commandline -t)[-1]
                  case "!"
                      commandline -f backward-delete-char history-token-search-backward
                  case "*"
                      commandline -i '$'
              end
          end

          function fish_user_key_bindings
              bind ! bind_bang
              bind '$' bind_dollar
          end
          set fish_greeting
          fetch -s
          #          starship init fish | source
                                                    	'';
    };
  };
}
