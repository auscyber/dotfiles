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
      bat
      starship
    ];

    programs.fish = {
      enable = true;

      shellAliases = {
        #      ghc = "stack exec -- ghc";
        fzf = "fzf --reverse --height 40%";
        vim = "nvim";
        cat = "bat";
        e = "vim";
        #alias ng="nvim -c ':Neogit'"
        ls = "exa --icons --git";
        ll = "ls -la";
        t = "tmux";
        grep = "grep --color=auto";
        hm = "home-manager ";

      };
      interactiveShellInit = ''
        		set fish_greeting
                    fetch -s
              	  starship init fish | source
                                	'';
    };
  };
}
