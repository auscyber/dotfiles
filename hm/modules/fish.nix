{
  config,
  pkgs,
  system,
  lib,
  modulesPath,
  ...
}:
{
  home.packages = with pkgs; [ bat ];
  home.file."Music/Phoebe/lyricslist" = {
    source = ../../phoebelyrics/lyricslist;
  };
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
}
