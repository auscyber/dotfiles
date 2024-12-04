{ config, pkgs, system, lib, modulesPath, ... }:
{
  programs.exa.enable = true;
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    history = {
      size = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
    };
    sessionVariables = {
      MANPATH = "\${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man";

      EDITOR = "nvim";
      editor = "$EDITOR";
      BROWSER = "firefox";
      GTK2_RC_FILES = "$HOME/.gtkrc-2.0";
      _JAVA_AWT_WM_NONREPARENTING = 1;
      WLR_NO_HARDWARE_CURSORS = 1;

    };
    zplug = {
      enable = true;
      plugins = [
        {
          name = "zsh-users/zsh-autosuggestions";
        }
        { name = "spwhitt/nix-zsh-completions"; }
        { name = "zsh-users/zsh-completions"; }
        { name = "chisui/zsh-nix-shell"; }
        { name = "zsh-users/zsh-syntax-highlighting"; }


      ];
    };
    shellAliases = {
      ghc = "stack exec -- ghc";
      fzf = "fzf --reverse --height 40%";
      vim = "nvim";
      e = "vim";
      #alias ng="nvim -c ':Neogit'"
      ls = "exa --icons --git";
      ll = "ls -la";
      t = "tmux";
      grep = "grep --color=auto";
      windows = "sudo grub-reboot 2 && sudo reboot";
      hm = "home-manager --flake $NIXFLAKE#$FLAKENAME";

    };
    initExtra = ''
      export PATH=$PATH:~/.cabal/bin:~/go/bin:~/.emacs.d/bin:/home/auscyber/.local/bin:~/.dotnet/tools:/usr/sbin:/snap/bin:$NPM_PACKAGES/bin:~/.luarocks/bin:/usr/local/go/bin:$DENO_INSTALL/bin:/opt/jdk8u292-b10:$IDRIS_PREFIX/bin
      fetch -s
      eval "$(starship init zsh)"
                  	'';
  };
}


