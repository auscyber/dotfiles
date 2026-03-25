{
  config,
  pkgs,
  system,
  lib,
  modulesPath,
  ...
}:
let
  cfg = config.auscybernix.shell.zsh;
in
{
  options.auscybernix.shell.zsh = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Zsh shell configuration.";
    };
  };
  config = lib.mkIf cfg.enable {
    programs.eza.enable = true;
    home.packages = with pkgs; [ bat ];
    home.file."Music/Phoebe/lyricslist" = {
      source = ../../phoebelyrics/lyricslist;
    };
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
      plugins =
        builtins.map
          (package: {
            name = package.pname;
            inherit (package) src;
          })
          (
            with pkgs;
            [
              zsh-autosuggestions
              nix-zsh-completions
              zsh-completions
              zsh-nix-shell
              zsh-syntax-highlighting
            ]
          );
      shellAliases = {
        ghc = "stack exec -- ghc";
        fzf = "fzf --reverse --height 40%";
        vim = "nvim";
        cat = "bat";
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

        export PATH=$PATH:~/.cabal/bin:~/go/bin:~/.emacs.d/bin:~/.local/bin:~/.dotnet/tools:/usr/sbin:/snap/bin:$NPM_PACKAGES/bin:~/.luarocks/bin:/usr/local/go/bin:$DENO_INSTALL/bin:/opt/jdk8u292-b10:$IDRIS_PREFIX/bin
        fetch -s
        eval "$(starship init zsh)"
                    	'';
    };
  };
}
