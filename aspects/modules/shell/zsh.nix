{ den, ... }:
{
  den.aspects.zsh = { user, ... }: {
    includes = [ den.aspects.shell ];

    homeManager =
      { config, pkgs, ... }:
      {
        programs.zsh = {
          enable = true;
          enableCompletion = true;
          syntaxHighlighting.enable = true;
          history = {
            size = 10000;
            path = "${config.xdg.dataHome}/zsh/history";
          };
          sessionVariables = {
            EDITOR = "nvim";
            GTK2_RC_FILES = "$HOME/.gtkrc-2.0";
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
          initContent = ''
            export PATH=$PATH:~/.cabal/bin:~/go/bin:~/.local/bin:~/.dotnet/tools:/usr/local/go/bin
          '';
        };
      };
  };
}
