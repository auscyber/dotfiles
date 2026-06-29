{ den, __findFile, ... }:
{
  den.aspects.shell = {
    includes = [
      den.aspects.starship
      den.aspects.jujutsu
    ];
    os = { pkgs, ... }: {
      environment.systemPackages = with pkgs; [
        git
        vim
        neovim
        wget
        htop
        fzf
        curl
      ];
    };

    provides.to-users.homeManager = { pkgs, ... }: {
      programs.zoxide.enable = true;
      programs.fzf.enable = true;
      programs.bat.enable = true;
      programs = {
        ssh = {
          enable = true;
          enableDefaultConfig = false;
          settings = {
            "imflo.pet" = {
              forwardAgent = true;
            };
          };

        };
        direnv = {
          enable = true;
          nix-direnv = {
            enable = true;
          };
        };
        home-manager.enable = true;
        gpg = {

          enable = true;
        };

        git = {
          enable = true;
          settings = {
            user = {
              name = "Ivy Pierlot";
              email = "ivyp@outlook.com.au";
            };
          };
        };
        eza = {
          enable = true;
          git = true;
          icons = "auto";
        };

        nix-your-shell = {
          enable = true;
          nix-output-monitor.enable = true;
        };
        nh.enable = true;
      };
      home.packages = with pkgs; [ ivy-fetch ];
      home.shellAliases = {
        #      ghc = "stack exec -- ghc";
        fzf = "fzf --reverse --height 40%";
        vim = "nvim";
        cat = "bat";
        e = "vim";
        #alias ng="nvim -c ':Neogit'"
        ls = "eza --icons --git";
        ll = "ls -la";
        t = "tmux";
        grep = "grep --color=auto";
        hm = "home-manager ";
      };
    };
  };

}
