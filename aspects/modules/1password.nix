{ inputs, ... }:
{

  ff.op-shell-plugins.url = "github:1Password/shell-plugins";

  den.aspects.onepassword = {
    gui = {
      darwin.programs._1password-gui.enable = true;
      hm = { pkgs, ... }: {
        imports = [ inputs.op-shell-plugins.homeModules.default ];
        programs._1password-shell-plugins = {
          # enable 1Password shell plugins for bash, zsh, and fish shell
          enable = true;
          # the specified packages as well as 1Password CLI will be
          # automatically installed and configured to use shell plugins
          plugins = with pkgs; [
            gh
            glab
            #        wrangler

            awscli2
          ];
        };

      };
      hmDarwin = {
        programs = {
          git.signing = {
            format = "ssh";
            signByDefault = true;
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOuXMdca6Lz0Rxz+EmKy/cSXuBev6knlsdKzm7R5D4E1";
            signer = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";

          };
          jujutsu.settings = {
            signing = {
              behavior = "drop";
              backend = "ssh";
              key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOuXMdca6Lz0Rxz+EmKy/cSXuBev6knlsdKzm7R5D4E1";

              backends.ssh.program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";

            };
            git = {
              sign-on-push = true;
            };
          };
        };
      };

    };
    homeManager = {
      programs.onepassword.enable = true;
    };
  };
}
