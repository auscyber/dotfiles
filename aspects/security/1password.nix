{
  den,
  inputs,
  lib,
  ...
}:
{
  ff.op-shell-plugins.url = "github:1Password/shell-plugins";

  den.aspects.gui = {
    homeManager =
      {
        pkgs,
        config,
        ...
      }:
      {
        launchd.agents.set-in-gui = {
          enable = true;
          config = {
            ProgramArguments = [
              "/bin/launchctl"
              "setenv"
              "IN_GUI"
              "yes"
            ];
            RunAtLoad = true;
            LimitLoadToSessionType = "Aqua";
          };
        };
        systemd.user.services.set-in-gui = {
          Unit.PartOf = [ "graphical-session.target" ];
          Install.WantedBy = [ "graphical-session.target" ];
          Service = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = "${pkgs.systemd}/bin/systemctl --user set-environment IN_GUI=yes";
          };
        };
      };
  };

  den.aspects.onepassword = {
    includes = [
      (den.batteries.unfree [ "onepassword-password-manager" ])

      # Zen ships the 1Password addon only because 1Password is in play — so the
      # dependency belongs here, not in the browser aspect. Fires only on
      # entities that actually resolved the zen aspect.
      (den.lib.whenAspect den.aspects.browsers.zen {
        homeManager = { pkgs, ... }: {
          programs.zen-browser._internalProfile.extensions.packages = [
            pkgs.firefox-addons.onepassword-password-manager
          ];
        };
      })
    ];
    gui = {
      provides.to-hosts.os.programs._1password-gui.enable = true;

      provides.to-users.homeManager =
        {
          pkgs,
          config,
          ...
        }:
        let
          _1passwordSocket =
            if pkgs.stdenv.isDarwin then
              "${config.home.homeDirectory}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
            else
              "${config.home.homeDirectory}/.1password/agent.sock";
        in
        {
          imports = [ inputs.op-shell-plugins.hmModules.default ];
          config = lib.mkMerge [
            {
              programs.zsh.initExtra = ''
                [[ -n "$IN_GUI"  ]] && export SSH_AUTH_SOCK="${_1passwordSocket}"
              '';

              programs.bash.initExtra = ''
                [[ -n "$IN_GUI"  ]] && export SSH_AUTH_SOCK="${_1passwordSocket}"
              '';

              programs.fish.interactiveShellInit = ''
                if test -n "$IN_GUI"
                  set -gx SSH_AUTH_SOCK "${_1passwordSocket}"
                end
              '';

              home.packages = [ pkgs._1password-cli ];
              programs._1password-shell-plugins = {
                # enable 1Password shell plugins for bash, zsh, and fish shell
                enable = true;
                # the specified packages as well as 1Password CLI will be
                # automatically installed and configured to use shell plugins
                plugins = with pkgs; [
                  gh
                  glab
                  #            wrangler

                  awscli2
                ];
              };
            }
          ];
        };

      provides.to-users.hmDarwin = {
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
      #    programs._1password.enable = true;
    };
  };
}
