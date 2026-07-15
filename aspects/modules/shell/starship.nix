{

  den.aspects.starship.homeManager = { user, ... }: {
    programs.starship = {
      enable = true;
      settings = {
        format = ''
          $username$hostname$status$directory$shell$rust$package$cmd_duration$vcs_status$nix_shell$haskell$purescript$python$julia$lua$golang$docker_context$package$fennel
          $character'';

        scan_timeout = 10;

        shell = {
          disabled = false;
          powershell_indicator = "[pwsh](blue)";
          zsh_indicator = "[zsh](#8BB2C1)";
          fish_indicator = "[fsh](#CF9FFF)";
        };
        custom.jj = {
          when = true;

          command = "{ jj log -r 'closest_bookmark(@)' -T '\"  \" ++ bookmarks ++ \" \"' --no-graph; jj log -r @ -T prompt --no-graph --ignore-working-copy; } | tr '\\n' ' '";

          format = " $output ";
          ignore_timeout = true;
          shell = "bash";
        };

        custom.vcs_status = {
          when = true;

          command = "jj root > /dev/null 2>&1; [[ $? -eq 0 ]] && starship module custom.jj || starship module git_status";

          style = "fg:#6b3d99 bg:#ffff66";
          format = "[ $output ]($style)";
          shell = "bash";
        };
        git_status = {
          style = "fg:#6b3d99 bg:#ffff66";
          # format = '[($all_status$ahead_behind)]($style)'
          format = "$all_status$ahead_behind";
          disabled = true;
        };

        character = {
          success_symbol = "[λ](bold 195)";
          error_symbol = "[λ](bold white)";
        };

        hostname = {
          format = "[$hostname](bold green) ";
          ssh_only = true;
        };

        username = {
          style_user = "white";
          style_root = "red bold";
          format = "[$user]($style) ";
          disabled = false;
          show_always = false;
        };

        aws = {
          symbol = " ";
        };

        battery = {
          full_symbol = "";
          charging_symbol = "";
          discharging_symbol = "";
        };

        conda = {
          symbol = " ";
        };

        dart = {
          symbol = " ";
        };

        directory = {
          read_only = " ";
        };

        elixir = {
          symbol = " ";
        };

        elm = {
          symbol = " ";
        };

        git_branch = {
          symbol = " ";
        };

        golang = {
          symbol = " ";
        };

        hg_branch = {
          symbol = " ";
        };

        java = {
          symbol = " ";
        };

        julia = {
          symbol = " ";
        };

        memory_usage = {
          symbol = " ";
        };

        nim = {
          symbol = " ";
        };

        nix_shell = {
          symbol = "❄️ ";
          impure_msg = "[impure](bold red)";
          pure_msg = "[pure](bold green)";
          format = "via [$symbol$state( \\($name\\))](bold blue) ";
        };

        nodejs = {
          symbol = " ";
        };

        package = {
          symbol = " ";
        };

        perl = {
          symbol = " ";
        };

        php = {
          symbol = " ";
        };

        python = {
          symbol = " ";
        };

        ruby = {
          symbol = " ";
        };

        rust = {
          symbol = " ";
        };

        swift = {
          symbol = "ﯣ ";
        };

      };
    };
  };
}
