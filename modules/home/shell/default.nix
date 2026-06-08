{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.auscybernix.shell;
in
{
  options.auscybernix.shell = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable custom shell configuration.";
    };

  };

  config = lib.mkIf cfg.enable {
    age.secrets = {
      attic_token = {
        rekeyFile = ../../../secrets/attickey.age;
      };
    };
    age.templates."attic-config" = {
      path = "${config.home.homeDirectory}/.config/attic/config.toml";
      dependencies = {
        inherit (config.age.secrets) attic_token;
      };
      content =
        { pkgs, placeholders, ... }:
        ''
          default-server = "central"
          [servers.central]
          endpoint = "https://cache.ivymect.in"
          token = "${placeholders.attic_token}"
        '';
    };
    home.packages = with pkgs; [
      attic-client
    ];
    services.gpg-agent = {
      extraConfig = ''
           allow-loopback-pinentry
           default-cache-ttl 600
        max-cache-ttl 7200
        ttyname $GPG_TTY
        enable-ssh-support
        debug-level 2

      '';
      enableScDaemon = true;
    };
    programs.starship = {
      enable = true;
      settings = {
        format = ''
          $username$hostname$status$directory$shell$rust$package$cmd_duration$git_status$nix_shell$haskell$purescript$python$julia$lua$golang$docker_context$package$fennel
          $character'';

        scan_timeout = 10;

        shell = {
          disabled = false;
          powershell_indicator = "[pwsh](blue)";
          zsh_indicator = "[zsh](#8BB2C1)";
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

        aws.symbol = " ";

        battery = {
          full_symbol = "";
          charging_symbol = "";
          discharging_symbol = "";
        };

        conda.symbol = " ";
        dart.symbol = " ";
        directory.read_only = " ";
        elixir.symbol = " ";
        elm.symbol = " ";
        git_branch.symbol = " ";
        golang.symbol = " ";
        hg_branch.symbol = " ";
        java.symbol = " ";
        julia.symbol = " ";
        memory_usage.symbol = " ";
        nim.symbol = " ";

        nix_shell = {
          symbol = "❄️ ";
          impure_msg = "[impure](bold red)";
          pure_msg = "[pure](bold green)";
          format = "via [$symbol$state( \\($name\\))](bold blue) ";
        };

        nodejs.symbol = " ";
        package.symbol = " ";
        perl.symbol = " ";
        php.symbol = " ";
        python.symbol = " ";
        ruby.symbol = " ";
        rust.symbol = " ";
        swift.symbol = "ﯣ ";
      };
    };
    programs.gh = {
      enable = true;
      #      package = pkgs.hello;
      settings = {
        git_protocol = "ssh";
      };

    };
    programs.zoxide.enable = true;
    programs.fzf.enable = true;
    programs.nix-your-shell = {
      enable = true;
      nix-output-monitor.enable = true;
    };
    programs.nh.enable = true;

    programs = {
      ssh = {

        enable = true;
        enableDefaultConfig = false;
        matchBlocks = {
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
    };

  };
}
