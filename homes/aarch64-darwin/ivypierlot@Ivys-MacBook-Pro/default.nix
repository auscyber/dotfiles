# home.nix

{
  config,
  pkgs,
  inputs,
  lib,
  hostname,
  #  darwinConfig,
  ...
}:

{

  #  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICTsjq9lMzer6RPeDfXZ9eI1eiMf8b/fteSOb5XC5rBG";
  auscybernix.meta.description = "Home configuration for ${hostname}";
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMA2BIUJfAXT/4WGJZ+W9nMZfAYMHgjZ+RUqxOx7UWs7";
  #  services.gpg-agent.socketAddress =
  #    config.launchd.agents.gpg-agent.config.Sockets.Extra.SockPathName;

  services.yubikey-agent.enable = true;
  age.rekey.masterIdentities = [
    {
      identity = ../../../modules/common/age-yubikey.pub;
      pubkey = "age1yubikey1qv6zc6sjz4klkjxnnt2sv8ptlcjtmhphduu4rrqjuw88jn2nftuu6ep0kr3";

    }

  ];

  # Home Manager needs a bit of information about you and the paths it should
  # manage.

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  # # Adds the 'hello' command to your environment. It prints a friendly
  # # "Hello, world!" when run.
  # pkgs.hello

  # # It is sometimes useful to fine-tune packages, for example, by applying
  # # overrides. You can do that directly here, just don't forget the
  # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
  # # fonts?
  # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

  # # You can also create simple shell scripts directly inside your
  # # configuration. For example, this adds a command 'my-hello' to your
  # # environment:
  # (pkgs.writeShellScriptBin "my-hello" ''
  #   echo "Hello, ${config.home.username}!"
  # '')
  #  programs.ghostty.package = pkgs.nur.repos.DimitarNestorov.ghostty;
  imports = [ ./ui.nix ];
  programs.git.signing = {
    format = "ssh";
    signByDefault = true;
    key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOuXMdca6Lz0Rxz+EmKy/cSXuBev6knlsdKzm7R5D4E1";
    signer = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";

  };
  #  services.ollama = {
  #    enable = true;
  #    environmentVariables = {
  #      "OLLAMA_ORIGINS" = "moz-extension://*";
  #    };
  #
  #  };
  auscybernix = {
    services.mopidy.enable = false;
    wms.rift.enable = true;
    programs.sketchybar.enable = true;
    keybinds.kanata = {
      enable = true;
      extraPackages = [
        pkgs.jq
        pkgs.yabai
        pkgs.rift
      ];
      extraCommandPiping = ../../../modules/home/keybinds/kanata/config.toml;
      tray = {
          config = {
            "$schema" = "https://raw.githubusercontent.com/rszyma/kanata-tray/main/doc/config_schema.json";

            general = {
              allow_concurrent_presets = false; # (default: false)

              # Optional TCP control server to listen for remote commands, such as stopping/starting a preset.
              # Reference: https://github.com/rszyma/kanata-tray/blob/main/doc/control_server.md
              control_server_enable = true; # (default: false)
            };

            defaults = {
              #kanata_executable = '~/bin/kanata' # if empty or omitted, system $PATH will be searched.
              kanata_config = ""; # if empty or not omitted, kanata default config locations will be used.
              tcp_port = 5829; # (default: 5829)
              autorestart_on_crash = false; # (default: false)

              # Hooks allow running custom commands on specific events (e.g. starting preset).
              # Reference: https://github.com/rszyma/kanata-tray/blob/main/doc/hooks.md

              layer_icons = {
                frenchkeys = "french.ico";
                qwerty = "qwerty.ico";
                "*" = "other_layers.ico";
              };

              presets."main cfg" = {
                autorun = true;
                layer_icons = {
                  frenchkeys = "french.ico";
                };
                # kanata_executable = ''
              };
            };
          };
          # layer_icons = {  }
          # tcp_port = 1234
          # extra_args = ['-n', '-c=~/.config/kanata/another.kbd']

      };
      #      appBundleIds = [
      #        "app.zen-browser.zen"
      #        "com.1password.1password"
      #        "com.1password.1password-launcher"
      #      ];
      config = config.lib.file.getLocalPath ../../../kanata.kbd;
      extraConfigPaths = [
        #        (
        #          let
        #            splitStuff =
        #              splitted:
        #              builtins.filter (line: line != "") (
        #                lib.splitString "\n" (
        #                  builtins.readFile (
        #                    pkgs.runCommand "toJSON"
        #                      {
        #                        buildInputs = [
        #                          pkgs.perlPackages.JSON
        #                          pkgs.perl
        #                        ];
        #                      }
        #                      ''
        #                        echo '${
        #                          lib.replaceString " " "" splitted
        #                        }' | perl -CSD -0777 -ne 'use utf8; @m = /\X/gu; print join("\n",  @m);' | sed '/^[[:space:]]*$/d' > $out
        #                      ''
        #                  )
        #                )
        #              );
        #
        #            qwerty-fr = builtins.fromJSON (
        #              builtins.readFile (
        #                pkgs.runCommand "toJson" { } ''cat ${../../../qwerty-fr.yaml} | ${lib.getExe pkgs.yq} > $out''
        #              )
        #            );
        #            deadkeys = qwerty-fr.deadkeys;
        #
        #            layerMaps = builtins.map (
        #              layer:
        #              let
        #                base = splitStuff layer.base;
        #                alt = splitStuff layer.alt;
        #                zipped = lib.zipListsWith (
        #                  a: b: "((key-history ${a} 1)) ((unicode \"${b}\") (sequence-noerase 1)) break"
        #                ) base alt;
        #
        #              in
        #              {
        #                name = layer.name;
        #                char = builtins.elemAt (splitStuff layer.char) 1;
        #                output = ''
        #                  (switch
        #                  ${lib.concatStringsSep "\n" zipped}
        #                  _ (use-defsrc) break
        #                  )
        #                '';
        #              }
        #            ) deadkeys;
        #
        #          in
        #          "${pkgs.writeText "qwerty-fr.kbd" ''
        #            (defzippy
        #            ${../../../zippy.txt}
        #
        #              output-character--mappings  (
        #              ${lib.concatStringsSep "\n" (
        #                map (layer: ''
        #                  "${layer.char}" ${layer.output}
        #                '') layerMaps
        #              )}
        #            )
        #            )
        #
        #          ''}"
        #        )

      ];
    };
    programs.zotero.enable = true;
    browsers.zen-browser.enable = true;
    programs._1password-cli.enable = true;
    terms.ghostty.enable = true;

    shell = {
      enable = true;
      fish = {
        enable = true;
      };
    };
    programs.neovim.enable = true;
    editors.zed.enable = true;

  };
  home.packages = with pkgs; [
    nodejs
    desktoppr
    vscode
    pandoc
    discord
    texliveFull
    #    wezterm
    zotero
    #    prismlauncher
    mupdf
    #      (agda.withPackages
    #        (p: [
    #          p.standard-library
    #        ]))
  ];
  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };
  #  services.gpg-agent.socketAddress = "${config.launchd.agents.gpg-agent.config.Sockets.Extra.SockPathName}";

  programs.ssh = {
    enable = true;
    package = pkgs.openssh;
    matchBlocks = {
      "faggot.sh" = {
        forwardAgent = true;
        #        identityFile = "~/.ssh/id_ed25519.pub";
        extraOptions = {
          "RemoteForward" = " /run/user/1001/gnupg/S.gpg-agent ${config.services.gpg-agent.socketAddress} ";
        };

      };
	  "auspc" = {
	  forwardAgent = true;
	  host = "192.168.0.24";
	  user = "auscyber";
extraOptions = {
          "RemoteForward" = " /run/user/1000/gnupg/S.gpg-agent ${config.services.gpg-agent.socketAddress} ";
        };
	  };
      "secondpc" = {

        extraOptions = {
          "RemoteForward" = " /run/user/1000/gnupg/S.gpg-agent ${config.services.gpg-agent.socketAddress} ";
        };
      };
    };
  };
  # You can also manage environment variables but you will have to manually

  targets.darwin.defaults.NSGlobalDomain = {
    AppleIconAppearanceCustomTintColor = "0.593048 1.000000 0.728584 0.596341";
    AppleInterfaceStyle = "Dark"; # dark mode
    AppleShowAllFiles = true;
    ApplePressAndHoldEnabled = false; # enable press and hold

    # If you press and hold certain keyboard keys when in a text area, the key’s character begins to repeat.
    # This is very useful for vim users, they use `hjkl` to move cursor.
    # sets how long it takes before it starts repeating.
    InitialKeyRepeat = 10; # normal minimum is 15 (225 ms), maximum is 120 (1800 ms)
    # sets how fast it repeats once it starts.
    KeyRepeat = 3; # normal minimum is 2 (30 ms), maximum is 120 (1800 ms)
  };
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/davish/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.activation = {
    background = lib.mkIf config.stylix.enable ''
      run ${pkgs.desktoppr}/bin/desktoppr all ${config.stylix.image}
    '';

  };
  home.stateVersion = "24.05"; # Did you read the comment?
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
}
