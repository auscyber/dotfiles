# home.nix

{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:

{

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
  auscybernix = {
    programs.sketchybar.enable = true;
    keybinds.kanata = {
      enable = true;
      extraPackages = [
        pkgs.jq
        pkgs.yabai
      ];
      appBundleIds = [
        "app.zen-browser.zen"
        "com.1password.1password"
        "com.1password.1password-launcher"
      ];
      config = config.lib.file.getLocalPath ../../../kanata.kbd;
    };
    keybinds.skhd = {
      enable = false;

      binds =
        (lib.attrsets.mapAttrs'
          (name: value: {
            name = "alt - ${name}";
            inherit value;
          })
          (
            lib.genAttrs (builtins.genList (x: "${builtins.toString (x + 1)}") 9) (
              name: "yabai -m space --focus ${name}"
            )
          )
        )
        // (lib.attrsets.mapAttrs'
          (name: value: {
            name = "shift + alt - ${name}";
            inherit value;
          })
          (
            lib.genAttrs (builtins.genList (x: "${builtins.toString (x + 1)}") 9) (
              name: "yabai -m window --space ${name}"
            )
          )
        )
        // {
          #          "cmd + alt - s " = "yabai -m window --toggle spotify || open  -a Spotify ";
          "cmd + alt - d " = " yabai -m window --toggle discord || open -a Discord";
          # focus window
          "alt - h" = " yabai -m window --focus west";
          "cmd + shift - 0x24" = "open -na Ghostty";

          # swap managed window
          "shift + alt - h " = "yabai -m window --swap north";

          # move managed window
          "shift + cmd - h " = "yabai -m window --warp east";

          # balance size of windows
          "shift + alt - 0 " = "yabai -m space --balance";

          # make floating window fill screen
          "shift + alt - up     " = "yabai -m window --grid 1:1:0:0:1:1";

          # make floating window fill left-half of screen
          "shift + alt - left   " = "yabai -m window --grid 1:2:0:0:1:1";

          # create desktop, move window and follow focus - uses jq for parsing json (brew install jq)
          "shift + cmd - n" = ''
            yabai -m space --create && \
                                     index="$(yabai -m query --spaces --display | jq 'map(select(."is-native-fullscreen" == false))[-1].index')" && \
                                     yabai -m window --space "''${index}" && \
                                     yabai -m space --focus "''${index}"
            						 '';

          # fast focus desktop
          "cmd + alt - x " = " yabai -m space --focus recent";

          # send window to desktop and follow focus
          # shift + cmd - z : yabai -m window --space next; yabai -m space --focus next
          # shift + cmd - 2 : yabai -m window --space  2; yabai -m space --focus 2

          # focus monitor
          "ctrl + alt - z  " = " yabai -m display --focus prev";

          # send window to monitor and follow focus
          # ctrl + cmd - c  : yabai -m window --display next; yabai -m display --focus next
          # ctrl + cmd - 1  : yabai -m window --display 1; yabai -m display --focus 1

          # move floating window
          "shift + ctrl - a " = " yabai -m window --move rel:-20:0";
          "shift + ctrl - s " = " yabai -m window --move rel:0:20";

          # increase window size
          "shift + alt - a " = " yabai -m window --resize left:-20:0";
          "shift + alt - w " = " yabai -m window --resize top:0:-20";

          # decrease window size
          "shift + cmd - s " = " yabai -m window --resize bottom:0:-20";
          "shift + cmd - w " = " yabai -m window --resize top:0:20";

          # set insertion point in focused container
          "ctrl + alt - h " = " yabai -m window --insert west";

          # toggle window zoom
          "alt - d " = " yabai -m window --toggle zoom-parent";
          "alt - f " = " yabai -m window --toggle zoom-fullscreen";

          # toggle window split type
          "alt - e " = " yabai -m window --toggle split";

          # float / unfloat window and center on screen
          "alt - t " = " yabai -m window --toggle float --grid 4:4:1:1:2:2";

          # toggle sticky(+float), picture-in-picture
          "alt - p " = " yabai -m window --toggle sticky --toggle pip";
        };

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
    gnupg
    prismlauncher
    virt-manager
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

  # You can also manage environment variables but you will have to manually
  services.espanso = {
    enable = true;
    matches = {
      french = {
        matches = [

          {
            trigger = "e'";
            replace = "é";
          }

          {
            trigger = "e`";
            replace = "è";
          }

          {
            trigger = "e^";
            replace = "ê";
          }

          {
            trigger = "e:";
            replace = "ë";
          }

          {
            trigger = "o^";
            replace = "ô";
          }

          {
            trigger = "a`";
            replace = "à";
          }

          {
            trigger = "a^";
            replace = "â";
          }

          {
            trigger = "i^";
            replace = "î";
          }

          {
            trigger = "i:";
            replace = "ï";
          }

          {
            trigger = "u`";
            replace = "ù";
          }

          {
            trigger = "u^";
            replace = "û";
          }

          {
            trigger = "u:";
            replace = "ü";
          }

          {
            trigger = "oe";
            replace = "œ";
          }

          {
            trigger = "ae";
            replace = "æ";
          }

          {
            trigger = "y:";
            replace = "ÿ";
          }

          {
            trigger = "c,";
            replace = "ç";
          }

          {
            trigger = "E'";
            replace = "É";
          }

          {
            trigger = "E`";
            replace = "È";
          }

          {
            trigger = "E^";
            replace = "Ê";
          }

          {
            trigger = "E:";
            replace = "Ë";
          }

          {
            trigger = "O^";
            replace = "Ô";
          }

          {
            trigger = "A`";
            replace = "À";
          }

          {
            trigger = "A^";
            replace = "Â";
          }

          {
            trigger = "I^";
            replace = "Î";
          }

          {
            trigger = "I:";
            replace = "Ï";
          }

          {
            trigger = "U`";
            replace = "Ù";
          }

          {
            trigger = "U^";
            replace = "Û";
          }

          {
            trigger = "U:";
            replace = "Ü";
          }

          {
            trigger = "OE";
            replace = "Œ";
          }

          {
            trigger = "AE";
            replace = "Æ";
          }

          {
            trigger = "Y:";
            replace = "Ÿ";
          }

          {
            trigger = "C,";
            replace = "Ç";
          }
        ];
      };
    };
  };

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
