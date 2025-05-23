# home.nix

{ config, pkgs, inputs, ... }:

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

  home.packages = with pkgs;
    [
      nodejs
      vscode
      pandoc
      discord
      spotify
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
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
}
