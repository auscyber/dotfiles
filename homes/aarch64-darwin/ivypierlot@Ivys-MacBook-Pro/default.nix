# home.nix

{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:

{

  sops.age.keyFile = "/users/ivypierlot/Library/Application Support/sops/age/keys.txt";
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
    wms.yabai.enable = true;
    programs.sketchybar.enable = true;
    keybinds.kanata = {
      enable = true;
      extraPackages = [
        pkgs.jq
        pkgs.yabai
      ];
      extraCommandPiping = ../../../modules/home/keybinds/kanata/config.toml;
      tray = {
        configFile = builtins.toString ./tray_config.toml;
      };
      appBundleIds = [
        "app.zen-browser.zen"
        "com.1password.1password"
        "com.1password.1password-launcher"
      ];
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
    gnupg
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

  programs.ssh = {
    enable = true;
    package = pkgs.openssh;
    matchBlocks = {
      "109.123.227.80" = {
        host = "109.123.227.80";
        forwardAgent = true;
        identitiesOnly = true;
        identityFile = "~/.ssh/id_ed25519.pub";
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
