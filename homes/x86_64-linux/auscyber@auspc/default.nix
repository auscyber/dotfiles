conf@{
  config,
  pkgs,
  system,
  lib,
  ...
}:
let
  impConf = fil: import fil conf;

in
rec {
  stylix = {
    targets.kde.enable = true;
  };
  #  imports = [ ./picom.nix ];
  home.sessionVariables = {
    SSH_AUTH_SOCK = "${config.home.homeDirectory}/.1password/agent.sock";
  };

  programs = {
    ssh = {
      enable = true;
      matchBlocks = {
        #        "*" = {
        #          identityAgent = "~/.1password/agent.sock";
        #        };
      };

    };
    git = {
      enable = true;
      signing = {
        key = "0x64C359636F8BC1D4";
        signByDefault = true;
      };
    };

    home-manager.enable = true;
  };
  wayland.windowManager.hyprland.settings.exec-once = [
    "${pkgs.kdePackages.kwallet-pam}/libexec/pam_kwallet_init"
  ];
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIZAourM3ZASk6AiN8qDD1gm+jW6/FvlXNc3sfNudQtU auscyber@auspc";
  sops.age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
  auscybernix = {
    keybinds.kanata = {
      enable = false;
      extraPackages = with pkgs; [
        hyprland
        ghostty
        rofi
      ];

      config = config.lib.file.getLocalPath ../../../kanata.kbd;
    };
    programs.zotero.enable = true;
    browsers.zen-browser.enable = true;
    programs._1password-cli.enable = true;
    terms.ghostty.enable = true;
    wms.hyprland.enable = false;

    shell = {
      enable = true;
      fish = {
        enable = true;
      };
    };
    programs.neovim.enable = true;

  };
  #  xdg.configFile."nvim/parser/c.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-c}/parser";
  #  xdg.configFile."nvim/parser/lua.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-lua}/parser";
  #  xdg.configFile."nvim/parser/rust.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-rust}/parser";
  #  xdg.configFile."nvim/parser/python.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-python}/parser";
  #  xdg.configFile."nvim/parser/nix.so".source = "${pkgs.tree-sitter.builtGrammars.tree-sitter-nix}/parser";

  # xdg.configFile."nvim/fnl/plugins/sqlite.fnl".text = ''
  # (module plugins.sqlite)
  # (print "hi")
  # (set vim.g.sql_clib_path "${pkgs.sqlite.out}/lib/libsqlite3.so")
  #   '';

  services = {
    dunst.enable = false;
    gpg-agent = {
      enable = true;
      pinentry.program = pkgs.pinentry-qt;
      enableSshSupport = true;
      extraConfig = ''
        allow-loopback-pinentry
      '';
    };
  };
  programs.gpg.enable = true;

  home.packages =
    with pkgs;
    [
      helium
      heroic
      shadps4
      #      element-desktop
      tidal-hifi
      st

      rclone
      #      neovim-nightly
      #      firefox
      tmux
      wineWowPackages.stable
      #      emacs
      pcmanfm
      fzf
      vscode
      openjdk8
      xorg.xmodmap
      xorg.xmessage
      rofi
      arandr
      steam
      #      jetbrains.idea-ultimate
      libnotify
      stack
      xclip
      ripgrep
      discord
      polybarFull
      playerctl
      htop
      #      eclipses.eclipse-java
      starship
      fish
      nitrogen
      maim
      #      teams
      gcc
      dunst
      nixfmt
      lua
      (spotify.overrideAttrs (attrs: {
        nativeInputs = [ gnutls ];
      }))
      unzip
      #scala
      #  starship ardour
      slack
      #  luaPackages.lua-lsp
      #idris2
      #      stdenv.cc.cc.lib
      grub2_efi
      (python3.withPackages (p: with p; [ pynvim ]))
      #      metals
      _1password-cli
      nautilus
      eww
      #wezterm
      zoom-us
      file
      mitscheme
      libreoffice
      thunderbird
    ]
    ++ (with pkgs.lua51Packages; [ luarocks ]);

  home.stateVersion = "21.11";
}
