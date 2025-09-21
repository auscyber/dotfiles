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
  programs = {
    git = {
      enable = true;
      signing = {
        key = "0xC3F28549AD3115E8";
        signByDefault = true;
      };
    };

    home-manager.enable = true;
  };
  auscybernix = {
    keybinds.kanata = {
      enable = true;
      extraPackages = with pkgs; [
        hyprland
        ghostty
        wofi
      ];

      config = builtins.toString ../../../kanata.kbd;
    };
    programs.zotero.enable = true;
    browsers.zen-browser.enable = true;
    programs._1password-cli.enable = true;
    terms.ghostty.enable = true;
    wms.hyprland.enable = true;
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
      pinentryPackage = pkgs.pinentry-qt;
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
      st

      rclone
      #      neovim-nightly
      #      firefox
      tmux
      wineWowPackages.stable
      emacsGcc
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
      eclipses.eclipse-java
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
      _1password
      gnome-keyring
      nautilus
      eww
      #wezterm
      zoom-us
      file
      mitscheme
      libreoffice
      pinentry
      thunderbird
    ]
    ++ (with pkgs.lua51Packages; [ luarocks ]);

  home.stateVersion = "21.11";
}
