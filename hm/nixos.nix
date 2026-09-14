conf@{ config
, pkgs
, system
, lib
, ...
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
    vim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [
        vim-airline
        vim-addon-nix
      ];
      settings = {
        ignorecase = true;
      };
      extraConfig = ''
        set mouse=a
      '';
    };
    home-manager.enable = true;
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
      pinentryFlavor = "qt";
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
      (pkgs.gradleGen.override {
          java = jdk8;
        }).gradle_latest
      rclone
      neovim-nightly
      firefox
      tmux
      rust-analyzer
      wineWowPackages.stable
      emacsGcc
      kotlin
      pcmanfm
      fzf
      vscode
      openjdk8
      xorg.xmodmap
      xorg.xmessage
      multimc
      skypeforlinux
      rofi
      arandr
      ccls
      steam
      jetbrains.idea-ultimate
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
      teams
      gcc
      dunst
      procps-ng
      nodejs-14_x
      nixfmt
      lua
      (spotify.overrideAttrs (attrs: {
        nativeInputs = [ gnutls ];
      }))
      unzip
      scala
      rnix-lsp
      #  starship ardour
      slack
      #  luaPackages.lua-lsp
      #idris2
      stdenv.cc.cc.lib
      grub2_efi
      (python3.withPackages (p: with p; [ pynvim ]))
      metals
      _1password
      gnome.gnome-keyring
      gnome.nautilus
      eww
      #wezterm
      zoom-us
      file
      mitscheme
      libreoffice
      pinentry
      thunderbird
    ]
    ++ (with pkgs.lua51Packages; [ luarocks ])
    ++ (with pkgs.haskellPackages; [
      fourmolu
      taffybar
      #my-xmonad
      haskell-language-server
    ])
    ++ [
      (pkgs.haskellPackages.ghcWithPackages (
        pk: with pk; [
          microlens-th
          microlens
          dbus
          xmonad-contrib
          cabal-install
          X11
          xmonad
        ]
      ))
    ]
    ++ (with nodePackages; [
      p3x-onenote
      yarn
      typescript-language-server
      typescript
      purescript-language-server
    ])
    ++ (with ocamlPackages; [
      utop
      dune
      ocaml
      opam
      merlin
    ]);
  #  home.stateVersion = "21.11";
}
