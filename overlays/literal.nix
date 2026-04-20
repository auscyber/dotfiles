{
  inputs,
  pkgs,
  self,
  system,
}:

let
  inherit (pkgs) lib;
  sources = pkgs.callPackage ../_sources/generated.nix { };
  zen-browser = {
    aarch64-darwin = inputs.my-nur.packages.aarch64-darwin.zen-browser;
    x86_64-linux = inputs.zen-browser.packages.x86_64-linux.beta-unwrapped;
    aarch64-linux = inputs.zen-browser.packages.aarch64-linux.beta-unwrapped;
  };

  ghostty = {
    #    aarch64-darwin = pkgs.callPackage ../packages/ghostty/default.nix { source = sources.ghostty; }; # inputs.my-nur.packages.aarch64-darwin.ghostty-nightly-bin;
    aarch64-darwin = pkgs.ghostty-bin;
    x86_64-linux = pkgs.ghostty;
    aarch64-linux = pkgs.ghostty;
  };
  pinentry = {
    aarch64-darwin = pkgs.pinentry_mac;
    x86_64-linux = pkgs.pinentry-curses;
    aarch64-linux = pkgs.pinentry;
  };
  helium = {
    x86_64-linux = pkgs.callPackage ../packages/helium/default.nix {
      source = sources.helium_linux;
    };
    aarch64-darwin = pkgs.callPackage ../packages/helium/mac.nix {
      source = sources.helium_macos;
    };
  };
  pkgsSwift = import inputs.nixpkgs-swift { inherit (pkgs) system; };
  pkgsMaster = import inputs.nixpkgs-master {
    inherit (pkgs) system;
    config.allowUnfree = true;
  };
  pkgsUnstableSmall = import inputs.unstable-small {
    inherit (pkgs) system;
    config.allowUnfree = true;
  };
in
(
  {
    proton-ge-bin = pkgs.proton-ge-bin.overrideAttrs (attrs: {
      inherit (sources.proton-ge-bin) src version;
    });
    sketchybar = pkgs.sketchybar.overrideAttrs (attrs: {
      inherit (sources.sketchybar) src version;
      patches = [ ../packages/sketchybar-pid.patch ];
    });

    sccacheWrapper =
      lib.makeOverridable
        (
          { extraConfig, cc }:
          cc.override {
            cc = pkgs.callPackage ../packages/sccache-links.nix {
              inherit extraConfig;
              unwrappedCC = cc.cc;
            };
          }
        )
        {
          extraConfig = "";
          inherit (pkgs.stdenv) cc;
        };
    sccacheStdenv = lib.lowPrio (
      lib.makeOverridable
        (
          { stdenv, ... }@extraArgs:
          pkgs.addAttrsToDerivation
            {
              env.RUSTC_WRAPPER = "${self.sccacheWrapper}";
            }
            (
              pkgs.overrideCC stdenv (
                self.ccacheWrapper.override (
                  {
                    inherit (stdenv) cc;
                  }
                  // lib.optionalAttrs (builtins.hasAttr "extraConfig" extraArgs) {
                    extraConfig = extraArgs.extraConfig;
                  }
                )
              )
            )
        )
        {
          inherit (pkgs) stdenv;
        }
    );
    kernelStdenv =
      let
        llvmKernelStdenv = pkgs.stdenvAdapters.overrideInStdenv pkgs.llvmPackages.stdenv [
          pkgs.llvm
          pkgs.lld
          pkgs.llvmPackages.clang-unwrapped
        ];
        stdenv = self.ccacheStdenv.override {
          stdenv = llvmKernelStdenv;
        };
      in
      stdenv;

    linuxZenWMuQSS =
      let
        llvmKernelStdenv = pkgs.stdenvAdapters.overrideInStdenv pkgs.llvmPackages.stdenv [
          pkgs.llvm
          pkgs.lld
          #          pkgs.llvmPackages.clang-unwrapped
          pkgs.llvmPackages.clang
        ];
        stdenv = self.ccacheStdenv.override {
          stdenv = llvmKernelStdenv;
        };
      in
      (pkgs.linuxPackagesFor (
        pkgs.linux_zen.override (prev: {
          #          stdenv = llvmKernelStdenv;
          inherit stdenv;
          buildPackages = pkgs.buildPackages // {
            stdenv = stdenv;
          };

          ignoreConfigErrors = true;

          kernelPatches = [
            {
              name = "llvm-lto";
              patch = null;
              structuredExtraConfig = with lib.kernel; {
                # We are not a k8s server
                SCHED_ALT = yes;
                CPU_MITIGATIONS = lib.mkForce no;

                # Clang options require a lot of extra config
                CC_IS_CLANG = lib.mkForce yes;
                LTO = lib.mkForce yes;
                LTO_CLANG = lib.mkForce yes;
                # full LTO is much more expsneive
                LTO_CLANG_THIN = lib.mkForce yes;
              };
              ignoreConfigErrors = true;

            }
          ];
        })
      )).extend
        (
          self: super: {
            alx-wol = self.callPackage ../packages/alx-wol.nix {
            };
          }
        );
    inherit (pkgsSwift) swift swiftPackages;
    helium = helium."${system}";
    nil = inputs.nil.packages."${system}".default;
    attic = pkgs.attic;
    direnv = pkgs.direnv.overrideAttrs (attrs: {
      postPatch = ''
        substituteInPlace GNUmakefile --replace-fail " -linkmode=external" ""
      '';
    });
    attic-server = pkgs.attic-server;
    #    kmonad = inputs.kmonad.packages."${system}".default;
    kanata = inputs.my-nur.packages."${system}".kanata;
    kanata-tray = inputs.my-nur.packages."${system}".kanata-tray;
    #	gnupg-wrapped = pkgs.symlinkJoin {
    #	  name = "gnupg";
    #	  paths = [
    #(pkgs.callPackage ../packages/gpg {  inherit (pkgs) gnupg; })
    #pkgs.gnupg
    #	  ];
    #	};

    lix = pkgs.lix.overrideAttrs (attrs: {
      doCheck = false;
    });
    age-plugin-gpg = inputs.age-plugin-gpg.packages."${system}".default.overrideAttrs (attrs: {
      postInstall = (attrs.postInstall or "") + ''
        	  ln -s $out/bin/age-plugin-gpg $out/bin/age-plugin-gpg-1
        	  '';
    });
    rift = pkgs.callPackage ../packages/rift.nix {

      source = {
        src = ../inputs/rift;
        version = "0.8.3";
        cargoLock = ../inputs/rift/Cargo.lock;
        pname = "rift";
      };

    };
    #.overrideAttrs
    # (attrs: {
    #   version = "5.0.0";
    #   src = pkgs.fetchurl {
    #     url = "https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice/releases/download/v5.0.0/Karabiner-DriverKit-VirtualHIDDevice-5.0.0.pkg";
    #     hash = "sha256-hKi2gmIdtjl/ZaS7RPpkpSjb+7eT0259sbUUbrn5mMc=";
    #   };
    # })

    yabai = inputs.my-nur.packages."${system}".yabai;
    jankyborders = pkgs.jankyborders.overrideAttrs (attrs: {
      inherit (sources.jankyborders) src version;
    });
    input-leap = pkgs.input-leap;

    ivy-fetch = pkgs.callPackage ../packages/ivy-fetch { };
    hln = pkgs.callPackage ../packages/hardlink.nix { };
    pinentry = pinentry."${system}";
    kanata-vk-agent = inputs.my-nur.packages."${system}".kanata-vk-agent;
    #desktoppr = pkgs.callPackage ../packages/desktoppr.nix { };
    inherit (inputs.nixos-conf-editor.packages."${system}") nixos-conf-editor;
    #            nh = inputs.nh.packages."${system}".default;
    #    inherit (inputs.rnix.packages."${system}") rnix-lsp;
    ghostty = ghostty."${system}";
    slskd = pkgs.callPackage ../packages/slskd.nix {
      source = {
        inherit (sources.slskd) src;
        version = "0.24.2";
        npmHash = sources.slskd.extract."src/web/package-lock.json";
      };
    };
    zen-browser = zen-browser."${system}";
    picom = pkgs.picom.overrideAttrs (attrs: {
      src = inputs.picom;
    });
    mopidy-tidal = pkgs.mopidy-tidal.overrideAttrs (_: {
      doCheck = false;
    });
    game-devices-udev-rules = pkgs.game-devices-udev-rules.overrideAttrs (attrs: {

      inherit (sources.game-devices-udev-rules) src;
    });
    zotero-extensions = {
      zotero-better-bibtex = pkgs.fetchFirefoxAddon {
        name = "zotero-better-bibtex";
        url = "https://github.com/retorquere/zotero-better-bibtex/releases/download/v7.0.59/zotero-better-bibtex-7.0.59.xpi";
        hash = "sha256-4p6Twni+kGKQPjmFmjCHJMypu8WSGweTiZINfPjy9i8=";
      };
      zotero-attanger = pkgs.fetchFirefoxAddon {
        name = "zotero-attanger";
        url = "https://github.com/MuiseDestiny/zotero-attanger/releases/download/1.3.9/zotero-attanger.xpi";
        hash = "sha256-C8YC473o1gthq5gpi5FEdbIcTX4MsA7hGcC1oLyJotw=";
      };
    };

  }
  // lib.optionalAttrs (pkgs.stdenv.isDarwin) {
    #    gtk3 = pkgs.gtk3.overrideAttrs (attrs: {
    #      patches = attrs.patches ++ [
    #        (pkgs.fetchpatch {
    #          url = "https://raw.githubusercontent.com/NixOS/nixpkgs/refs/heads/staging/pkgs/development/libraries/gtk/patches/3.0-mr5531-backport.patch";
    #          hash = "sha256-vP0xmeKQazr93bTV+2kIwsNA+rZPmNd9iaUfpYOpD0M=";
    #        })
    #      ];
    #    });

    pam_rssh = pkgs.pam_rssh.overrideAttrs (attrs: {
      meta = attrs.meta // {
        platforms = lib.platforms.unix;
      };
    });

  }
  // (lib.optionalAttrs (pkgs.stdenv.isLinux) {
    heroic = (
      pkgs.heroic.override {
        extraPkgs = pkgs: [
          pkgs.gamescope
        ];
      }
    );

    deadlock-mod-manager = inputs.deadlock.packages."${system}".default.overrideAttrs (attrs: {
      preFixup = ''
        gappsWrapperArgs+=(
          --set FONTCONFIG_FILE "${pkgs.fontconfig.out}/etc/fonts/fonts.conf"
          --set TAURI_DIST_DIR "$out/share/deadlock-modmanager/dist"
          --set DISABLE_UPDATE_DESKTOP_DATABASE 1
          --prefix PATH : ${lib.makeBinPath [ pkgs.desktop-file-utils ]}
          --add-flags "--disable-auto-update"
        )
      '';
      env.VITE_API_URL = "https://api.deadlockmods.app";
      buildInputs = attrs.buildInputs ++ [
        pkgs.copyDesktopItems
        pkgs.desktop-file-utils
        pkgs.wrapGAppsHook3
      ];

      desktopItems = [
        (pkgs.makeDesktopItem {
          desktopName = "deadlock-mod-manager";
          name = "Deadlock Mod Manager";
          exec = "deadlock-mod-manager %u";
          terminal = false;
          type = "Application";
          icon = "deadlock-mod-manager";
          mimeTypes = [ "x-scheme-handler/deadlock-mod-manager" ];
          categories = [
            "Utility"
            "Game"
          ];
        })
      ];

    });
    inherit (inputs.eww.packages.${system}) eww;

    inherit (inputs.hyprland.packages."${system}") hyprland xdg-desktop-portal-hyprland;

    #    ipython = pkgs.ipython.overrideAttrs (attrs: {
    #
    #      doCheck = false;
    #    });
    #
    #    libsecret = pkgs.libsecret.overrideAttrs (attrs: {
    #      doCheck = false;
    #    });

  })
)
