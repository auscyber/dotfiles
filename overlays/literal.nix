{
  inputs,
  pkgs,
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
    aarch64-darwin = pkgs.ghostty-bin; # inputs.my-nur.packages.aarch64-darwin.ghostty-nightly-bin;
    x86_64-linux = pkgs.ghostty;
    aarch64-linux = pkgs.ghostty;
  };
  pinentry = {
    aarch64-darwin = pkgs.pinentry_mac;
    x86_64-linux = pkgs.pinentry;
    aarch64-linux = pkgs.pinentry;
  };
in
(
  {

    nil = inputs.nil.packages."${system}".default;
    kmonad = inputs.kmonad.packages."${system}".default;
    kanata = inputs.my-nur.packages."${system}".kanata;
    kanata-tray = inputs.my-nur.packages."${system}".kanata-tray;
    lix = pkgs.lix.overrideAttrs (attrs: {
      doCheck = false;
    });
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
    input-leap = pkgs.input-leap.overrideAttrs (attrs: {
      # patches = [ ];
	  inherit (sources.input-leap) src version;
    });

    inherit (inputs.hyprland.packages."${system}") hyprland xdg-desktop-portal-hyprland;
    ivy-fetch = pkgs.callPackage ../packages/ivy-fetch { };
    hln = pkgs.callPackage ../packages/hardlink.nix { };
    pinentry = pinentry."${system}";
    kanata-vk-agent = inputs.my-nur.packages."${system}".kanata-vk-agent;
    #desktoppr = pkgs.callPackage ../packages/desktoppr.nix { };
    inherit (inputs.nixos-conf-editor.packages."${system}") nixos-conf-editor;
    #            nh = inputs.nh.packages."${system}".default;
    inherit (inputs.eww.packages.${system}) eww;
    #    inherit (inputs.rnix.packages."${system}") rnix-lsp;
    ghostty = ghostty."${system}";
    buildEnvExtra =
      lib.extra.overrideDerivation inputs.staging-next.legacyPackages."${system}".buildEnv
        pkgs;
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
    pam_rssh = pkgs.pam_rssh.overrideAttrs (attrs: {
      meta = attrs.meta // {
        platforms = lib.platforms.unix;
      };
    });

  }
  // (lib.optionalAttrs (pkgs.stdenv.isLinux) {

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
